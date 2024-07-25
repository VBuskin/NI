
# Obtaining frequency and related information on (non-)NI verbs -----------

# Load scripts
source("NI_Loading.R")


# Variable data

## Frequencies
NI_data_variable %>% 
  dplyr::count(lemma) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x = n)) +
  geom_histogram()

## Dispersion

### Normalised entropy (Gries 2021: 95-6)

tab1 <- table(NI_data_variable$lemma, NI_data_variable$Text_category)

# Example: hear

NI_data_variable %>% 
  dplyr::filter(lemma == "hear") -> NI_hear

# Distribution across text categories
tab1 <- table(NI_hear$Text_category)
  
prop_tab1 <- prop.table(tab1)

prop_tab1 <- prop_tab1[prop_tab1 > 0]

-sum(prop_tab1 * log2(prop_tab1)) / log2(length(levels(NI_hear$Text_category)))

# Example: begin

NI_data_variable %>% 
  dplyr::filter(lemma == "begin") -> NI_begin

tab1 <- table(NI_begin$Text_category)

prop_tab1 <- prop.table(tab1)

prop_tab1 <- prop_tab1[prop_tab1 > 0]

-sum(prop_tab1 * log2(prop_tab1)) / log2(length(levels(NI_hear$Text_category)))


# Invariable data

## Frequencies
NI_data_invariable %>% 
  dplyr::count(lemma) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x = n)) +
    geom_histogram()

# Example: excuse

NI_data_invariable %>% 
  dplyr::filter(lemma == "excuse") -> NI_excuse

tab1 <- table(NI_excuse$lemma, NI_excuse$Text_category)

prop_tab1 <- prop.table(tab1)

prop_tab1 <- prop_tab1[prop_tab1 > 0]

-sum(prop_tab1 * log2(prop_tab1)) / log2(length(levels(NI_hear$Text_category)))



# Function for normalised entropy -----------------------------------------

norm_entropy <- function(df) {
  
  # Requires a "lemma" column and a "Text_category" column
  
  # Initialise list
  
  dispersion_df <- list()
  
  for (i in unique(df$lemma)) {
    
    # Subset
    lem_sub <- df %>% dplyr::filter(lemma == i)
      
    # Get contingency table
    lem_tab <- table(lem_sub$Text_category)
    
    # Get frequency
    lem_freq <- lem_sub %>% count(lemma) %>% pull(n)
    
    # Get prop table
    lem_prop <- prop.table(lem_tab)
    
    # Get non-zero cases
    lem_prop <- lem_prop[lem_prop > 0]
    
    # Make sure Text_category is a factor
    lem_sub$Text_category <- as.factor(lem_sub$Text_category)
    
    # Calculate entropy
    h_norm <- -sum(lem_prop * log2(lem_prop)) / log2(length(levels(lem_sub$Text_category)))
    
    # Store results in tibble
    output_df <- tibble(lemma = i, 
                        frequency = lem_freq, 
                        dispersion = h_norm) 
    
    # Store result of each iteration
    dispersion_df[[i]] <- output_df
    
  }
  
  # Combine list elements to df
  dispersion_df_full <- bind_rows(dispersion_df)
  
  # End
  return(dispersion_df_full)
  
}

# Test

NI_data_variable %>% 
  dplyr::filter(lemma == "hear" | lemma == "accept" | lemma == "eat") -> data_test


norm_entropy(data_test)

df <- data_test

# Full application

NI_var_disp <- norm_entropy(NI_data_variable)

# Visualisation
library("plotly")

# Creating a plotly 3D scatter plot
plot_ly(NI_var_disp, x = ~lemma, y = ~frequency, z = ~dispersion, type = 'scatter3d', mode = 'markers') %>%
  layout(scene = list(xaxis = list(title = 'Lemma'),
                      yaxis = list(title = 'Frequency'),
                      zaxis = list(title = 'Dispersion')))


# For invariant verbs -----------------------------------------------------

NI_data_invariable %>% 
  dplyr::filter(lemma == "excuse") -> data_test2

df <- data_test2

norm_entropy(data_test2)

NI_invar_disp <- norm_entropy(NI_data_invariable)


# Combine NI + non-NI verbs -----------------------------------------------


NI_full_disp <- norm_entropy(NI_data)

# Now add a column that indicates whether a verb is variable or not

NI_full_disp %>% 
  dplyr::mutate(variable = ifelse(lemma %in% find_variable_lemmas(NI_data), "variable", "invariant")) -> NI_freq_disp_df

# Visualisation

## Let's replace all NaN values with 0
## Verbs that occur only once can be argued to have minimal dispersion

NI_freq_disp_df[NI_freq_disp_df$dispersion == "NaN",]$dispersion <- 0


# Creating a plotly 3D scatter plot
plot_ly(NI_freq_disp_df, x = ~lemma, y = ~frequency, z = ~dispersion, color = ~variable, type = 'scatter3d', mode = 'markers') %>%
  layout(scene = list(xaxis = list(title = 'Lemma'),
                      yaxis = list(title = 'Frequency'),
                      zaxis = list(title = 'Dispersion')))

# Wow ... all the invariant cases are grouped in the same corner



# Combine all transitive verbs with NI verbs ------------------------------

trans_verbs <- as_tibble(read.csv("R_data/trans_verbs.csv", header = TRUE, sep = ","))

## Remove irrelevant columns
trans_verbs %>% 
  dplyr::select(-X, -X.1, -X.2) -> trans_verbs

## Extract the other categories (Verb-type, Verb-group)
trans_verbs %>%
  extract(Levin_classes, into = c("Verb_type", "Verb_group"),
          regex = "^(.*?)\\((.*?)\\)", remove = FALSE) %>% 
  dplyr::select(-Levin_classes) -> verb_classes_df


# Use confirmed variable lemmas

library("Hmisc")

NI_Verbs_confirmed <- as.character(unique(NI_data_variable$lemma)) # 121

NI_Verbs_confirmed[NI_Verbs_confirmed %in% verb_classes_df$verb] # 64

# Verbs that are missing from the transitive verbs list

missing_verbs <- NI_Verbs_confirmed[NI_Verbs_confirmed %nin% verb_classes_df$verb] # 64

# Create a data frame for the missing verbs with the default value for 'variable'
missing_verbs_df <- tibble(
  verb = missing_verbs,
  variable = "variable"  # Assign the default value
)

# Add column for variability
verb_classes_var_df <- verb_classes_df %>% 
  dplyr::select(verb) %>% 
  dplyr::mutate(variable = ifelse(verb %in% NI_Verbs_confirmed, "variable", "invariant"))

# Combine dfs

combined_df <- bind_rows(verb_classes_var_df, missing_verbs_df)


# Compute frequencies and dispersion --------------------------------------

source("NI_Corpora.R")

# Read in lemmatised corpus

ICE_GB_dirty <- lemmatise_ICE(transcripts_collapsed_GB)


# Query (lemmatised?) corpus
## Test with random subset

kwic_comb <- kwic_ICE(ICE_GB_dirty, combined_df$verb) 

# Get register column

kwic_comb <- separate_wider_delim(kwic_comb, cols = docname, delim = "-", names = c("Text_category", "File_number"))

# Extract sentence

kwic_comb_parsed <- kwic_comb

# Collapse pre, keyword and post

kwic_comb_parsed$text <- paste(kwic_comb_parsed$pre, kwic_comb_parsed$keyword, kwic_comb_parsed$post)



# Extraction function

extract_sentences <- function(df) {
  # Set empty list environment
  input <- list()
  # Loop
  for (i in 1:nrow(df)) {
    # Use the sentence extraction from above
    cleaned_sent <- extract_target_sentence(df[i,]$text, df[i,]$keyword)
    # Create tibble with lemma, keyword and cleaned text
    input_df <- tibble(
      File_number = df[i,]$File_number,
      Text_category = df[i,]$Text_category,
      keyword = df[i,]$keyword,
      text_cleaned = cleaned_sent
    )
    # Store results of each iteration
    input[[i]] <- input_df
  }
  # Convert list of results into df
  full_input_df <- dplyr::bind_rows(input)
  # End
  return(full_input_df)
}

kwic_extracted <- extract_sentences(kwic_comb_parsed)

#write_xlsx(kwic_extracted, "R_data/kwic_comb_extracted_25-07-2024.xlsx")


# Now I need to POS tag text_cleaned and extract the POS of the keyword; then filter for all verbal uses

m_eng <- udpipe_load_model(file = here::here("/Users/vladimirbuskin", "english-ewt-ud-2.5-191206.udpipe"))


# Don't use this function

POS_tag <- function(df) {
  
  # Define data (= extracted sentence)
  data_df <- df$text_cleaned
  
  # Create list environment
  pos_tagged_df <- list()
  
  for (i in 1:length(data_df)) {
  
  # Annotate the extracted sentences
  text_anndf <- udpipe::udpipe_annotate(m_eng, x = data_df) %>%
    as.data.frame() %>%
    dplyr::select(-sentence)
  
  # Find the token corresponding to the keyword in the extracted sentence
  
  # Store word
  word_pos <- tibble(
    ID = i,
    token = text_anndf$token[i],
    upos = text_anndf[text_anndf$token == kwic_extracted[i,]$keyword,]$upos,
    xpos = text_anndf[text_anndf$token == kwic_extracted[i,]$keyword,]$xpos
  )
  
  # Store result of each iteration
  pos_tagged_df[[i]] <- word_pos
  
  }
  
  # Combine the results
  tagged_df_full <- tibble(bind_rows(pos_tagged_df))
  
  # End
  return(tagged_df_full)
  
}


# Alternative solution (use this one) ----------------------------------------------------


lemma_features <- function(df) {
  
  # Function requires df with a "lemma" and a "text_category" column

  # Define list to store results
  verb_feats <- list()
  
  # Loop through each row of NI_data_parsed
  for (i in 1:nrow(df)) {
    
    # (2) Extract the text
    text <- df[i,]$text_cleaned
    
    # (2) Extract the text category
    text_category <- df[i,]$Text_category
    
    # (2) Extract the File_number
    text_file_number <- df[i,]$File_number
    
    # (2) Extract the target_word
    target_word <- df[i,]$keyword
    
    # Extract the target sentence
    #extracted_sentence <- extract_target_sentence(text, target_word)
    
    # Perform dependency parsing if the sentence is not NULL
    if (!is.null(text)) {
      
      # POS-taggings
      text_anndf <- udpipe::udpipe_annotate(m_eng, x = text) %>%
        as.data.frame() #%>%
      #dplyr::select(-sentence)
      
      # Find the index of the first occurrence of the target word
      first_occurrence_index <- which(text_anndf$token == target_word)[1]
      
      # Store lemma and features for the first occurrence
      if (!is.na(first_occurrence_index)) {
        lemma_features <- tibble(
          ID = i,
          text_category = text_category,
          text_file_number = text_file_number,
          lemma = text_anndf$lemma[first_occurrence_index],
          POS = text_anndf$upos[first_occurrence_index],
          features = text_anndf$feats[first_occurrence_index],
          sentence = text_anndf$sentence[first_occurrence_index]
        )
        verb_feats[[i]] <- lemma_features
      }
    }
  }
  
  # Combine the results
  data_feats_df <- bind_rows(verb_feats)
  
  # End
  return(data_feats_df)
  
}


lemma_features_full_df <- lemma_features(kwic_extracted)


# Next: perform entropy calculations for the sample

norm_entropy2 <- function(df) {
  
  # Requires a "lemma" column and a "Text_category" column
  
  # Initialise list
  
  dispersion_df <- list()
  
  df <- NI_data_features
  
  for (i in unique(df$lemma)) {
    
    # Subset
    lem_sub <- df %>% dplyr::filter(lemma == i)
    
    # Get contingency table
    lem_tab <- table(lem_sub$text_category)
    
    # Get frequency
    lem_freq <- lem_sub %>% count(lemma) %>% pull(n)
    
    # Get prop table
    lem_prop <- prop.table(lem_tab)
    
    # Get non-zero cases
    lem_prop <- lem_prop[lem_prop > 0]
    
    # Make sure Text_category is a factor
    lem_sub$text_category <- as.factor(lem_sub$text_category)
    
    # Calculate entropy
    h_norm <- -sum(lem_prop * log2(lem_prop)) / log2(length(levels(lem_sub$text_category)))
    
    # Store results in tibble
    output_df <- tibble(lemma = i, 
                        frequency = lem_freq, 
                        dispersion = h_norm) 
    
    # Store result of each iteration
    dispersion_df[[i]] <- output_df
    
  }
  
  # Combine list elements to df
  dispersion_df_full <- bind_rows(dispersion_df)
  
  # Redefine missing values
  dispersion_df_full[dispersion_df_full$dispersion == "NaN",]$dispersion <- 0
  
  # End
  return(dispersion_df_full)
  
}

# Test

lemma_disp_output <- norm_entropy2(lemma_features_full_df) 

# "Values close to 0 mean that the distribution is very uneven such that most observations are concentrated in very few levels or even just one whereas values close to 1 mean that the distribution is very even/ uniform" (Gries 2021: 96).


# Full application --------------------------------------------------------




