
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

head(kwic_extracted)

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

lemma_features2 <- function(df) {
  # Define list to store results
  verb_feats <- list()
  
  # Initialize progress bar with ETA
  pb <- progress_bar$new(
    format = "[:bar] :current/:total (:percent) ETA: :eta",
    total = nrow(df),
    clear = FALSE,
    width = 60
  )
  
  # Loop through each row of the data frame
  for (i in 1:nrow(df)) {
    # Update progress bar
    pb$tick()
    
    # Extract relevant information
    text <- df$text_cleaned[i]
    text_category <- df$Text_category[i]
    text_file_number <- df$File_number[i]
    target_word <- df$keyword[i]
    
    # Initialize variables with NA in case of failure or empty text
    lemma <- NA
    POS <- NA
    features <- NA
    sentence <- NA
    
    # Check if text is not NA, not empty, and not just whitespace
    if (!is.na(text) && trimws(text) != "") {
      # Try annotating the text
      tryCatch({
        text_anndf <- udpipe::udpipe_annotate(m_eng, x = text) %>%
          as.data.frame()
        
        # Check if annotation returned any tokens
        if (nrow(text_anndf) > 0) {
          first_occurrence_index <- which(text_anndf$token == target_word)[1]
          
          if (!is.na(first_occurrence_index)) {
            lemma <- text_anndf$lemma[first_occurrence_index]
            POS <- text_anndf$upos[first_occurrence_index]
            features <- text_anndf$feats[first_occurrence_index]
            sentence <- text_anndf$sentence[first_occurrence_index]
          }
        }
      }, error = function(e) {
        warning(paste("Error processing text in row", i, ":", e$message))
      })
    }
    
    # Store results, even if they're NA
    lemma_features <- tibble(
      ID = i,
      text_category = text_category,
      text_file_number = text_file_number,
      lemma = lemma,
      POS = POS,
      features = features,
      sentence = sentence
    )
    verb_feats[[i]] <- lemma_features
  }
  
  # Combine the results
  data_feats_df <- bind_rows(verb_feats)
  
  # Return the result
  return(data_feats_df)
}

library("progress")
#library("parallel")
#library("pbapply")

system.time(lemma_features_full_df <- lemma_features2(kwic_extracted))

#write_xlsx(lemma_features_full_df, "R_data/lemma_features_full_df_25-07-2024.xlsx")

head(lemma_features_full_df, n = 200)

lemma_features_full_df %>% 
  dplyr::filter(POS == "VERB") -> lemma_pos_full_df

lemma_pos_full_df %>% 
  count(lemma) %>% 
  View() # Output looks good

i <- "increase"

# Next: perform entropy calculations for the sample

compute_dispersion <- function(df) {

  # Requires a "lemma" column and a "Text_category" column
  
  # Initialise list
  
  dispersion_df <- list()
  
  # Initialize progress bar with ETA
  pb <- progress_bar$new(
    format = "[:bar] :current/:total (:percent) ETA: :eta",
    total = nrow(df),
    clear = FALSE,
    width = 60
  )
  
  # Proportions of corpus parts
  corpus_parts <- c(100/500, 80/500, 70/500, 50/500, 20/500, 30/500, 40/500, 40/500, 20/500, 20/500, 10/500, 20/500)
  
  
  for (i in unique(df$lemma)) {
    
    # Update progress bar
    pb$tick()
    
    # Subset
    lem_sub <- df %>% dplyr::filter(lemma == i)
    
    # Get frequency in different corpus parts
    lem_tab <- table(lem_sub$text_category)
    
    # Get frequency in the entire corpus
    lem_freq <- lem_sub %>% count(lemma) %>% pull(n)
    
    # Get proportions in the corpus
    lem_prop <- prop.table(lem_tab)
    
    # Get non-zero cases
    lem_prop <- lem_prop[lem_prop > 0]
    
    # Make sure Text_category is a factor
    lem_sub$text_category <- as.factor(lem_sub$text_category)
    
    # Match corpus_parts length to the unique categories in lem_tab
    matching_indices <- match(names(lem_tab), unique(df$text_category))
    corpus_parts_adj <- corpus_parts[matching_indices]
    
    # Calculate DKL
    DKL <- sum(lem_tab / lem_freq * log2(lem_tab / lem_freq * 1 / corpus_parts_adj))
    
    # Calculate H_norm
    h_norm <- -sum(lem_prop * log2(lem_prop)) / log2(length(levels(lem_sub$text_category)))
    
    # Store results in tibble
    output_df <- tibble(lemma = i, 
                        frequency = lem_freq, 
                        Hnorm = h_norm,
                        DKL = DKL) 
    
    # Store result of each iteration
    dispersion_df[[i]] <- output_df
    
  }
  
  # Combine list elements to df
  dispersion_df_full <- bind_rows(dispersion_df)
  
  # Redefine missing values
  dispersion_df_full[dispersion_df_full$Hnorm == "NaN",]$Hnorm <- 0
  
  # End
  return(dispersion_df_full)
  
}

# Test

lemma_disp_output <- norm_entropy2(lemma_pos_full_df)

lemma_disp_output <- compute_dispersion(lemma_pos_full_df)

# Done
lemma_disp_output

# "Values close to 0 mean that the distribution is very uneven such that most observations are concentrated in very few levels or even just one whereas values close to 1 mean that the distribution is very even/ uniform" (Gries 2021: 96).

# Add variant/invariant column

lemma_disp_output %>% 
  dplyr::mutate(variable = ifelse(lemma %in% find_variable_lemmas(NI_data), "variable", "invariant")) -> lemma_disp_output_full

# Add log frequencies

lemma_disp_output_full %>% 
  mutate(log_frequency = log(frequency)) -> lemma_disp_output_full

# Plot

# Visualisation
library("plotly")

# Creating a plotly 3D scatter plot
plot_ly(lemma_disp_output_full, x = ~lemma, y = ~log_frequency, z = ~dispersion, color = ~variable, type = 'scatter3d', mode = 'markers') %>%
  layout(scene = list(xaxis = list(title = 'Lemma'),
                      yaxis = list(title = 'Frequency'),
                      zaxis = list(title = 'Dispersion')))


# Default plot

lemma_disp_output_full %>% 
  ggplot(aes(x = lemma, y = log_frequency, col = variable)) +
  geom_point() 


lemma_disp_output_full %>% 
  group_by(variable) %>% 
  summarise(
    mean = mean(frequency),
    n = length(variable),
    sd = sd(frequency),
    mean_disp = mean(dispersion),
    sd_disp = sd(dispersion),
    mean_log = mean(log_frequency),
    sd_mean_log = sd(log_frequency)
  ) -> lemma_stats


length(lemma_disp_output_full$variable)


# Store

saveRDS(lemma_disp_output_full, "R_data/lemma_disp_output_full.RDS")


# Negative binomial models (for computing rates)

# t-test

t.test(lemma_disp_output_full$frequency ~ lemma_disp_output_full$variable, var.equal = FALSE) # significant

t.test(lemma_disp_output_full$Hnorm ~ lemma_disp_output_full$variable, var.equal = FALSE) # singificant

t.test(lemma_disp_output_full$DKL ~ lemma_disp_output_full$variable, var.equal = FALSE) # singificant


effsize::cohen.d(lemma_disp_output_full$frequency, lemma_disp_output_full$variable) # low eff size

effsize::cohen.d(lemma_disp_output_full$Hnorm, lemma_disp_output_full$variable) # high eff size

effsize::cohen.d(lemma_disp_output_full$DKL, lemma_disp_output_full$variable) # high eff size



# Model

library("rms")

lemma_disp_output_full$variable <- as.factor(lemma_disp_output_full$variable)

lemma_disp_output_full$variable <- relevel(lemma_disp_output_full$variable, ref = "invariant")

mod1 <- glm(variable ~ frequency + dispersion, data = lemma_disp_output_full, family = "binomial")

mod2.glm <- glm(variable ~ log_frequency + Hnorm, data = lemma_disp_output_full, family = "binomial")

mod2 <- lrm(variable ~ log_frequency + Hnorm, data = lemma_disp_output_full)

mod3.lrm <- lrm(variable ~ log_frequency + Hnorm + DKL + concreteness, data = lemma_disp_output_full)

drop1(mod3.glm, test = "Chisq")

anova_result <- anova(mod3, mod2, test = "Chisq")

# Frequenzeffekt wäre kognitiv plausibel!

library("effects")

plot(Effect("Hnorm", mod = mod2.glm))

plot(Effect("log_frequency", mod = mod2.glm))


# Concreteness ------------------------------------------------------------

# Perform concreteness annotation

lemma_disp_output_full


concreteness_annotation <- function(data) {
  
  c_ratings <- read.table("../Null Instantiation/Concreteness_ratings.csv", sep = "\t", header = TRUE)
  
  # Create empty list environment
  lemma_rating <- list()
  
  # Start loop
  for (i in unique(data$lemma)) {
    
    # Extract rating for a lemma i
    rating <- c_ratings[c_ratings$Word == i,]$Conc.M
    
    # Extract sd for a lemma i
    sd <- c_ratings[c_ratings$Word == i,]$Conc.SD
    
    # Add results to a tibble
    tibble(lemma = i,
           concreteness = rating,
           dispersion = sd
    ) %>% 
      mutate(cv = sd / rating) -> output_df
    
    # Store result of each iteration
    lemma_rating[[i]] <- output_df
    
  }
  
  # Combine the results
  c_ratings <- bind_rows(lemma_rating)
  
  # Finalise df
  data |> 
    mutate(concreteness = c_ratings[match(data$lemma, c_ratings$lemma),]$concreteness,
           conc_sd = c_ratings[match(data$lemma, c_ratings$lemma),]$dispersion,
           conc_cv = c_ratings[match(data$lemma, c_ratings$lemma),]$cv) -> verbs_concreteness_df
  
  # End
  return(verbs_concreteness_df)
  
}

lemma_conc_df <- concreteness_annotation(lemma_disp_output_full)

# Remove NAs

lemma_conc_df %>% drop_na() -> lemma_conc_df_clean

# Perform tests

t.test(lemma_conc_df_clean$concreteness ~ lemma_conc_df_clean$variable, var.equal = FALSE) # significant -- concreteness is useful!

effsize::cohen.d(lemma_conc_df_clean$concreteness, lemma_conc_df_clean$variable) # low eff size


# Update model

mod3.glm <- glm(variable ~ log_frequency + dispersion + concreteness, data = lemma_conc_df, family = "binomial")

mod3.lrm <- lrm(variable ~ log_frequency + DKL + concreteness, data = lemma_conc_df)

cor(lemma_conc_df$log_frequency, lemma_conc_df$dispersion) # moderate correlation

summary(mod3.glm)

mod3.lrm


# DKL ---------------------------------------------------------------------

## Test for a specific lemma

lemma_pos_full_df %>% filter(lemma == "increase")  -> increase_df

# Frequency of an item in the corpus parts
v <- table(increase_df$lemma, increase_df$text_category)

# Frequency of an item in the corpus
f <- length(increase_df$lemma)

# Percentages of corpus parts
s <- c(100/500, 80/500, 70/500, 50/500, 20/500, 30/500, 40/500, 40/500, 20/500, 20/500, 10/500, 20/500)

# Compute DKL
sum(v/f * log2(v/f * 1/s))

# Low value -> even spread (low divergence from reference distribution)
# High value -> uneven spread (high divergence from reference distribution)

# Implemented in the computer_dispersion() function

lemma_disp_output <- compute_dispersion(lemma_pos_full_df)

lemma_conc_df

#saveRDS(lemma_conc_df, "R_data/lemma_conc_df.RDS")


# Might also consider including polysemy count, collocation strength etc.

# Check out psycholinguistic measures: https://websites.psychology.uwa.edu.au/school/MRCDatabase/uwa_mrc.htm

# Lexical specificity -----------------------------------------------------


# MRC ---------------------------------------------------------------------

mrc <- read.csv("R_data/mrc2.csv", sep = "|", header = FALSE)

tibble(mrc)

mrc[5000,]

# Split up to make it interpretable

# Extract the large numeric sequence and additional information from column V1
mrc_split <- mrc %>%
  # Extract the large numeric sequence
  mutate(
    # Use regex to separate the numeric sequence and the rest
    V1_cleaned = gsub("(\\d+)(.*)", "\\1", V1),
    # Extract the rest of the information
    Rest = gsub("^(\\d+)(.*)", "\\2", V1)
  ) %>%
  # Separate the rest into the three additional columns
  separate(Rest, into = c("POS", "Category", "Lemma"), sep = " ", extra = "merge", fill = "right") %>% 
  as_tibble()



# Extract each component based on the fixed-width specifications
mrc_split$NLET <- substring(mrc[,1], 1, 2)
mrc_split$NPHON <- substring(mrc[,1], 3, 4)
mrc_split$NSYL <- substring(mrc[,1], 5, 5)
mrc_split$K_F_FREQ <- substring(mrc[,1], 6, 10)
mrc_split$K_F_NCATS <- substring(mrc[,1], 11, 12)
mrc_split$K_F_NSAMP <- substring(mrc[,1], 13, 15)
mrc_split$T_L_FREQ <- substring(mrc[,1], 16, 21)
mrc_split$BROWN_FREQ <- substring(mrc[,1], 22, 25)
mrc_split$FAM <- substring(mrc[,1], 26, 28)
mrc_split$CONC <- substring(mrc[,1], 29, 31)
mrc_split$IMAG <- substring(mrc[,1], 32, 34)
mrc_split$MEANC <- substring(mrc[,1], 35, 37)
mrc_split$MEANP <- substring(mrc[,1], 38, 40)
mrc_split$AOA <- substring(mrc[,1], 41, 43)
mrc_split$TQ2 <- substring(mrc[,1], 44, 44)
mrc_split$WTYPE <- substring(mrc[,1], 45, 45)
mrc_split$PDWTYPE <- substring(mrc[,1], 46, 46)
mrc_split$ALPHSYL <- substring(mrc[,1], 47, 47)
mrc_split$STATUS <- substring(mrc[,1], 48, 48)
mrc_split$VAR <- substring(mrc[,1], 49, 49)
mrc_split$CAP <- substring(mrc[,1], 50, 50)
mrc_split$IRREG <- substring(mrc[,1], 51, 51)

# Remove the original first column if no longer needed
mrc_split <- mrc_split[,-1]

# Remove other superfluous columns

mrc_split <- mrc_split[,-c(1:3)]

# Remove superfluous whitespace in the lemma column

mrc_split$Lemma <- trimws(mrc_split$Lemma)

# Testing

mrc_split %>% select(Lemma) %>% pull()

example[50:100,]

# Function to extract and clean lemma
extract_lemma <- function(lemma) {
  # Extract the lemma part after the "&" symbol
  cleaned_lemma <- str_extract(lemma, "&\\s*[^\\s]+") %>%
    str_replace("&\\s*", "") %>%
    tolower() %>%
    str_trim()
  return(cleaned_lemma)
}

# Try with this regex

regex <- "(?<=\\s)\\S.*" # it works; but I have to make sure it doesn't replace the irrelevant ones with NAs


# Use this one

extract_lemma2 <- function(lemma) {
  # Remove everything before the last sequence of non-whitespace characters
  cleaned_lemma <- str_replace(lemma, ".+?\\s+(?=\\S+$)", "") %>%
    tolower() %>%
    str_trim()
  
  return(cleaned_lemma)
}


# Apply the cleaning function and create a new column # WORKS
mrc_cleaned <- mrc_split %>%
  mutate(Cleaned_Lemma = sapply(Lemma, extract_lemma2)) %>% 
  relocate(Cleaned_Lemma)


saveRDS(mrc_cleaned, "R_data/mrc_cleaned.RDS") # screw this df, who encodes their stuff like that???

mrc_cleaned <- readRDS("R_data/mrc_cleaned.RDS")

# Check my verbs

mrc_cleaned %>% 
  filter(Cleaned_Lemma == "drink") %>% 
  View()

# Subset according to my verbs (all in the lemma_conc_df)

lemma_conc_df$lemma

mrc_cleaned %>% 
  dplyr::select(-V1_cleaned, -POS, -Lemma) %>% 
  filter(Cleaned_Lemma %in% lemma_conc_df$lemma) %>% 
  filter(Category == "VV") %>% 
  relocate(Cleaned_Lemma, Category, NLET, NPHON) -> mrc_df

hist(as.numeric(mrc_df$NPHON))

# WOW, I now have a massive psycholinguistic database that I can work with


# Convert all relevant columns to integers

# Columns to convert to numeric
columns_to_convert <- colnames(mrc_df)[3:16]

# Loop through each column and convert to numeric
for (col in columns_to_convert) {
  mrc_df[[col]] <- as.double(mrc_df[[col]])
}

# Combine everything ------------------------------------------------------

NLET <- mrc_df[mrc_df$Cleaned_Lemma == lemma,]$NLET


psycholinguistic_annotation <- function(data) {
  
  # Source df required (mrc_df)
  mrc_ratings <- mrc_df
  
  # Create empty list environment
  lemma_rating <- list()
  
  # Start loop
  for (i in unique(data$lemma)) {
    
    # Extract ratings for a lemma i
    NLET <- mrc_ratings[mrc_ratings$Cleaned_Lemma == i,]$NLET
    NPHON <- mrc_ratings[mrc_ratings$Cleaned_Lemma == i,]$NPHON
    NSYL <- mrc_ratings[mrc_ratings$Cleaned_Lemma == i,]$NSYL
    K_F_FREQ <- mrc_ratings[mrc_ratings$Cleaned_Lemma == i,]$K_F_FREQ
    K_F_NCATS <- mrc_ratings[mrc_ratings$Cleaned_Lemma == i,]$K_F_NCATS
    K_F_NSAMP <- mrc_ratings[mrc_ratings$Cleaned_Lemma == i,]$K_F_NSAMP
    T_L_FREQ <- mrc_ratings[mrc_ratings$Cleaned_Lemma == i,]$T_L_FREQ
    BROWN_FREQ <- mrc_ratings[mrc_ratings$Cleaned_Lemma == i,]$BROWN_FREQ
    FAM <- mrc_ratings[mrc_ratings$Cleaned_Lemma == i,]$FAM
    CONC <- mrc_ratings[mrc_ratings$Cleaned_Lemma == i,]$CONC
    IMAG <- mrc_ratings[mrc_ratings$Cleaned_Lemma == i,]$IMAG
    MEANC <- mrc_ratings[mrc_ratings$Cleaned_Lemma == i,]$MEANC
    MEANP <- mrc_ratings[mrc_ratings$Cleaned_Lemma == i,]$MEANP
    AOA <- mrc_ratings[mrc_ratings$Cleaned_Lemma == i,]$AOA
    
    # Add results to a tibble
    tibble(lemma = i,
           NLET = NLET,
           NPHON = NPHON,
           NSYL = NSYL,
           K_F_FREQ = K_F_FREQ,
           K_F_NCATS = K_F_NCATS,
           K_F_NSAMP = K_F_NSAMP,
           T_L_FREQ = T_L_FREQ,
           BROWN_FREQ = BROWN_FREQ,
           FAM = FAM,
           CONC = CONC,
           IMAG = IMAG,
           MEANC = MEANC,
           MEANP = MEANP,
           AOA = AOA,
    ) -> output_df
    
    # Store result of each iteration
    lemma_rating[[i]] <- output_df
    
  }
  
  # Combine the results
  full_ratings <- bind_rows(lemma_rating)
  
  # Finalise df
  data |> 
    mutate(NLET = full_ratings[match(data$lemma, full_ratings$lemma),]$NLET,
           NPHON = full_ratings[match(data$lemma, full_ratings$lemma),]$NPHON,
           NSYL = full_ratings[match(data$lemma, full_ratings$lemma),]$NSYL,
           K_F_FREQ = full_ratings[match(data$lemma, full_ratings$lemma),]$K_F_FREQ,
           K_F_NCATS = full_ratings[match(data$lemma, full_ratings$lemma),]$K_F_NCATS,
           K_F_NSAMP = full_ratings[match(data$lemma, full_ratings$lemma),]$K_F_NSAMP,
           T_L_FREQ = full_ratings[match(data$lemma, full_ratings$lemma),]$T_L_FREQ,
           BROWN_FREQ = full_ratings[match(data$lemma, full_ratings$lemma),]$BROWN_FREQ,
           CONC = full_ratings[match(data$lemma, full_ratings$lemma),]$CONC,
           FAM = full_ratings[match(data$lemma, full_ratings$lemma),]$FAM,
           IMAG = full_ratings[match(data$lemma, full_ratings$lemma),]$IMAG,
           MEANC = full_ratings[match(data$lemma, full_ratings$lemma),]$MEANC,
           MEANP = full_ratings[match(data$lemma, full_ratings$lemma),]$MEANP,
           AOA = full_ratings[match(data$lemma, full_ratings$lemma),]$AOA,
    ) -> full_annotated_df
  
  # End
  return(full_annotated_df)
  
}

psy_full_ann_df <- psycholinguistic_annotation(lemma_conc_df) # YES

saveRDS(psy_full_ann_df, "R_data/psy_full_ann_df.RDS")


# MODELS ------------------------------------------------------------------

psy_full_ann_df

train_data <- psy_full_ann_df[,-1]

train_data$variable <- as.factor(train_data$variable)
train_data$variable <- relevel(train_data$variable, ref = "invariant")

mod3.lrm <- lrm(variable ~ ., data = train_data)

train_data_clean <- train_data %>% drop_na()

mod.rf <- randomForest(variable ~ ., data = train_data_clean)

mod.rf2 <- ranger(variable ~ NLET +
                  NPHON +
                  NSYL +
                  log_frequency +
                  DKL +
                  Hnorm +
                  concreteness +
                  CONC +
                  FAM +
                  IMAG +
                  MEANC + # Meaningfulness ratings
                  MEANP + # the same but times 100
                  AOA, # Age of acquisition times 100
                  data = train_data_clean,
                  importance = "permutation")

# Insane results

ggplot(
  enframe(
    mod.rf2$variable.importance,
    name = "variable",
    value = "importance"
  ),
  aes(
    x = reorder(variable, importance),
    y = importance,
    fill = importance
  )
) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  ylab("Variable Importance") +
  xlab("") +
  ggtitle("Permutation Feature Importance") +
  guides(fill = "none") +
  scale_fill_gradient(low = "red", high = "blue") +
  theme_minimal()



