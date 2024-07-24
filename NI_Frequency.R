
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

## Decide which NI verbs to use

# Lemmas from the literature
NI_Verbs_full <- unique(data_raw$lemma) # 227

included_verbs <- NI_Verbs_full[NI_Verbs_full %in% verb_classes_df$verb] # 109

library("Hmisc")
missing_verbs <- NI_Verbs_full[NI_Verbs_full %nin% included_verbs]

all_verbs <- c(verb_classes_df$verb, missing_verbs)

# Confirmed variable lemmas
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



          