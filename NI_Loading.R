
# Perform all data loading and annotation here

# Load libraries from the external script
source("NI_libraries.R")

# Load in most recent summary file

data_raw <- tibble(read_xlsx("../Null Instantiation/Hits/Summaries/Summary_25_06_2024.xlsx"), na = "NA") 


# General -----------------------------------------------------------------

## Remove irrelevant cases

NI_data <- data_raw %>% 
  dplyr::filter(False_positive == "no" & Voice == "active" & Object_FE_Realisation != "x" & Object_FE_Realisation != "NA")  |> 
  dplyr::filter(!is.na(frame) & !is.na(Object_FE_Realisation) & is.na(Relevance)) %>% 
  mutate(across(c(Object_FE_Animacy, Object_Definiteness), ~ na_if(., "NA")))

## Add Text_category column

NI_data <- separate_wider_delim(NI_data, cols = docname, delim = "-", names = c("Text_category", "File_number"))


# Concreteness ------------------------------------------------------------

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
  
  NI_c_ratings <- bind_rows(lemma_rating)
  
  data |> 
    mutate(concreteness = NI_c_ratings[match(data$lemma, NI_c_ratings$lemma),]$concreteness,
           conc_sd = NI_c_ratings[match(data$lemma, NI_c_ratings$lemma),]$dispersion,
           conc_cv = NI_c_ratings[match(data$lemma, NI_c_ratings$lemma),]$cv) ->> NI_data_concreteness # Add to global environment
  
}

concreteness_annotation(NI_data)

# Selectional strength ----------------------------------------------------


# Create the tibble from Resniks's (1996: 138) results
verb_strength <- tibble(
  Verb = c("pour", "drink", "pack", "sing", "steal", "eat", "hang", "wear", "open", "push", "say", 
           "pull", "like", "write", "play", "hit", "catch", "explain", "read", "watch", "do", "hear", 
           "call", "want", "show", "bring", "put", "see", "find", "take", "get", "give", "make", "have"),
  Strength_Brown = c(4.80, 4.38, 4.12, 3.58, 3.52, 3.51, 3.35, 3.13, 2.93, 2.87, 2.82, 2.77, 2.59, 
                     2.54, 2.51, 2.49, 2.47, 2.39, 2.35, 1.97, 1.84, 1.70, 1.52, 1.52, 1.39, 1.33, 
                     1.24, 1.06, 0.96, 0.93, 0.82, 0.79, 0.72, 0.43)
)

# Loop through the data set

selec_rating <- list()

for (i in unique(NI_data$lemma)) {
  
  # Extract rating for a lemma i
  strength <- verb_strength[verb_strength$Verb == i,]$Strength_Brown
  
  # Add results to a tibble
  tibble(
    verb = i,
    Selectional_strength = strength) -> output_df
  
  # Store result of each iteration
  selec_rating[[i]] <- output_df
  
}

# Combine the results
NI_selec <- bind_rows(selec_rating)


# Perform the left join to add the selec_strength column
NI_data_selec2 <- NI_data_concreteness %>%
  left_join(NI_selec, by = c("lemma" = "verb")) %>%
  rename(selec_strength = Selectional_strength)


# Clusters ----------------------------------------------------------------

# OUT OF ORDER -- UNDER CONSTRUCTION (24-07-2024)

#NI_clusters <- readRDS("../Null Instantiation/Vector_spaces/df_tsne.RDS")

#NI_data_selec3 <- merge(NI_data_selec2, NI_clusters[, c("Lemma", "Cluster")], by.x = "lemma", by.y = "Lemma", all.x = TRUE)

# Rename the Cluster column

#names(NI_data_selec3)[which(names(NI_data_concreteness) == "Cluster")] <- "Cluster_Lemma"

#NI_data_clustered <- NI_data_selec3

# Get CLARA clusters

#clusters_merged <- readRDS("../Null Instantiation/Vector_spaces/clusters_merged.Rds")

#delete_lines <- c(12, 15, 29, 35, 42, 49, 55, 59, 66, 75, 79, 96, 106, 153, 162)

# Run this

#clusters_cleaned <- clusters_merged[-delete_lines,]

# Merge with main df

#NI_data_clustered_full <- merge(NI_data_clustered, clusters_cleaned[, c("Lemma", "Cluster", "Cluster_clara")], by.x = "lemma", by.y = "Lemma", all.x = TRUE) %>% dplyr::select(-Cluster.y) %>% rename(Cluster_vec = Cluster.x)



# Variable lemmas ---------------------------------------------------------

# Either use the ones saved or cf. NI_Variable_Lemmas.R for an up-to-date version

#variable_lemmas <- readRDS("../Null Instantiation/Vector_spaces/variable_lemmas.RDS")

# Find out which verbs are variable/invariantin my dataset
find_variable_lemmas <- function(df) {
  
  df %>% 
    dplyr::group_by(lemma, Object_FE_Realisation) %>% 
    dplyr::count() %>%
    # This is important, otherwise I can't access the n = 0 observations
    dplyr::ungroup() %>%
    # Make sure all combinations are present in the df
    tidyr::complete(lemma, Object_FE_Realisation, fill = list(n = 0)) -> Results_c
  
  # Get invariant cases
  Results_c %>% 
    dplyr::filter(n == 0) -> Results_invariant
  
  # Extract invariant lemmas
  Results_invariant_lemmas <- Results_invariant$lemma
  
  # Extract variable lemmas
  Results_variable_lemmas <- unique(Results_c$lemma[-which(Results_c$lemma %in%  Results_invariant_lemmas)])
  
  # End
  return(Results_variable_lemmas)
  
}

# Subset df by variable lemmas
NI_data %>% 
  filter(lemma %in% find_variable_lemmas(NI_data)) -> NI_data_variable
  
# Subset df by invariable lemmas
NI_data %>% 
  filter(lemma %nin% find_variable_lemmas(NI_data)) %>% 
  as_tibble() -> NI_data_invariable


# Factors and reference level ---------------------------------------------


# Convert to factors

columns_to_convert <- c("Object_FE_Realisation", "lemma", "Telicity", "frame", "Object_Definiteness", "Object_FE_Animacy", "Text_category")

for (col in columns_to_convert) {
  if (!is.numeric(NI_data_variable[[col]])) {
    NI_data_variable[[col]] <- as.factor(NI_data_variable[[col]])
  }
}


# Define ref level

NI_data_variable$Object_FE_Realisation <- as.factor(NI_data_variable$Object_FE_Realisation)

NI_data_variable$Object_FE_Realisation <- relevel(NI_data_variable$Object_FE_Realisation, ref = "overt")

# Convert to tibble

NI_data_variable <- as_tibble(NI_data_variable)



