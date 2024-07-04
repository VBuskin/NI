

# Introduction ------------------------------------------------------------

# This small-scale analysis serves to ascertain whether lemma concreteness correlates with sense specificity.

# Load previous scripts
source("NI_Loading.R")
library("effsize")

# View unique lemmas
as.character(unique(NI_data_variable$lemma))

# Draw random sample
set.seed(2606)

lemma_sample <- sample(as.character(unique(NI_data_variable$lemma)))

# Load concreteness ratings
c_ratings <- read.table("../Null Instantiation/Concreteness_ratings.csv", sep = "\t", header = TRUE)

# Load hyponyms from ChatGPT (perhaps use this rather than do the Python thing)
hyponyms <- c("respond", "demonstration", "approximation", "observe", "offer", "cleanse", 
              "repair", "vend", "recommence", "trim", "blame", "imbibe", 
              "strike", "iterate", "clarify", "decline", "complete", "prick", 
              "debate", "peck", "punt", "keyboard", "prepare", "startle", 
              "embrace", "chauffeur", "flip", "encounter", "persuade", "gesture", 
              "press", "partition", "slice", "wed", "enroll", "separate", 
              "dine", "loan", "tug", "retreat", "depart", "resist", 
              "tour", "misplace", "savor", "neglect", "spill", "clean", 
              "carve", "advise")


# Use Python

# Check if pandas is available in Python environment
py_module_available("pandas")

pd <- import("pandas")

lemmas_pd2 <- pd$DataFrame(lemma_sample)

py$lemmas_full <- lemmas_pd2


# Source the script

source_python("NI_Semantic_Annotation.py")

# Convert target df to R object
hyponyms <- py$hyponyms_df

hyponyms <- as_tibble(hyponyms) # Remove NAs

hyponyms %>% drop_na(hyponym) -> hyponyms



# Retrieve concreteness ratings for lemmas ------------------------------


# Create empty list environment
lemma_rating <- list()

# Start loop
for (i in lemma_sample) {
  
  # Extract rating for a lemma i
  rating <- c_ratings[c_ratings$Word == i,]$Conc.M
  
  # Add results to a tibble
  tibble(lemma = i,
         concreteness = rating) -> output_df
  
  # Store result of each iteration
  lemma_rating[[i]] <- output_df
  
}

# Combine the results

lemma_ratings <- bind_rows(lemma_rating)

# Retrieve concreteness ratings for hyponyms ------------------------------

# Create empty list environment
hyponym_rating <- list()

# Start loop
for (i in hyponyms$hyponym) {
  
  # Extract rating for a lemma i
  rating <- c_ratings[c_ratings$Word == i,]$Conc.M
  
  # Add results to a tibble
  tibble(hyponym = i,
         concreteness_hyponym = rating) -> output_df
  
  # Store result of each iteration
  hyponym_rating[[i]] <- output_df
  
}

# Combine the results

hyponym_ratings <- bind_rows(hyponym_rating)

# Filter hyponyms to include only those in hyponym_ratings
subset_lemmas_hyponyms <- semi_join(hyponyms, hyponym_ratings, by = "hyponym")

lem_hyp_subset <- hyponyms[hyponyms$hyponym %in% hyponym_ratings$hyponym, ] # does the same thing

# Subset the full df according to the hyponym df (only matches) and get the ratings

#This works
lem_hyp_subset$lemma_rating <- lemma_ratings[lemma_ratings$lemma %in% lem_hyp_subset$lemma,]$concreteness

# This doesn't
#lem_hyp_subset$hyponym_rating <- hyponym_ratings[hyponym_ratings$hyponym %in% lem_hyp_subset$hyponym,]$concreteness_hyponym 

lemma_hyponyms_ratings_full <- merge(lem_hyp_subset, hyponym_ratings, by = "hyponym", all.x = TRUE)

lemma_hyponyms_ratings_full %>% 
  dplyr::rename(lemma_concreteness = lemma_rating,
                hyponym_concreteness = concreteness_hyponym) -> lemma_hyponyms_ratings_full

# Test for differences

t.test(lemma_hyponyms_ratings_full$lemma_concreteness, lemma_hyponyms_ratings_full$hyponym_concreteness, paired = TRUE) # damn

lemma_hyponyms_ratings %>% drop_na() -> lemma_hyponyms_ratings2
cohen.d(lemma_hyponyms_ratings2$concreteness, lemma_hyponyms_ratings2$concreteness_hyponym)

var.test(lemma_hyponyms_ratings$concreteness, lemma_hyponyms_ratings$concreteness)



