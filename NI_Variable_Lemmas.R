# Identify variable lemmas from here

source("NI_Loading.R")

# Find out which verbs are invariant in my dataset

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

invariant_lemmas <- find_invariant_lemmas(NI_data)

#Remove irrelevant verbs from my main df

model_data %>% 
 dplyr::filter(!(lemma %in% Results_invariant_lemmas)) -> model_data_variable

model_data %>% 
 dplyr::filter(lemma %in% variable_lemmas) -> model_data_variable

variable_lemmas <- as.character(unique(model_data_variable$lemma))

# Store/Load

#saveRDS(variable_lemmas, "Vector_spaces/variable_lemmas.RDS")

#variable_lemmas <- readRDS("Vector_spaces/variable_lemmas.RDS")

#length(unique(model_data_variable$lemma)) # correspond to "allow_omission = 0"

NI_data_clustered_full %>% 
  dplyr::filter(lemma %in% variable_lemmas) -> NI_data_variable

