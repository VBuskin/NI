# Identify variable lemmas from here

source("NI_Loading.R")

# Find out which verbs are invariant in my dataset

model_data %>% 
 dplyr::group_by(lemma, Object_FE_Realisation) %>% 
count(Object_FE_Realisation, .drop = FALSE) -> Results_c

Results_c %>% 
 dplyr::filter(n == 0) -> Results_invariant

Results_invariant_lemmas <- Results_invariant$lemma

Results_variable_lemmas <- unique(Results_c$lemma[-which(Results_c$lemma %in% Results_invariant_lemmas)])

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

