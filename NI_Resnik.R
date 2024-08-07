
# Part III: Resnik's selectional preference strength -----------------------

## Small sample for illustration

source("NI_Loading.R")

## Get verbs with objects

objects_sample <- NI_data_variable %>% 
  dplyr::filter(lemma == "eat" | lemma == "drink" | lemma == "finish", Object_FE != "null") %>% 
  dplyr::select(lemma, Object_FE)



