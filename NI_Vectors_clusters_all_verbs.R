
# Fit additional distributional semantic models here ---------------------------------

# Load scripts

source("NI_Vector_spaces.R")



# Load verb list ----------------------------------------------------------

trans_verbs <- read.csv("R_data/trans_verbs.csv", header = TRUE, sep = ",")

# Compare with NI verbs

trans_verbs %>% 
  dplyr::filter(verb %in% unique(data_raw$lemma))

trans_verbs[trans_verbs$verb %in% unique(data_raw$lemma),]


