
# Construct corpora here ----------------------------------------------------

# Load libraries
source("NI_Libraries.R")


# define corpus files
path_GB <- list.files("../Null Instantiation/Corpora/ICE_GB", full.names = TRUE)

# load corpus files
transcripts_GB <- sapply(path_GB, function(x){
  x <- readLines(x)
})

# Collapse every transcript into a single character vector
transcripts_collapsed_GB <- sapply(path_GB, function(x){
  # read-in text
  x <- readLines(x)
  # paste all lines together
  x <- paste0(x, collapse = " ")
  # remove superfluous white spaces
  x <- str_squish(x)
})


## Run multiple searches ---------------------------------------------------

# Define NI_Verbs (careful: make sure there's no whitespace before the closing quoation mark ")

#NI_regexes <- unique(NI_data_variable$pattern)

#NI_Test <- c("eat")

# Create a kwic df

#kwic_NI <- quanteda::kwic(
  # tokenize transcripts
 # quanteda::tokens(transcripts_collapsed_GB, what = "fasterword"), 
  # define search
#  pattern = quanteda::phrase(NI_Test),
  # regex
 # valuetype = "regex",
  # extend context
#  window = 20) %>%
  # make it a data frame
 # as.data.frame() %>%
  # clean docnames
  #dplyr::mutate(docname = str_replace_all(docname, ".*/([A-Z][0-9][A-Z]-[0-9]{1,3}).txt", "\\1")) 



## Lemmatise hits (inactive) ----------------------------------------------------------


# Define verbs to select from (it works!!!)

#NI_Verbs_hits <- as.character(levels(kwic_NI$pattern))

#NI_Lemmas_kwic <- NI_data %>% 
 # select(Pattern, NI_Verb) %>% 
  #filter(Pattern %in% NI_Verbs_hits)

#kwic_NI_2 <-  tibble(kwic_NI) %>% 
  #mutate(lemma = pattern)

# Important loop

#for (x in kwic_NI) {
 # kwic_NI_lemmatised <- kwic_NI %>% 
 #   mutate(lemma = NI_data[match(kwic_NI$pattern, NI_data$Pattern),]$NI_Verb)
#}