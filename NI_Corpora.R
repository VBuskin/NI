
# Construct corpora here ----------------------------------------------------


## British English ---------------------------------------------------------


# Load libraries
source("NI_Libraries.R")


read_GB <- function() {

  # define corpus files
  path_GB <- list.files("../Null Instantiation/Corpora/ICE_GB", full.names = TRUE)
  
  # load corpus files
  transcripts_GB <- sapply(path_GB, function(x){
    x <- readLines(x)
  })
  
  # Collapse every transcript into a single character vector
  transcripts_collapsed_GB <<- sapply(path_GB, function(x){
    # read-in text
    x <- readLines(x)
    # paste all lines together
    x <- paste0(x, collapse = " ")
    # remove superfluous white spaces
    x <- str_squish(x)
  })
}

#read_GB()

## Singapore English -------------------------------------------------------

read_SING <- function() {

  # define corpus files
  path_SING <- list.files("../Null Instantiation/Corpora/ICE_SING", full.names = TRUE)
  
  # load corpus files
  transcripts_SING <- sapply(path_SING, function(x){
    x <- readLines(x)
  })
  
  # Collapse every transcript into a single character vector
  transcripts_collapsed_SING <<- sapply(path_SING, function(x){
    # read-in text
    x <- readLines(x)
    # paste all lines together
    x <- paste0(x, collapse = " ")
    # remove superfluous white spaces
    x <- str_squish(x)
  })
}

#read_SING()

## Query corpora  ---------------------------------------------------

# Define NI_Verbs (careful: make sure there's no whitespace before the closing quoation mark ")

#NI_regexes <- unique(NI_data_variable$pattern)

#NI_Test <- c("eat")


kwic_ICE <- function(corpus, query) { 
  quanteda::kwic(
  # tokenize transcripts
  quanteda::tokens(corpus, what = "fasterword"), 
  # define search
  pattern = quanteda::phrase(query),
  # regex
  valuetype = "regex",
  # extend context
  window = 20) %>%
  # make it a data frame
  as.data.frame() %>%
  # clean docnames
  dplyr::mutate(docname = str_replace_all(docname, ".*/([A-Z][0-9][A-Z]-[0-9]{1,3}).txt", "\\1")) %>% 
  as_tibble()
}

kwic_SING <- kwic_ICE(transcripts_collapsed_SING, kwic_regexes)



# Get regexes -------------------------------------------------------------

#kwic_regexes <- unique(NI_data$pattern) # requires Summary 12-06-2024 or more recent




## Lemmatise hits ----------------------------------------------------------


# Define verbs to select from (working)


lemmatise_kwic <- function(kwic_object, lemma_source) {
  
  for (x in kwic_object) {
    kwic_lemmatised <- kwic_object %>% 
      # Requires lemma source to contain the columns "pattern" and "lemma"
      mutate(lemma = lemma_source[match(kwic_object$pattern, lemma_source$pattern),]$lemma)
  }
  # Return the lemmatised kwic df
  return(kwic_lemmatised)
  
}


kwic_SING_lemmatised <- lemmatise_kwic(kwic_SING, NI_data)

# Split hits --------------------------------------------------------------

# Store individual hits (optional)

#kwic_NI %>% 
 # filter(pattern == "\bsaut(e(s|d)?|ing)?\b")

# Split files according to their lemmas

df_to_split <- kwic_SING_lemmatised

splitData <- split(df_to_split, list(df_to_split$lemma))


# Write an .xlsx file for each element in splitData, using the lemmas as file names

for (i in names(splitData)) {
  write_xlsx(splitData[[i]], paste0(i, ".xlsx"))
}



