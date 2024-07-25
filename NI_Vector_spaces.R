
# Fit distributional semantic models here ---------------------------------

# Load libraries

source("NI_Libraries.R")
source("NI_Loading.R")
source("NI_Corpora.R") 

# Call corpus functions

## Read in ICE-GB
read_GB()

## Read in ICE-SING
read_SING()


# Lemmatise corpora -------------------------------------------------------

# Lemmatise and clean corpus

lemmatise_clean_ICE <- function(corpus) {

  Corpus_lemmatised <- tokens_replace(tokens(corpus),
                                      pattern = lexicon::hash_lemmas$token,
                                      replacement = lexicon::hash_lemmas$lemma)
  
  # Convert everything to lowercase
  
  Corpus_lemmatised |> tokens_tolower() -> Corpus_lemmatised_low
  
  ## Detokenize the corpus and remove superfluous white spaces
  
  Corpus_lemmatised_clean <- lapply(Corpus_lemmatised_low, paste, collapse = " ")
  
  Corpus_lemmatised_clean <- str_squish(Corpus_lemmatised_clean)
  
  
  ## Remove corpus annotation (cf. Leuckert 2019: 89)
  
  ### 1st layer: Remove extra-corpus text (<X> ... </X>)
  
  Corpus_lemmatised_clean1 <- gsub("<\\s*X[^>]*>(.*?)<\\s*/\\s*X>", "", Corpus_lemmatised_clean)
  
  ### 2nd layer: Remove untranscribed text (<O> ... </O>)
  
  Corpus_lemmatised_clean2 <- gsub("<\\s*O[^>]*>(.*?)<\\s*/\\s*O>", "", Corpus_lemmatised_clean1)
  
  ### 3rd layer: Remove editorial comments (<&> ... </&>)
  
  Corpus_lemmatised_clean3 <- gsub("<\\s*&[^>]*>(.*?)<\\s*/\\s*&>", "", Corpus_lemmatised_clean2)
  
  ### 4th layer: Remove all remaining (in-line) tags (< ... >)
  
  Corpus_lemmatised_clean4 <- gsub("<(.*?)>", "", Corpus_lemmatised_clean3)
  
  ### Return the fully-processed corpus object
  
  Corpus_lemmatised_final <<- Corpus_lemmatised_clean4

}



# Only lemmatise, do not clean:

lemmatise_ICE <- function(corpus) {
  
  Corpus_lemmatised <- tokens_replace(tokens(corpus),
                                      pattern = lexicon::hash_lemmas$token,
                                      replacement = lexicon::hash_lemmas$lemma)
  
  # Convert everything to lowercase
  
  Corpus_lemmatised |> tokens_tolower() -> Corpus_lemmatised_low
  
  ## Detokenize the corpus and remove superfluous white spaces
  
  Corpus_lemmatised_clean <- lapply(Corpus_lemmatised_low, paste, collapse = " ")
  
  Corpus_lemmatised_clean <- str_squish(Corpus_lemmatised_clean)
  
  return(Corpus_lemmatised_clean)
  
}


# Apply function to corpus objects

ICE_GB_lemmatised <- lemmatise_ICE(transcripts_collapsed_GB)

ICE_SING_lemmatised <- lemmatise_ICE(transcripts_collapsed_SING)

#saveRDS(ICE_GB_lemmatised_clean4, "Corpora/ICE_GB_lemmatised.RDS")

#ICE_GB_lemmatised <- readRDS("../Null Instantiation/Corpora/ICE_GB_lemmatised.RDS")


## Fit DSM -----------------------------------------------------------------


# Cosine similarity function (cf. Clark 2015)

cosine_similarity <- function(x, y) {
  sum(x * y) / (sqrt(sum(x * x)) * sqrt(sum(y * y)))
}

# Requires NI_data df and cosine_similarity() from here on out (cf. NI_Loading.R)

vector_space <- function(corpus_object, lemmas) {

  # Fit distributional model
  vector_space <- word2vec(corpus_object, window = 5, dim = 20, iter = 10, type = "skip-gram", hs = TRUE, sample = 0.001)

  # Fit embedding
  embedding <- predict(vector_space, lemmas, type = "embedding")
  
  # Create a matrix to store similarities
  similarity_matrix <- matrix(0, nrow = length(lemmas), ncol = length(lemmas))
  
  rownames(similarity_matrix) <- colnames(similarity_matrix) <- lemmas
  
  # Calculate pairwise similarities
  
  for (i in 1:length(lemmas)) {
    for (j in 1:length(lemmas)) {
      similarity_matrix[i, j] <- cosine_similarity(embedding[i, ], embedding[j, ])
    }
  }
  
  # Sanitize the similarity matrix by replacing NA/NaN/Inf values with 0 or other appropriate values
  similarity_matrix[is.na(similarity_matrix)] <- 0
  similarity_matrix[is.infinite(similarity_matrix)] <- 0
  
  
  # Convert to matrix
  NI_sim_mat <- similarity_matrix
  
  # Final clean-up
  
  ## Check for NA values in embeddings
  na_indices <- which(apply(NI_sim_mat, 1, function(x) any(is.na(x))))
  
  ## If there are NA values, remove those rows
  if (length(na_indices) > 0) {
    NI_sim_mat <- NI_sim_mat[-na_indices, ]
    lemmas <- lemmas[-na_indices]
  }
  
  ## Ensure there are no NA values
  if (any(is.na(NI_sim_mat))) {
    stop("There are still NA values in the embeddings.")
  }
  
  return(NI_sim_mat)

}

GB_sim_mat <- vector_space(ICE_GB_lemmatised, unique(NI_data$lemma))

#SING_sim_mat <- vector_space(ICE_SING_lemmatised, unique(NI_data$lemma))
