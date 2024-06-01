
# Fit distributional semantic models here ---------------------------------

# Load libraries

source("NI_Libraries.R")
source("NI_Loading.R")

# Read in and clean corpus

# define corpus files
path_GB <- list.files("../../Null Instantiation/Corpora/ICE_GB", full.names = TRUE)
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

# Lemmatise ICE-GB

ICE_GB_lemmatised <- tokens_replace(tokens(transcripts_collapsed_GB),
                                    pattern = lexicon::hash_lemmas$token,
                                    replacement = lexicon::hash_lemmas$lemma)

# Convert everything to lowercase

ICE_GB_lemmatised |> 
  tokens_tolower() -> ICE_GB_lemmatised_low

## Detokenize the corpus and remove superfluous white spaces

ICE_GB_lemmatised_clean <- lapply(ICE_GB_lemmatised_low, paste, collapse = " ")

ICE_GB_lemmatised_clean <- str_squish(ICE_GB_lemmatised_clean)


## Remove corpus annotation (cf. Leuckert 2019: 89)

### 1st layer: Remove extra-corpus text (<X> ... </X>)

ICE_GB_lemmatised_clean1 <- gsub("<\\s*X[^>]*>(.*?)<\\s*/\\s*X>", "", ICE_GB_lemmatised_clean)

### 2nd layer: Remove untranscribed text (<O> ... </O>)

ICE_GB_lemmatised_clean2 <- gsub("<\\s*O[^>]*>(.*?)<\\s*/\\s*O>", "", ICE_GB_lemmatised_clean1)

### 3rd layer: Remove editorial comments (<&> ... </&>)

ICE_GB_lemmatised_clean3 <- gsub("<\\s*&[^>]*>(.*?)<\\s*/\\s*&>", "", ICE_GB_lemmatised_clean2)

### 4th layer: Remove all remaining (in-line) tags (< ... >)

ICE_GB_lemmatised_clean4 <- gsub("<(.*?)>", "", ICE_GB_lemmatised_clean3)

#head(ICE_GB_lemmatised_clean4)


## Fit DSM -----------------------------------------------------------------


# Cleaned ICE_GB (save or read in)

#saveRDS(ICE_GB_lemmatised_clean4, "Corpora/ICE_GB_lemmatised.RDS")

ICE_GB_lemmatised_clean4 <- readRDS("../Null Instantiation/Corpora/ICE_GB_lemmatised.RDS")

# Fit model

ICE_GB_vec <- word2vec(ICE_GB_lemmatised_clean4, window = 5, dim = 20, iter = 10, type = "skip-gram", hs = TRUE, sample = 0.001)


# REQUIRES NI_DATA DF from here on out (cf. NI_Loading.R)

## Apply to those NI verbs that occur in the results

NI_lemmas <- unique(NI_data$lemma) # there must be some that cause problems


## Create embedding

embedding_NI <- predict(ICE_GB_vec, NI_lemmas, type = "embedding")

# Function to compute cosine similarity (dotted product / euclidian distances)

cosine_similarity <- function(x, y) {
  sum(x * y) / (sqrt(sum(x * x)) * sqrt(sum(y * y)))
}


# Create a matrix to store similarities

similarity_matrix <- matrix(0, nrow = length(NI_lemmas), ncol = length(NI_lemmas))
rownames(similarity_matrix) <- colnames(similarity_matrix) <- NI_lemmas

# Calculate pairwise similarities

for (i in 1:length(NI_lemmas)) {
  for (j in 1:length(NI_lemmas)) {
    similarity_matrix[i, j] <- cosine_similarity(embedding_NI[i, ], embedding_NI[j, ])
  }
}


# Show individual values
# or
#library("proxy")
#proxy::simil(embedding_NI["eat",], method = "cosine")

# These two lines are equivalent

cosine_similarity(embedding_NI["eat",], embedding_NI["drink",])

sim_mat <- as.matrix(proxy::simil(embedding_NI[c("eat", "drink"), , drop = FALSE], method = "cosine"))


# Sanitize the similarity matrix by replacing NA/NaN/Inf values with 0 or other appropriate values

similarity_matrix[is.na(similarity_matrix)] <- 0
similarity_matrix[is.infinite(similarity_matrix)] <- 0


# Convert to matrix

NI_w2v_mat <- similarity_matrix


