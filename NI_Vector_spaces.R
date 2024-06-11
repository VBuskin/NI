
# Fit distributional semantic models here ---------------------------------

# Load libraries

source("NI_Libraries.R")
source("NI_Loading.R")
source("NI_Corpora.R") # get function definitions

# Call corpus functions

## Read in ICE-GB
read_GB()

## Read in ICE-SING
read_SING()


# Lemmatise corpora -------------------------------------------------------


lemmatise_ICE <- function(corpus) {

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


# Apply function to corpus objects

ICE_GB_lemmatised <- lemmatise_ICE(transcripts_collapsed_GB)

ICE_SING_lemmatised <- lemmatise_ICE(transcripts_collapsed_SING)



## Fit DSM -----------------------------------------------------------------


# Cleaned ICE_GB (save or read in)

#saveRDS(ICE_GB_lemmatised_clean4, "Corpora/ICE_GB_lemmatised.RDS")

#ICE_GB_lemmatised <- readRDS("../Null Instantiation/Corpora/ICE_GB_lemmatised.RDS")

# Fit distributional model

corpus_object <- ICE_GB_lemmatised

ICE_GB_vec <- word2vec(corpus_object, window = 5, dim = 20, iter = 10, type = "skip-gram", hs = TRUE, sample = 0.001)


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


