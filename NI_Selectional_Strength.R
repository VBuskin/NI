

# Compute selectional strength and work with WordNet ----------------------


# Set the WNHOME environment variable to the parent directory of 'dict'
Sys.setenv(WNHOME = "../WordNet-3.0")

library("wordnet")

#setDict("../WordNet-3.0/dict")

initDict()

# Available filter types
getFilterTypes()

# Example
filter <- getTermFilter("StartsWithFilter", "car", TRUE)
terms <- getIndexTerms("NOUN", 5, filter)
sapply(terms, getLemma)

# Synonyms
filter <- getTermFilter("ExactMatchFilter", "company", TRUE)
terms <- getIndexTerms("NOUN", 1, filter)
getSynonyms(terms[[1]])


# Related sets
filter <- getTermFilter("ExactMatchFilter", "hot", TRUE)
terms <- getIndexTerms("ADJECTIVE", 1, filter)
synsets <- getSynsets(terms[[1]])
related <- getRelatedSynsets(synsets[[1]], "!")
sapply(related, getWord)


# Usage -------------------------------------------------------------------

# Eat
filter <- getTermFilter("ExactMatchFilter", "eat", TRUE)
terms <- getIndexTerms("VERB", 5, filter)
getSynonyms(terms[[1]])



