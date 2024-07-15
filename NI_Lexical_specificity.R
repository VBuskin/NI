
# An attempt to measure lexical specificity -------------------------------

# Requirements:

source("NI_Libraries.R")
source("NI_Loading.R")
source("NI_Corpora.R") 

# Requires 

# Call corpus functions

## Read in ICE-GB
read_GB()

## Apply lemmatisation function to corpus object of interest
ICE_GB_lemmatised <- lemmatise_ICE(transcripts_collapsed_GB)

## Fit DSM
vector_space <- word2vec(ICE_GB_lemmatised, window = 5, dim = 50, iter = 20)

embedding <- as.matrix(vector_space) # use this as input for collocation identification

#embeddings_verbs <- predict(vector_space, c("eat", "devour"), type = "embedding")

eat.nn <- nearest.neighbours(embedding, "eat", n = 200)

drink.nn <- nearest.neighbours(embedding, "drink", n = 200)

names(drink.nn) == names(eat.nn) # barely any overlap

names(eat.nn)

devour.nn <- nearest.neighbours(embedding, "devour", n = 100)

names(devour.nn) # also hardly any overlap

# Pairwise distances

## What do I choose as reference anyway?
## Is there a quantitative way to establish hyponymy? The distributional profiles just show me that increased lexical specificity goes along with lower cosine similarity. 

# Eat (hypernym - hyponym comparisons)

cosine_similarity(embedding["eat", ], embedding["drink", ]) # highest

cosine_similarity(embedding["eat", ], embedding["chew", ]) # high

cosine_similarity(embedding["eat", ], embedding["consume", ]) # medium

cosine_similarity(embedding["eat", ], embedding["bite", ]) # low

cosine_similarity(embedding["eat", ], embedding["devour", ]) # extremely low/ has its own niche!

# Others

cosine_similarity(embedding["speak", ], embedding["utter", ]) # high

cosine_similarity(embedding["speak", ], embedding["whisper", ])

cosine_similarity(embedding["speak", ], embedding["shout", ])

cosine_similarity(embedding["speak", ], embedding["scream", ])

# Grooming

cosine_similarity(embedding["wash", ], embedding["dress", ]) # medium, yes

cosine_similarity(embedding["wash", ], embedding["exercise", ]) # negative, yes

cosine_similarity(embedding["wash", ], embedding["brush", ])  # very high, no

cosine_similarity(embedding["wash", ], embedding["clean", ]) # medium, maybe

cosine_similarity(embedding["wash", ], embedding["pluck", ]) # very high, no

# Driving

cosine_similarity(embedding["drive", ], embedding["steer", ]) # med, yes

cosine_similarity(embedding["drive", ], embedding["brake", ]) # med, yes

cosine_similarity(embedding["drive", ], embedding["accelerate", ]) # med, yes

cosine_similarity(embedding["drive", ], embedding["turn", ]) # med, no

cosine_similarity(embedding["drive", ], embedding["rev", ]) # low, no

cosine_similarity(embedding["drive", ], embedding["floor", ]) # low, no

cosine_similarity(embedding["drive", ], embedding["gun", ]) # low, no


# Artistic things

cosine_similarity(embedding["paint", ], embedding["draw", ]) # low, yes


# Eat (verb - object comparisons)

cosine_similarity(embedding["smoke", ], embedding["cigarette", ])

cosine_similarity(embedding["read", ], embedding["book", ])


# Maybe it would make sense to include as predictor the coordinates of a verb in the vector space? Then I won't need the similarity scores.
