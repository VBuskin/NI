#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jun 13 14:05:12 2024

@author: vladimirbuskin
"""

import nltk # NLP tools
import pywsd # for word sense disambiguation
import pyper # for converting python objects to R objects

#nltk.download('averaged_perceptron_tagger')
#nltk.download("wordnet")
#nltk.download("punkt")

import pandas as pd
# Set display options to show all rows and columns
pd.set_option('display.max_columns', None)  # Show all columns
pd.set_option('display.max_rows', None)     # Show all rows

from pywsd import disambiguate
from pywsd.lesk import simple_lesk
from pywsd.similarity import max_similarity as maxsim
from nltk.corpus import wordnet as wn

# Get synsets for the lemma 'eat'
synsets = wn.synsets('eat', pos=wn.VERB)

# Iterate over synsets and print details
for i, synset in enumerate(synsets, start=1):
    print(f"Sense {i}:")
    
    # Print the definition of the synset
    print("Definition:", synset.definition())
    
    # Print examples of the synset
    examples = synset.examples()
    print("Examples:", examples)
    
    # Print hypernyms of the synset (optional)
    hypernyms = synset.hypernyms()
    print("Hypernyms:", [hypernym.definition() for hypernym in hypernyms])
    
    print()

#### Disambiguation ####

# Example sentences
sentences = [
    "He eats a lot.",
    "This eats lots of resources."
]

for sentence in sentences:
    print(f"Sentence: {sentence}")
    # Disambiguate the senses of all words in the sentence
    disambiguated = disambiguate(sentence)
    
    # Extract the sense for the word "eat"
    for word, sense in disambiguated:
        if word.lower() == "eats" or word.lower() == "eat":
            print(f"Word: {word}")
            if sense:
                print(f"Sense: {sense}")
                print(f"Definition: {sense.definition()}")
            else:
                print("No sense found.")
    print("\n")

# Attempt 2
sent = 'I went to the bank to deposit my money'
ambiguous = 'bank'
answer = simple_lesk(sent, ambiguous, pos='n')
print(answer)
print(answer.definition())

# Attempt 3
disambiguate('I went to the bank to deposit my money')

# Some more
eat_sentence = "I ate a lot of food."
eat_disambiguated = disambiguate(eat, algorithm=maxsim, similarity_option='wup', keepLemmas=True)

eat2 = "This eats so much fuel."
eat_disambiguated2 = disambiguate(eat2, algorithm=maxsim, similarity_option='wup', keepLemmas=True)

# Initialize an empty list to store the sense information
senses_data = []

# Extract the sense for the word "eat"
eat_sense = None
for token in eat_disambiguated:
  lemma = token[1]  # Get the lemma of the token
  if lemma == 'eat':  # Check if the lemma is 'eat'
      eat_sense = (token[0], token[1], token[2])  # Extract the relevant information
      break  # Exit loop once found

# If eat_sense is found, add it to the senses_data list
if eat_sense:
  senses_data.append({
      "Sentence": eat_sentence,
      "Word": eat_sense[0],
      "Lemma": eat_sense[1],
      "Sense": str(eat_sense[2]),  # Convert Synset to string
      "Definition": eat_sense[2].definition() if eat_sense[2] else None  # Get definition if Synset exists
  })
  
# Store in df
df = pd.DataFrame(senses_data)

# Convert to R object

r = pyper.R()

# Convert pandas DataFrame to R data frame
r.assign("R_df", df)

# Verify the conversion by printing the R data frame in R
r("print(R_df)")

# Save the R workspace with the data
r.run("save.image('r_workspace.RData')")


