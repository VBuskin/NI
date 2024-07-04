
# Script to retrieve hyponyms

import pandas as pd
import random
from nltk.corpus import wordnet as wn
import nltk

# Define data
lemmas_list = lemmas_full['0'].tolist()

# Function to get the first hyponym for a given verb
def get_first_hyponym(word):
    synsets = wn.synsets(word, pos=wn.VERB)  # Ensure we only get verb synsets
    for synset in synsets:
        for hyponym in synset.hyponyms():
            for lemma in hyponym.lemmas():
                return lemma.name()  # Return the first hyponym found
    return None  # Return None if no hyponyms are found

# Create a dictionary to store lemmas and their first hyponym
hyponyms_dict = {word: get_first_hyponym(word) for word in lemmas_list}

# Convert dictionary to DataFrame
hyponyms_df = pd.DataFrame(hyponyms_dict.items(), columns=['lemma', 'hyponym'])


# Print the hyponyms for each word
#for word, hyponym in hyponyms_dict.items():
 #   print(f"{word}: {hyponym}")
