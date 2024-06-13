#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jun 13 14:05:12 2024

@author: vladimirbuskin
"""

import nltk

#nltk.download("wordnet")

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


