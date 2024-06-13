
# Import libraries
import nltk # NLP tools
import pywsd # for word sense disambiguation
import pyper # for converting python objects to R objects
import pandas as pd
# Set display options to show all rows and columns
pd.set_option('display.max_columns', None)  # Show all columns
pd.set_option('display.max_rows', None)     # Show all rows

# Import functions
from pywsd import disambiguate
from pywsd.allwords_wsd import disambiguate
from pywsd.lesk import simple_lesk
from pywsd.similarity import max_similarity as maxsim
from nltk.corpus import wordnet as wn
from nltk.tokenize import word_tokenize

# Accessing data from NI_Selectional_Strength in Python environment

print(python_input_df_py) # Print imported df

sentence = str(python_input_df_py.iloc[0, 2])

verb_dis = disambiguate(sentence, similarity_option='wup', keepLemmas=True)

# Iterate through verb_dis to find the meaning of 'Accepting'
verb_sense = None

for token in verb_dis:
  lemma = token[1]  # Get the lemma of the token
  if lemma == 'Accepting':
      print(f"Meaning of 'Accepting': {token[2]} ({token[2].definition() if token[2] else None})")
      break  # Exit loop once found

# Initialize an empty list to store the sense information
senses_data = []

sense = None
for token in verb_dis:
  lemma = token[1]  # Get the lemma of the token
  if lemma == keyword:  # Check if the lemma is 'eat'
      sense = (token[0], token[1], token[2])  # Extract the relevant information
      break  # Exit loop once found

# If eat_sense is found, add it to the senses_data list
if sense:
  senses_data.append({
      "Sentence": sentence,
      "Word": sense[0],
      "Lemma": sense[1],
      "Sense": str(sense[2]),  # Convert Synset to string
      "Definition": sense[2].definition() if sense[2] else None  # Get definition if Synset exists
  })
  
df = pd.DataFrame(senses_data)

df
