
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

# Function to get the best sense for a keyword in a sentence
def get_best_sense(sentence, keyword):
  try:
      # Tokenize the sentence and convert it to a list of lemmas
      tokens = nltk.word_tokenize(sentence)
      
      # Convert lemmas list back to a string to use as context in maxsim
      context_sentence = ' '.join(tokens)
      
      # Find the best sense of the keyword in the context of the sentence
      best_sense = maxsim(context_sentence, keyword, pos='v', option='wup')
      
      return best_sense
  except IndexError:
      return None

#get_best_sense("I eat food.", "eat")

# Iterate

senses_data = []

# Iterate over each row in the DataFrame
for i, row in python_input_df_py.iterrows():
    sentence = row['text_cleaned']
    keyword = row['keyword'].lower()  # Ensure keyword matching is case-insensitive
    
    # Get the best sense for the keyword in the sentence
    best_sense = get_best_sense(sentence, keyword)
    
    # Append the result to the senses_data list
    senses_data.append({
        "lemma": row['lemma'],
        "keyword": row['keyword'],
        "text_cleaned": sentence,
        "sense": str(best_sense) if best_sense else None,
        "definition": best_sense.definition() if best_sense else None
    })

# Create a new DataFrame from the senses_data list
senses_df = pd.DataFrame(senses_data)

# Display the DataFrame
print(senses_df)

# Now convert it back to an R object





