#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jun 13 14:05:12 2024

@author: vladimirbuskin
"""

import nltk # NLP tools
import pywsd # for word sense disambiguation
import pyper # for converting python objects to R objects
import math

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
from nltk.tokenize import word_tokenize

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

#### Get WordNet class of objects ####

import spacy
import nltk
from nltk.corpus import wordnet as wn
from nltk.stem import WordNetLemmatizer
nlp = spacy.load('en_core_web_sm')
from spacy.lang.en.examples import sentences
from collections import defaultdict

# Define dictionary

class KeyBasedDefaultDict(dict):
    def __init__(self, default_factories, *args, **kw):
        self._default_factories = default_factories
        super(KeyBasedDefaultDict, self).__init__(*args, **kw)

    def __missing__(self, key):
        factory = self._default_factories.get(key)
        if factory is None:
            raise KeyError(key)
        new_value = factory()
        self[key] = new_value
        return new_value


# Define parsing function parse_string() VERSION 1

def parse_string(string):
    doc = nlp(string)
    for token in doc:
        print(str(token) + "(" + str(token.dep_) + "; " + str(token.pos_) + ")")
    return doc
  
# Define parsing function parse_string() VERSION 2

  def parse_string(string):
    doc = nlp(string)
    output = []
    for token in doc:
        output.append(f"{str(token)}({str(token.dep_)}; {str(token.pos_)})")
    parsed_string = " ".join(output)  # Join list elements with newline character
    return parsed_string


## Examples

parse_string("This is a sentence.")

parse_string("He kicked the ball.")

parse_string("He forgot to tell you.")

# Get verbs function get_verbs()

def get_verbs(sent):
    verbs = []
    for token in sent:
        if str(token.pos_) == "VERB":
            verbs.append(str(token.lemma_))
    return verbs
  
  get_verbs(parse_string("The child hit the ball and ran to first base."))
  
# Assess eligibility of the sentence  
  
"""
Takes a parsed sentence, the sentence string, and a verb lemma.  Returns one of 3 things....

1. Returns the object of that verb if there is one (according to dependency parse).
2. Excludes as "ineligible" a lot of cases to correctly classify implicit objects.....
            namely passive sentences, sentences where the verb is classified as a subject
                    or object or modifier, and other cases (basically crafted by hand)
3.  If there is NO OBJECT but the sentence is eligible, returns "No object".
"""
def get_object(sent, sent_string, verb):
    is_object = False
    eligible = True
    obj = ""
    verb_lemma_in_sentence = False
    for token in sent:
        if (str(token.lemma_) == verb) and (str(token.pos_) == "VERB"): 
            # make sure the verb is actually in the sentence somewhere, as a verb.
            verb_lemma_in_sentence = True

        #exclude = [token.dep_ == u"auxpass", 
                   # a bunch of reasons that we should EXCLUDE the sentence.
        #token.dep_ == "nsubjpass",
        #(token.lemma_ == verb  and (token.dep_ == u"amod"
         #                           or token.dep_ == u"xcomp"
          #                          or token.dep_ == u"acomp"
           #                         or token.dep_ == "nsubj"
            #                        or token.dep_ == "prep"
             #                       or token.dep_ == "dobj"
              #                      or token.dep_ == "conj"
               #                     or token.dep_ == u"oprd"
                #                    or token.dep_ == u"acomp"
                 #                   or token.dep_ == u"ccomp"
                  #                  or token.dep_ == u"npadvmod"
                   #                 or token.dep_ == "advcl"
                    #                or token.dep_ == u"acl"
                     #               or token.dep_ == u"relcl"
                      #              or token.dep_ == u"compound"
                       #             or token.dep_ == u"aux")),
        #token.head.lemma_ == verb and (token.dep_ == u"xcomp"
         #                                     or token.pos_ == u"part"
          #                                    or token.dep_ == u"prt"
           #                                   or token.dep_ == u"conj"
            #                                  or token.dep_ == u"prep"
             #                                 or token.dep_ == u"agent"
              #                                or token.dep_ == u"advcl"
               #                               or token.dep_ == u"intj"
                #                              or token.dep_ == "ccomp"
                 #                             or token.dep_ == u"pcomp")]
        #if True in exclude:
         #       eligible = False
    
        if (token.dep_ == u"obj" or token.dep_ == u"dobj") and (str(token.head.lemma_) == verb):
                # makes sure this token is an OBJECT in the parse
                # and also that its "HEAD" in the dependency parse is the specified verb.
                is_object = True
                obj = token.lemma_
                eligible = True
            
    if not verb_lemma_in_sentence:
            eligible = False 
            
# Can't use this without AllenNLP        
  #  if eligible and (not is_object): # using the semantic roles ONLY to check for
   #             # further false positives for object omission
    #    lemmatizer = WordNetLemmatizer()
     #   semroles = predictor.predict(sent_string)
      #  for v in semroles["verbs"]:
       #     if lemmatizer.lemmatize(v["verb"], pos="v") == verb:
        #        print(v["description"])
         #       arg0 = "ARG0" in v["description"]
          #      arg1 =  "ARG1" in v["description"]
           #     if (arg0 and arg1):
            #        is_object = True
             #   elif (arg1 and not arg0):
              #      eligible = False
               #     print("EXCLUDED by semantic role labeler")
                #elif (arg0 and not arg1):
                 #   is_object = False
                
    if not eligible:
        return "Ineligible"
    elif is_object:
        return str(obj)
    elif eligible and (not is_object): # eligible sentences with no object.
        return "No object"
  


get_object(parse_string("The child threw balls"), "The child threw balls", "throw")
get_object(parse_string("The child threw the ball"), "The child threw the ball", "walk")
get_object(parse_string("The child threw."), "The child threw", "throw")

# Helper functions

# takes a list of dicts (all with same keys) and returns a list of the keys
def get_list_of_keys(list_of_dicts):
    set_of_keys = set()
    for d in list_of_dicts:
        keys = d.keys()
        for key in keys:
            set_of_keys.add(key)
    return list(set_of_keys)

##writes a list of dicts to a csv file
def dicts_to_csv(list_of_dicts, filename):
    f = open(filename, 'w')
    list_of_keys = get_list_of_keys(list_of_dicts)
    dict_writer = csv.DictWriter(f, list_of_keys)
    dict_writer.writeheader()
    dict_writer.writerows(list_of_dicts)

# "Simple selection" 
# -- of all the times a verb V occurs with any object,
# what percent of the time does it occur with its most common (non-pronoun) object?

def get_verb_obj_dict(verb_obj_pairs):
    verb_obj_dict = defaultdict(lambda: defaultdict(int))
    # outer keys are verbs, inner keys are objects of that verb,
                    # values are the number of occurrences of that obj with that verb
    for pair in verb_obj_pairs:
        verb = pair.split(" ")[0]
        obj = pair.split(" ")[1]
        verb_obj_dict[verb][obj] += 1
    return verb_obj_dict
  
for key in get_verb_obj_dict(["throw ball", "throw ball", "throw rock", "eat sandwich"]).keys():
    print(key)
    print(get_verb_obj_dict(["throw ball", "throw ball", "throw rock", "eat sandwich"])[key])

"""
Returns the percent of the time
a given object appears as a percentage of all objects
of a given verb.
Input: a particular lemmatized object; and a dict (verb_obj) with the keys being different objects
and the values being the number of times that object occurs.
"""
def how_often_obj_with_verb(max_obj, verb_obj):
    total_v_with_obj = 0
    for objkey in verb_obj.keys():
        total_v_with_obj += verb_obj[objkey]
    if verb_obj[max_obj] < 2:
        return "n/a"
    else:
        return 100 * round(float(verb_obj[max_obj]) / total_v_with_obj, 2)

test = {"ball": 2, "rock": 1}
how_often_obj_with_verb("ball", test)

def get_simple_sel_dict(verb_obj_pairs):
    mapping = {"sel": float,
               "maxobjs": str      
    }
    simple_sel_dict = defaultdict(lambda: KeyBasedDefaultDict(mapping))
    verb_obj_dict = get_verb_obj_dict(verb_obj_pairs)
    for verb in verb_obj_dict.keys():
        max_objs = ""
        verb_obj = verb_obj_dict[verb].copy()
        if len(verb_obj.keys()) > 1:
            max_obj = max(verb_obj.keys(), key=(lambda k: verb_obj[k]))
            simple_sel_dict[verb]["sel"] = how_often_obj_with_verb(max_obj, verb_obj)
            max_objs += max_obj + "(" + str(verb_obj[max_obj]) + "),"
            del verb_obj[max_obj]
            counter = 0
            while counter < 2 and len(verb_obj.keys()) > 1:
                max_obj = max(verb_obj.keys(), key=(lambda k: verb_obj[k]))
                max_objs += max_obj + "(" + str(verb_obj[max_obj]) + "),"
                del verb_obj[max_obj]
                counter += 1
            simple_sel_dict[verb]["maxobjs"] = max_objs
        else:
            simple_sel_dict[verb]["sel"] = "n/a"
            simple_sel_dict[verb]["maxobjs"] = "n/a"
    return simple_sel_dict
  
get_simple_sel_dict(["throw ball", 
                     "throw ball", 
                     "throw ball", 
                     "throw rock", 
                     "throw rock", 
                     "throw frisbee", 
                     "eat lunch", 
                     "eat food"])
                     
                     
#### Resnik ####

"""
Takes a sentence and a verb.  Returns one of 2 things....

1. Returns the object of that verb if there is one (according to dependency parse).
2.  Returns "False" if no object.
"""
def get_actual_object(sent, verb):
    obj = ""
    is_object = False
    for token in sent:
        if (token.dep_ == u"obj" or token.dep_ == u"dobj") and (str(token.head.lemma_) == verb):
                # makes sure this token is an OBJECT in the parse
                # and also that its "HEAD" in the dependency parse is the specified verb.
                is_object = True
                obj = token.lemma_
    if is_object:
        return str(obj)
    else:
        return False

get_actual_object(parse_string("The child threw a ball"), "throw")

get_actual_object(parse_string("The child threw"), "throw")


## Get synsets of nouns

class_dict = defaultdict(set) # for each class, its words. a set so we don't double-count.
word_dict = defaultdict(set) # for each word, its classes.  a set so we don't double-count.
word_freq_dict = defaultdict(int) # for each word (object), its number of occurrences.
class_freq_dict = defaultdict(float)

lemma = wn.lemmas("ball")

synset_1 = lemma[1].synset()

synset_1.hypernyms()

synset_1.hypernym_paths()

word_freq

# Implementation

import math
from collections import defaultdict

# Example dictionaries for illustration (replace with actual data)
word_freq_dict = {
    'word1': 5,
    'word2': 10,
    'word3': 7,
    # Add more words and their frequencies
}

word_dict = {
    'word1': ['class1', 'class2'],
    'word2': ['class1'],
    'word3': ['class2', 'class3'],
    # Add more words and the classes they belong to
}

class_dict = {
    'class1': ['word1', 'word2'],
    'class2': ['word1', 'word3'],
    'class3': ['word3'],
    # Add more classes and their associated words
}

sample_instance = ['word1', 'word3']

# Calculate class frequencies for the sample instance
class_freq_dict = defaultdict(float)  # for each class, its frequency

for word in sample_instance:
    if word in word_freq_dict and word in word_dict:
        for c in word_dict[word]:
            class_freq_dict[c] += word_freq_dict[word] / float(len(word_dict[word]))

# Print class frequencies for the sample instance
print("Class frequencies for the sample instance:")
for c, freq in class_freq_dict.items():
    print(f"{c}: {freq}")

# Calculate the total frequency N
N = sum(class_freq_dict.values())

# Calculate the probability of each class
class_prob_dict = defaultdict(float)  # for each class, its probability

for c in class_freq_dict.keys():
    class_prob_dict[c] = class_freq_dict[c] / float(N)

# Print class probabilities
print("\nClass probabilities:")
for c, prob in class_prob_dict.items():
    print(f"{c}: {prob}")

# Sanity check: Ensure the sum of all probabilities is 1
class_prob_total = sum(class_prob_dict.values())
print("\nClass probability total should be 1: ", class_prob_total)

# New step: Calculate the frequency at which a member of a class serves as the object of a verb
verb_class_dict = defaultdict(lambda: defaultdict(float))  # for each verb, for each class, the count of each class as the object of that verb

# Example data for illustration (replace with actual data)
verbs_of_interest = ['verb1', 'verb2', 'verb3']
verb_obj_dict = {
    'verb1': {'word1': 3, 'word2': 2, 'word3': 5},
    'verb2': {'word1': 1, 'word2': 4, 'word3': 6},
    'verb3': {'word1': 0, 'word2': 2, 'word3': 3},
    # Add more verbs and their object word frequencies
}

print("\nMaking dict of verbs with each class...")
for v in verbs_of_interest:  # for each verb that we actually care about
    for c in class_dict.keys():
        freq = 0
        for w in class_dict[c]:  # for each word in the class c
            freq += verb_obj_dict[v].get(w, 0)  # number of times w appears as object of v
        verb_class_dict[v][c] = freq

# Print the verb_class_dict
print("\nVerb class frequencies:")
for v, class_freqs in verb_class_dict.items():
    for c, freq in class_freqs.items():
        print(f"Verb: {v}, Class: {c}, Frequency: {freq}")

# Next step: Calculate the probability of a class c given a verb v
verb_class_prob_dict = defaultdict(lambda: defaultdict(float))  # for each verb, for each class, the probability of that class as obj of that verb

print("\nMaking dict of prob of each class with each verb...")
for verb in verbs_of_interest:
    N = sum(verb_obj_dict[verb].values())  # Total observations of this particular verb v in the whole sample
    for c in verb_class_dict[verb].keys():  # for each class that's occurred as obj with this verb
        s = 0
        for obj in class_dict[c]:  # for each object that falls into that class
            s += verb_obj_dict[verb].get(obj, 0) / float(len(word_dict[obj]))  # Sum up the number of times that obj appears with that verb divided by the total number of classes that obj falls into
        if s > 0:
            verb_class_prob_dict[verb][c] = s / float(N)

# Print verb_class_prob_dict
print("\nVerb class probabilities:")
for v, class_probs in verb_class_prob_dict.items():
    for c, prob in class_probs.items():
        print(f"Verb: {v}, Class: {c}, Probability: {prob}")

# Sanity check: Ensure the total probability of all classes as the object of a given verb sums to 1
for verb in verbs_of_interest:
    class_prob_total = sum(verb_class_prob_dict[verb].values())
    print(f"\nProbability total for given verb '{verb}' should be 1: {class_prob_total}")

# New step: Calculate the Selectional Strength of each verb
verb_sel_strength_dict = defaultdict(float)  # for each verb, its selectional strength

print("\nCalculating selectional strength of each verb...")
for verb in verbs_of_interest:
    sel_strength = 0
    for c in verb_class_prob_dict[verb].keys():  # for each class that's occurred as obj with this verb
        c_given_v = verb_class_prob_dict[verb][c]
        pr_c = class_prob_dict[c]
        if c_given_v > 0:
            sel_strength += (c_given_v * math.log(c_given_v / pr_c))
    verb_sel_strength_dict[verb] = sel_strength

# Print verb_sel_strength_dict
print("\nSelectional strengths of verbs:")
for verb, strength in verb_sel_strength_dict.items():
    print(f"Verb: {verb}, Selectional Strength: {strength}")
    

# Last step: Selectional association

import math
from collections import defaultdict

# Initialize the selectional association dictionary
sel_assn_dict = defaultdict(lambda: defaultdict(float))  # for each verb, for each class, the association between the verb and that class

# Calculate the selectional association for each verb and each class it appears with
for verb in verbs_of_interest:
    for c in verb_class_dict[verb].keys():  # for each class that's occurred as object with this verb
        c_given_v = verb_class_prob_dict[verb][c]
        pr_c = class_prob_dict[c]
        sel_strength = verb_sel_strength_dict[verb]
        if c_given_v > 0:
            sel_assn_dict[verb][c] = 100 * (c_given_v / sel_strength) * math.log(c_given_v / pr_c)

# Print the selectional association dictionary
print("\nSelectional associations:")
for verb, class_assns in sel_assn_dict.items():
    for c, assn in class_assns.items():
        print(f"Verb: {verb}, Class: {c}, Selectional Association: {assn}")

# Sanity check: for each verb, the sum of the association values for all classes should be 100
for verb in verbs_of_interest:
    total = 0
    for c in class_dict.keys():
        total += sel_assn_dict[verb][c]
    print(f"\nFor verb '{verb}', the association total should be 100: {total}")


#### Application to my data ####

print(python_input_df_py)

test_parse1 = python_input_df_py.at[0, 'text_cleaned']
test_parse1_output = parse_string(test_parse1)

test_parse1_output

# Apply parse_string function to each row in 'text_cleaned' column
python_input_df_py['dep_parsed'] = python_input_df_py['text_cleaned'].apply(parse_string)

python_output_df_py = python_input_df_py



