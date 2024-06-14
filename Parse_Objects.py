# Based on Glass (2021) and Resnik (1993, 1996)

import spacy
import csv
import os
import sys
from spacy.lang.en.examples import sentences
from collections import defaultdict
import math
import nltk
from nltk.corpus import wordnet as wn
from nltk.stem import WordNetLemmatizer
import operator
from nltk.tokenize import sent_tokenize

import re
import torch
#from spacy.lang.en.examples import sentences
nlp = spacy.load('en_core_web_sm')
import codecs
import glob
from scipy import stats
import numpy as np
import pandas as pd


import allennlp_models
from allennlp.predictors.predictor import Predictor
predictor = Predictor.from_path("https://storage.googleapis.com/allennlp-public-models/bert-base-srl-2020.03.24.tar.gz")



#predictor = Predictor.from_path("https://s3-us-west-2.amazonaws.com/allennlp/models/srl-model-2018.05.25.tar.gz")
# Originally, in 2018, I used this semantic role labeling tool; however by 2020 it is obsolete.
# so now I have moved to the new one (BERT).


# Let's try it without AllenNLP

predictor.predict(
  sentence="Did Uriah honestly think he could beat the game in under three hours?"
)



