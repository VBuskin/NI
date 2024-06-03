# Table of contents

## Data wrangling and annotation
1. `NI_Libraries`: Always run this first!
2. `NI_Annotation`: Necessary for identifying the next files to be annotated and exporting them
3. `NI_Loading`: All filtering operations on my raw df; use this for further analysis
4. `NI_Variable_Lemmas`: The invariable lemmas are already included in 3, but run this if you want to update them.
5. `NI_Corpora`: Load the corpus object(s) and perform quanteda lemmatisation, if desired.

## Unsupervised learning
6. `NI_Vector_spaces`: Construct the corpus object and fit a DSM with the desired parameters; don't use 5 if you use this.
7. `NI_Clusters`: All things cluster analysis – requires 6.


## Supervised learning
8. `NI_XGB`: Extreme Gradient Boosting (XGB)

## Misc
8. `NI_TI`: My particular implementation of a "Transitivity Index"
