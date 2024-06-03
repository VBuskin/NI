
# (Down-)Sampling ----------------------------------------------------------------

# Load libraries
source("NI_Libraries.R")

# Load corpus
source("NI_Corpora.R")


## Run the kwic search for the item you're interested in and then return to this script (cf. NI_Corpora!)

tell_data <- tibble(kwic_NI)

tell_data <- separate_wider_delim(tell_data, cols = docname, delim = "-", names = c("Text_category", "File_number"))

### Required sample size

sample_size_required <- 500

### Proportions in the population

tell_data_prop <- table(tell_data$Text_category)/length(tell_data$Text_category)

### Sizes of the stratified sample

strat_sample_sizes <- round(sample_size_required*(tell_data_prop))

addmargins(strat_sample_sizes) # view sum of hits

### Draw the sample

tell_data$Text_category <- as.factor(tell_data$Text_category)

set.seed(2203)

tell_strat_sample <- tibble(strata(tell_data, "Text_category", strat_sample_sizes, method = "srswor"))

## Final output df

output_sample <- tibble(getdata(tell_data, tell_strat_sample)) # export this as .xlsx etc.