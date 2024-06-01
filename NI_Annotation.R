
# Prepare files for annotation --------------------------------------------

# Load libraries

source("NI_Libraries.R")

## Read in annotated files

path_annotated <- list.files("../Null Instantiation/Hits/Annotated/", full.names = TRUE)


## Find all files which are not annotated

elements <- str_extract(path_annotated, "Hits\\/Annotated\\/\\/\\w*[^A].xlsx")


## Drop NAs

new_verbs_paths <- elements[!is.na(elements)]


## Only read in those files I'm interested in

new_verbs_list <- lapply(new_verbs_paths, function(x){
  x <- read_excel(x)
})


## Convert to large df

new_verbs_df <- data.table::rbindlist(new_verbs_list, fill = TRUE)


## Join the new verbs df with the summary df

NI_data_S2 <- full_join(new_verbs_df, NI_data_S) %>% 
  arrange(keyword)


## Write the df to a new file

write_xlsx(NI_data_S2, "../Null Instantiation/Hits/Summaries/Summary_25_04_2024_joined.xlsx")

