

# Compute selectional strength and work with WordNet ----------------------

# Load libraries

source("NI_Libraries.R")
source("NI_Loading.R")


# Part I: Verb sense disambiguation ---------------------------------------



# Extract sentences -------------------------------------------------------

## Preparation: Merge the columns "pre", "keyword" and "post" into one

NI_data_parsed <- NI_data

# Plain approach

NI_data_parsed$text <- paste(NI_data_parsed$pre, NI_data_parsed$keyword, NI_data_parsed$post)

# Extraction function

extract_target_sentence <- function(text, target_word) {
  # Split the text into sentences
  sentences <- unlist(strsplit(text, "\\."))
  
  # Find the index of the sentence containing the target word
  target_sentence_index <- NULL
  for (i in seq_along(sentences)) {
    if (grepl(target_word, sentences[i], fixed = TRUE)) {
      target_sentence_index <- i
      break
    }
  }
  
  # If the target word is found, extract the target sentence
  if (!is.null(target_sentence_index)) {
    # Extract the target sentence
    extracted_sentence <- trimws(sentences[target_sentence_index])
    
    # Remove non-linguistic annotations
    extracted_sentence <- gsub("<\\s*X[^>]*>(.*?)<\\s*/\\s*X>|<\\s*O[^>]*>(.*?)<\\s*/\\s*O>|<\\s*&[^>]*>(.*?)<\\s*/\\s*&>|<(.*?)>", "", extracted_sentence, perl = TRUE)
    
    return(extracted_sentence)
  } else {
    return(NULL)
  }
}


### Application to data ------------------------------------------------


test_data <- NI_data_parsed[1:10,] %>% dplyr::select(text, keyword, lemma)

full_data <- NI_data_parsed %>% dplyr::select(text, keyword, lemma)



extract_sentences <- function(df) {
  # Set empty list environment
  python_input <- list()
  # Loop
  for (i in 1:nrow(df)) {
    # Use the sentence extraction from above
    cleaned_sent <- extract_target_sentence(df[i,]$text, df[i,]$keyword)
    # Create tibble with lemma, keyword and cleaned text
    input_df <- tibble(
      lemma = df[i,]$lemma,
      keyword = df[i,]$keyword,
      text_cleaned = cleaned_sent
    )
    # Store results of each iteration
    python_input[[i]] <- input_df
  }
  # Convert list of results into df
  python_input_df <- dplyr::bind_rows(python_input)
  # End
  return(python_input_df)
}

python_input_df <- extract_sentences(full_data)

#python_input_df <- dplyr::bind_rows(python_input)


# Transfer to Python ------------------------------------------------------

# Now let's supply this df to Python

# Check if pandas is available in Python environment
py_module_available("pandas")

pd <- import("pandas")

python_input_df_py <- pd$DataFrame(python_input_df)


# Convert R data frame to Python pandas DataFrame

#python_input_df_py <- py$pd$DataFrame(python_input_df)

py$python_input_df_py <- python_input_df_py


# After this, it should appear in the Python working environment

#py$assign("python_input_df_py", python_input_df_py)


# Read in Python data -----------------------------------------------------

#load("r_workspace.RData")

# Source the script
source_python("NI_Semantic_Annotation.py")

# Convert target df to R object
senses_df <- py$senses_df

senses_df <- as_tibble(senses_df)

# View
as_tibble(senses_df) %>% View()

# Clean up the df ---------------------------------------------------------

# Clean up the 'sense' column to remove "Synset('...')" and keep only the sense ID
senses_df$sense <- gsub("^Synset\\('(.*)'\\)$", "\\1", senses_df$sense)

senses_df %>% View()

# Save the output

#saveRDS(senses_df, "R_data/senses_df_14_06_24.RDS")

# For the selectional strength, I might need to this for the objects too.


# Merge with main df ------------------------------------------------------

NI_data_parsed$sense <- senses_df$sense
NI_data_parsed$definition <- senses_df$definition

saveRDS(NI_data_parsed, "R_data/NI_data_sem_parsed_14_06_2024.RDS")


# Part II: Identifying objects --------------------------------------------


