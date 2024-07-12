

# Part I: NLP stuff ----------------------

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


test_data <- NI_data_parsed[1:50,] %>% dplyr::select(text, keyword, lemma)

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

python_input_df <- extract_sentences(test_data)

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

dep_df <- py$python_output_df_py

dep_df <- as_tibble(dep_df)

# View
as_tibble(dep_df) %>% View()

# Identify those rows where there is a 'dobj' relationship

dep_df %>% 
  dplyr::filter(grepl("dobj", dep_parsed)) %>% 
  View()
  
# The 'dobj' relationship must be found AFTER the keyword to avoid multiple counts

find_dobj_after_verb <- function(df) {
  df %>%
    mutate(
      # Find position of the keyword in dep_parsed
      keyword_position = map2_chr(keyword, dep_parsed, ~ {
        keyword_match <- str_locate(.y, .x)
        if (!is.na(keyword_match[1, "start"])) {
          return(as.character(keyword_match[1, "start"]))
        } else {
          return(NA_character_)
        }
      }),
      
      # Check if 'dobj' appears after the keyword
      has_dobj_after_keyword = map2_lgl(keyword_position, dep_parsed, ~ {
        if (!is.na(.x)) {
          dobj_match <- str_locate(.y, "dobj")
          return(!is.na(dobj_match[1, "start"]) && dobj_match[1, "start"] > as.integer(.x))
        } else {
          return(FALSE)
        }
      })
    ) %>%
    filter(has_dobj_after_keyword)
}

# Apply the function to your dataframe
subset_df <- find_dobj_after_verb(dep_df)

# Retrieve the word

find_dobj_word <- function(df) {
  df %>%
    mutate(
      dobj_word = map_chr(dep_parsed, ~ {
        match <- str_match(.x, "\\b(\\w+)\\(dobj;")
        if (!is.na(match[1, 2])) {
          return(match[1, 2])
        } else {
          return(NA_character_)
        }
      })
    )
}

matched_df <- find_dobj_word(dep_df) # WORKS


# Next step, get WordNet classes of the objects

# Get the df back to Python

matched_df

matched_df_py <- pd$DataFrame(matched_df)

# Convert R data frame to Python pandas DataFrame

py$matched_df_py <- matched_df_py


# Get it back to R

matched_results <- py$matched_df_py

matched_results <- as_tibble(matched_results)

write_xlsx(matched_results, "R_data/parsed_sample.xlsx")

matched_results$noun_class3 # list

