
# Model data --------------------------------------------------------------

# Add case list

add_case <- function(data) {
  # Convert to tiblle
  data <- as_tibble(data)
  # Add new column with case numbers corresponding to the row numbers
  data %>% 
    mutate(Case = 1:nrow(data)) %>% 
    # Move to the front of the df
    relocate(Case) %>% 
    # Make sure it's a tibble
    as_tibble() -> data_cases
  
  return(data_cases)

}

NI_data_parsed <- add_case(NI_data_parsed)

# Pre-model data (keep updated)

#saveRDS(NI_final_df, "Vector_spaces/NI_final_df.RDS") # 25-05-2024

#NI_data_final <- as_tibble(readRDS("Vector_spaces/NI_final_df.RDS"))

# Model data for training and testing

convert_to_model_data <- function(data, ref_level) {
  # Define format of the model data
  model_data <- data %>%  
    dplyr::select(Object_FE_Realisation, # Response
                  lemma,
                  Telicity,
                  Object_FE_Animacy,
                  #concreteness,
                  #selec_strength,
                  #Cluster_vec,
                  #Cluster_clara,
                  frame,
                  Object_Definiteness,
                  Text_category,
                  sense) %>% 
                  as_tibble()
  # Convert response to factor
  model_data$Object_FE_Realisation <- as.factor(model_data$Object_FE_Realisation)
  # Define response ref level
  model_data$Object_FE_Realisation <- relevel(model_data$Object_FE_Realisation, ref = ref_level)
  # End
  return(model_data)
}

model_data <- convert_to_model_data(NI_data_parsed, ref_level = "overt")

