
# Model data --------------------------------------------------------------

# Add case list

NI_data_full <- as_tibble(NI_data_variable)

NI_data_full %>% 
  mutate(Case = 1:nrow(NI_data_full)) %>% 
  relocate(Case) -> NI_final_df

# Pre-model data (keep updated)

#saveRDS(NI_final_df, "Vector_spaces/NI_final_df.RDS") # 25-05-2024

#NI_data_final <- as_tibble(readRDS("Vector_spaces/NI_final_df.RDS"))

# Model data for training and testing

model_data <- NI_data_variable |> 
  dplyr::select(Object_FE_Realisation, # Response
                lemma,
                Telicity,
                Object_FE_Animacy,
                concreteness,
                selec_strength,
                Cluster_vec,
                Cluster_clara,
                frame,
                Object_Definiteness,
                Text_category) %>% 
                as_tibble()


# Set reference level(s)

model_data$Object_FE_Realisation <- as.factor(model_data$Object_FE_Realisation)


