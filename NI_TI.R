
# Transitivity Index ------------------------------------------------------

# Load libraries

source("NI_Libraries.R")

# Requires filtered results to be stored in "Results1"

Results1$Object_FE_Realisation <- as.factor(Results1$Object_FE_Realisation)

Results1 %>% 
  group_by(lemma, Object_FE_Realisation) %>% 
  count(Object_FE_Realisation, .drop = FALSE) -> Results_c


# Manual solution (whole column)

TI <- Results_c[Results_c$Object_FE_Realisation == "overt",][,3]/Results_c[Results_c$Object_FE_Realisation == "null",][,3]


# Create final tibble

Results_TI <- tibble(Lemma = unique(Results1$lemma), TI = TI)

# This works, but the TI index is not meaningful if one of the two the frequencies of "null" and/or "overt" are 0. This results in values like "Inf", which suggests pure (in-)transitivity. How do I get rid of those lemmas?

# If a lemma has a 0 in one of the two n columns, it should be removed from the entire data frame.

# This makes sense

Results_c[1:2,1:3][3]

# Decide what to delete

Results_c %>% 
  filter(n == 0) -> Results_delete; Results_delete 

Results_delete <- Results_delete$lemma

# Decide what to keep

Results_keep_new <- unique(Results_c$lemma[-which(Results_c$lemma %in% Results_delete)])

# Filter for the ones I want to keep

Results_c %>% 
  ungroup() %>% 
  filter(lemma %in% Results_keep_new) -> Results_cleaned

# Updated transitivity table

TI_new <- Results_cleaned[Results_cleaned$Object_FE_Realisation == "null",][,3]/Results_cleaned[Results_cleaned$Object_FE_Realisation == "overt",][,3]

Results_TI_new <- tibble(Lemma = unique(Results_cleaned$lemma), TI = TI_new)

# qed


# Create data frame with all useful hits summarised in a file

write_xlsx(NI_data_A, "Hits/Summaries/Summary_12_02_24_collapsed.xlsx")

# Add new verbs to Results1

# Filter further and group data

Results2 <-  NI_data_S %>% 
  filter(Finiteness != "x", Finiteness != "NA") %>% # remove all questionable cases
  group_by(lemma) %>% 
  select(lemma, Object_FE) %>% 
  count(Object_FE) 

# Save results

write_xlsx(NI_data_A, "Hits/Annotated/Summary_04_12_23.xlsx")

