
# Fit additional distributional semantic models here ---------------------------------

# Load scripts

source("NI_Vector_spaces.R")



# Load verb list ----------------------------------------------------------

trans_verbs <- as_tibble(read.csv("R_data/trans_verbs.csv", header = TRUE, sep = ","))


## Remove irrelevant columns
trans_verbs %>% 
  dplyr::select(-X, -X.1, -X.2) -> trans_verbs

## Extract the other categories (Verb-type, Verb-group)
trans_verbs %>%
  extract(Levin_classes, into = c("Verb_type", "Verb_group"),
          regex = "^(.*?)\\((.*?)\\)", remove = FALSE) %>% 
    dplyr::select(-Levin_classes) -> verb_classes_df

## Decide which NI verbs to use

# Lemmas from the literature
NI_Verbs_full <- unique(data_raw$lemma) # 227

included_verbs <- NI_Verbs_full[NI_Verbs_full %in% verb_classes_df$verb] # 109

library("Hmisc")
missing_verbs <- NI_Verbs_full[NI_Verbs_full %nin% included_verbs]

all_verbs <- c(verb_classes_df$verb, missing_verbs)

# Confirmed variable lemmas
NI_Verbs_confirmed <- as.character(unique(NI_data_variable$lemma)) # 121

NI_Verbs_list[NI_Verbs_list %in% verb_classes_df$verb] # 64

# Add missing verbs to the df

# Get embeddings -----------------------------------------------------------------

# Get embeddings for all transitive verbs
ICE_GB_verbs <- vector_space(ICE_GB_lemmatised, all_verbs)

## Need functions from NI_Clusters.R

### Remove duplicates
ICE_GB_verbs <- remove_duplicates_to_df(ICE_GB_verbs, all_verbs)

### Reduce dimensions

# Perform t-SNE to reduce dimensions to 2
tsne_result <- Rtsne(ICE_GB_verbs, dims = 2)

# Create a data frame with the 2D coordinates and lemma names
df_tsne <- data.frame(tsne_result$Y, Lemma = embedding_df_unique$Lemma)

# Create column which defines the availability of NI

df_tsne[df_tsne$Lemma %in% NI_Verbs_list,]

df_tsne$NI_indicator <- ifelse(df_tsne$Lemma %in% NI_Verbs_list, "NI", "non-NI")


# Visualise

ggplot(df_tsne, aes(x = X1, y = X2, color = NI_indicator, label = Lemma)) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(vjust = 1.5, hjust = 0.5) +
  ggtitle("Visualization of Lemmas in the ICE-GB Vector Space") +
  theme_minimal() +
  labs(subtitle = "Reduced to two dimensions using t-Distributed Stochastic Neighbor Embedding (t-SNE)")


ggplot(df_tsne[df_tsne$NI_indicator == "NI",], aes(x = X1, y = X2, label = Lemma)) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(vjust = 1.5, hjust = 0.5) +
  ggtitle("Visualization of Lemmas in the ICE-GB Vector Space") +
  theme_minimal() +
  labs(subtitle = "Reduced to two dimensions using t-Distributed Stochastic Neighbor Embedding (t-SNE)")

# Confirms what I saw before. 


# Kolmogorov-Smirnov test

ks_test_x1 <- ks.test(df_tsne$X1[df_tsne$NI_indicator == "NI"],
                   df_tsne$X1[df_tsne$NI_indicator == "non-NI"]) # no difference with respect to X1
ks_test_x2 <- ks.test(df_tsne$X2[df_tsne$NI_indicator == "NI"],
                   df_tsne$X2[df_tsne$NI_indicator == "non-NI"]) # There is a significant difference between NI and non_NI verbs with respect to how they behave on X2 axis



# K-means -----------------------------------------------------------------

# Can clustering algorithms capture their distinct behaviour?

optimal_clusters_k_means_lem <- fviz_nbclust(ICE_GB_verbs, FUNcluster = cluster::pam, method = "silhouette") + theme_classic() + ggtitle("optimal numbers of clusters - kmeans")

# Write function that plots the number of NI verbs captured against the number of clusters defined
# Goal: The cluster should isolate NI verbs as much as possible/minimise variability (use entropy!)


# KL/relative entropy divergence

library("philentropy")

# Extract X1 values for NI and non-NI verbs
X1_NI <- df_tsne$X1[df_tsne$NI_indicator == "NI"]
X1_nonNI <- df_tsne$X1[df_tsne$NI_indicator == "non-NI"]

# Remove zero values from X1_NI and X1_nonNI
X1_NI <- X1_NI[X1_NI != 0]
X1_nonNI <- X1_nonNI[X1_nonNI != 0]

# Randomly sample from X1_nonNI to create a sample of the same size as X1_NI
set.seed(123)  # for reproducibility
X1_nonNI_sample <- sample(X1_nonNI, size = length(X1_NI), replace = FALSE)

# Function to normalize to a probability distribution
normalize_distribution <- function(x) {
  x_normalized <- x / sum(x)
  return(x_normalized)
}

# Normalize distributions
X1_NI_norm <- normalize_distribution(X1_NI)
X1_nonNI_sample_norm <- normalize_distribution(X1_nonNI_sample)

# Compute KL divergence
kl_divergence <- sum(X1_NI_norm * log2(X1_NI_norm / X1_nonNI_sample_norm), na.rm = TRUE)
