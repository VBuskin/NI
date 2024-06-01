
# Cluster analysis --------------------------------------------------------

# Load required scripts

source("NI_Vector_spaces.R")

# K-means -----------------------------------------------------------------

# Perform k-means clustering
set.seed(123) # For reproducibility

# Check for NA values in embeddings
na_indices <- which(apply(NI_w2v_mat, 1, function(x) any(is.na(x))))

# If there are NA values, remove those rows
if (length(na_indices) > 0) {
  NI_w2v_mat <- NI_w2v_mat[-na_indices, ]
  NI_lemmas <- NI_lemmas[-na_indices]
}

# Ensure no NA values
if (any(is.na(NI_w2v_mat))) {
  stop("There are still NA values in the embeddings.")
}

# Combine embeddings and lemmas into a data frame
embedding_df <- data.frame(NI_w2v_mat, Lemma = NI_lemmas)

# Remove duplicate rows based on the embeddings
embedding_df_unique <- embedding_df[!duplicated(embedding_df[, 1:ncol(embedding_NI)]), ]

# Separate back into embeddings and lemmas
embedding_NI_unique <- as.matrix(embedding_df_unique[, 1:ncol(embedding_NI)])

NI_lemmas_unique <- embedding_df_unique$Lemma

# Identify optimal number of clusters

optimal_clusters_k_means_lem <- fviz_nbclust(embedding_NI_unique, FUNcluster = kmeans, method = "silhouette") + theme_classic() + ggtitle("optimal numbers of clusters - kmeans")

# Fit k-means model
kmeans_result <- kmeans(embedding_NI_unique, centers = 2) # or
pam_result <- pam(embedding_NI_unique, k = 2) # more robust to outliers

# Print cluster assignments
print(kmeans_result$cluster)

# Plot them

fviz_cluster(list(data=embedding_NI_unique, cluster=kmeans_result$cluster), 
             ellipse.type="norm", geom="point", stand=FALSE, palette="jco", ggtheme=theme_classic())



# Dimensionality reduction ------------------------------------------------


# Perform t-SNE to reduce dimensions to 2
tsne_result <- Rtsne(embedding_NI_unique, dims = 2)

# Create a data frame with the 2D coordinates and lemma names
df_tsne <- data.frame(tsne_result$Y, Lemma = NI_lemmas_unique)
df_tsne$Cluster <- as.factor(kmeans_result$cluster)


# Rename the Cluster column
#names(NI_data_concreteness)[which(names(NI_data_concreteness) == "Cluster")] <- "Cluster_Lemma"


#colnames(df_tsne) <- c("X1", "X2", "Lemma")

# SIMILARITY PLOT + CLUSTERS!!
ggplot(df_tsne, aes(x = X1, y = X2, color = Cluster, label = Lemma)) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(vjust = 1.5, hjust = 0.5) +
  ggtitle("Visualization of Lemmas in the ICE-GB Vector Space") +
  theme_minimal() +
  labs(subtitle = "Reduced to two dimensions using t-Distributed Stochastic Neighbor Embedding (t-SNE)")
#df_tsne[df_tsne$Cluster == 2,]$Lemma



# Inspection --------------------------------------------------------------


## Relationship with concreteness?

df_tsne %>% 
  mutate(Concreteness = NI_c_ratings[match(Lemma, NI_c_ratings$lemma),]$concreteness) -> df_tsne

# Actually a significant concreteness difference between clusters!
df_tsne_test <- t.test(df_tsne[df_tsne$Cluster == 2,]$Concreteness, df_tsne[df_tsne$Cluster == 1,]$Concreteness)

library("effsize")
cohen.d(df_tsne[df_tsne$Cluster == 2,]$Concreteness, df_tsne[df_tsne$Cluster == 1,]$Concreteness)

ggplot(df_tsne, aes(x = Cluster, y = Concreteness)) +
  geom_boxplot() +
  theme_minimal()

saveRDS(df_tsne, "Vector_spaces/df_tsne.RDS")
a <- readRDS("Vector_spaces/df_tsne.RDS")

# Continue with clara

set.seed(123)
cluster_data <- dplyr::select(model_data, !(Object_FE_Realisation))

clara_result <- clara(cluster_data, k = 2, metric = "gower", correct.d = TRUE)

plot(clara_result)

clara_result


# Add cluster labels to the original data
df_with_clusters <- cbind(cluster_data, Cluster_clara = factor(clara_result$clustering))

str(df_with_clusters) # keep them for now

df_with_clusters %>% 
  dplyr::group_by(lemma) %>% 
  count(Cluster_clara) %>% 
  drop_na() %>% 
  rename(Lemma = "lemma") -> Clusters_grouped

# This requirs "Cluster" to be added to the group_by() function
#sum(Clusters_grouped$Cluster == Clusters_grouped$Cluster_clara)/nrow(Clusters_grouped) * 100

clusters_merged <- merge(df_tsne, Clusters_grouped, by = "Lemma", all.x = TRUE)


ggplot(clusters_merged, aes(x = X1, y = X2, color = Cluster, shape = Cluster_clara, label = Lemma)) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(vjust = 1.5, hjust = 0.5) +
  ggtitle("Visualization of Lemmas in the ICE-GB Vector Space") +
  theme_minimal() +
  facet_grid(~Cluster_clara) +
  labs(subtitle = "Combined with CLARA clusters based on the input feature matrix",
       color = "Vector space cluster",
       shape = "Feature space cluster")

# Group 1-1 (left side, force-dynamic, less force)
# Group 1-2 (right side, force-dynamic, more force)
# Group 2-1 (left side, cognitive-affective)
# Group 2-2 (right side, cognitive-communicative)

saveRDS(clusters_merged, "Vector_spaces/clusters_merged.RDS")

clusters_merged

clusters_merged %>% 
  dplyr::select(Lemma, Cluster, Cluster_clara) %>% 
  dplyr::filter(Cluster == "2")

# Then there's probably also register difference

## Conclusion: NI verbs form two internal clusters which partially reflect concreteness differences. One group denotes more hands-on actions, whereas the other mainly involves cognitive ones. 

