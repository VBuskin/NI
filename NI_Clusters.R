
# Cluster analysis --------------------------------------------------------

# Load required scripts

source("NI_Vector_spaces.R")

# K-means -----------------------------------------------------------------

# Preparation
set.seed(123) # For reproducibility

# Run this prior to running the clustering algorithms

remove_duplicates_to_df <- function(similarity_matrix) {

  # Combine embeddings and lemmas into a data frame
  embedding_df <- data.frame(similarity_matrix, Lemma = unique(NI_data$lemma))
  
  # Remove duplicate rows based on the embeddings
  embedding_df_unique <<- embedding_df[!duplicated(embedding_df[, 1:ncol(similarity_matrix)]), ]
  
  # Separate back into embeddings and lemmas
  embedding_NI_unique <- as.matrix(embedding_df_unique[, 1:ncol(similarity_matrix)])
  
  # Final df
  cluster_data <- embedding_NI_unique
  
  return(cluster_data)

}

GB_sim_mat <- remove_duplicates_to_df(GB_sim_mat)

SING_sim_mat <- remove_duplicates_to_df(SING_sim_mat)

#NI_lemmas_unique <- embedding_df_unique$Lemma

# K-means

data_for_clustering <- GB_sim_mat

data_for_clustering <- SING_sim_mat

kmeans_result <- kmeans(data_for_clustering, centers = 2) # or

pam_result <- pam(embedding_NI_unique, k = 2) # which is more robust to outliers

# Visualisation and optimal number of clusters

# Identify optimal number of clusters

optimal_clusters_k_means_lem <- fviz_nbclust(data_for_clustering, FUNcluster = kmeans, method = "silhouette") + theme_classic() + ggtitle("optimal numbers of clusters - kmeans")

# Fit k-means model
kmeans_result <- kmeans(data_for_clustering, centers = 2) # or
pam_result <- pam(embedding_NI_unique, k = 2) # more robust to outliers

# Print cluster assignments
print(kmeans_result$cluster)

# Plot them

fviz_cluster(list(data=data_for_clustering, cluster=kmeans_result$cluster), 
             ellipse.type="norm", geom="point", stand=FALSE, palette="jco", ggtheme=theme_classic())



# Dimensionality reduction ------------------------------------------------

# Requires running remove_duplicates_to_df() and the embedding_df_unique$Lemma object

reduce_dim <- function(cluster_data) {

# Perform t-SNE to reduce dimensions to 2
tsne_result <- Rtsne(cluster_data, dims = 2)

# Create a data frame with the 2D coordinates and lemma names
df_tsne <- data.frame(tsne_result$Y, Lemma = embedding_df_unique$Lemma)

df_tsne$Cluster <- as.factor(kmeans_result$cluster)

return(df_tsne)

}

GB_tsne <- reduce_dim(data_for_clustering)

SING_tsne <- reduce_dim(data_for_clustering)

#SING_tsne <- reduce_dim(data_for_clustering)

# Similarity + clusters plot

ggplot(GB_tsne, aes(x = X1, y = X2, color = Cluster, label = Lemma)) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(vjust = 1.5, hjust = 0.5) +
  ggtitle("Visualization of Lemmas in the ICE-GB Vector Space") +
  theme_minimal() +
  labs(subtitle = "Reduced to two dimensions using t-Distributed Stochastic Neighbor Embedding (t-SNE)")
#df_tsne[df_tsne$Cluster == 2,]$Lemma



ggplot(SING_tsne, aes(x = X1, y = X2, color = Cluster, label = Lemma)) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(vjust = 1.5, hjust = 0.5) +
  ggtitle("Visualization of Lemmas in the ICE-SING Vector Space") +
  theme_minimal() +
  labs(subtitle = "Reduced to two dimensions using t-Distributed Stochastic Neighbor Embedding (t-SNE)")



# Compare similarity matrices ---------------------------------------------

# Make sure to only consider the common set of lemmas
common_lemmas <- intersect(GB_tsne$Lemma, SING_tsne$Lemma)

# Subset the matrices to the common words
GB_tsne_common <- GB_tsne %>% dplyr::filter(Lemma %in% common_lemmas)
SING_tsne_common <- SING_tsne %>% dplyr::filter(Lemma %in% common_lemmas)

# Ensure the order of lemmas is the same in both dataframes
GB_tsne_common <- GB_tsne_common[match(common_lemmas, GB_tsne_common$Lemma), ]
SING_tsne_common <- SING_tsne_common[match(common_lemmas, SING_tsne_common$Lemma), ]


# Extract t-SNE coordinates
GB_coords <- as.matrix(GB_tsne_common[, c("X1", "X2")])
SING_coords <- as.matrix(SING_tsne_common[, c("X1", "X2")])

# Frobenius Norm
frobenius_norm <- function(mat1, mat2) {
  sqrt(sum((mat1 - mat2)^2))
}

frobenius_dist <- frobenius_norm(GB_coords, SING_coords)
print(frobenius_dist)

# Correlation
correlation <- cor(as.vector(GB_coords), as.vector(SING_coords))
print(correlation)

# Plot GB t-SNE
ggplot(GB_tsne_common, aes(x = X1, y = X2, color = as.factor(Cluster))) +
  geom_point() +
  ggtitle("GB t-SNE Plot")

# Plot SING t-SNE
ggplot(SING_tsne_common, aes(x = X1, y = X2, color = as.factor(Cluster))) +
  geom_point() +
  ggtitle("SING t-SNE Plot")

# Try to install library("vegan") after disabling your Conda environment

# Function to perform Procrustes analysis using MASS package
procrustes_mass <- function(X, Y) {
  # Perform Procrustes analysis
  procrustes_result <- MASS::procrustes(X, Y)
  
  # Procrustes distance
  procrustes_distance <- procrustes_result$ss
  return(procrustes_distance)
}

# Compute Procrustes distance
procrustes_distance <- procrustes_mass(GB_coords, SING_coords)
print(paste("Procrustes Distance: ", procrustes_distance))

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

