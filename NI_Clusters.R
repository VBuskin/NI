
# Cluster analysis --------------------------------------------------------

# Load required scripts

source("NI_Vector_spaces.R")


# Old approaches ----------------------------------------------------------

## K-means -----------------------------------------------------------------

# Preparation
set.seed(123) # For reproducibility

# Run this prior to running the clustering algorithms

remove_duplicates_to_df <- function(similarity_matrix, words) {

  # Combine embeddings and lemmas into a data frame
  embedding_df <- data.frame(similarity_matrix, Lemma = words)
  
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

optimal_clusters_k_means_lem <- fviz_nbclust(data_for_clustering, FUNcluster = kmeans, method = "silhouette") + theme_classic() + ggtitle("optimal numbers of clusters - kmeans") # nolint

# Fit k-means model
kmeans_result <- kmeans(data_for_clustering, centers = 2) # or
pam_result <- pam(embedding_NI_unique, k = 2) # more robust to outliers

# Print cluster assignments
print(kmeans_result$cluster)

# Plot them

fviz_cluster(list(data=data_for_clustering, cluster=kmeans_result$cluster), 
             ellipse.type="norm", geom="point", stand=FALSE, palette="jco", ggtheme=theme_classic())



## Dimensionality reduction ------------------------------------------------

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



## Compare similarity matrices ---------------------------------------------

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


# Function to compute absolute differences for each lemma and return as a dataframe
compute_absolute_differences_df <- function(GB_data, SING_data) {
  # Initialize dataframe to store results
  absolute_diff_df <- data.frame(lemma = character(), absolute_difference = numeric(), stringsAsFactors = FALSE)
  
  # Get unique lemmas
  unique_lemmas <- unique(GB_data$Lemma)
  
  # Iterate through each lemma
  for (lemma in unique_lemmas) {
    # Extract coordinates for the current lemma in GB data
    GB_coords <- GB_data[GB_data$Lemma == lemma, c("X1", "X2")]
    
    # Extract coordinates for the current lemma in SING data
    SING_coords <- SING_data[SING_data$Lemma == lemma, c("X1", "X2")]
    
    # Compute absolute difference for the current lemma
    absolute_diff <- sqrt(rowSums((GB_coords - SING_coords)^2))
    
    # Compute mean absolute difference for the current lemma
    mean_absolute_diff <- mean(absolute_diff)
    
    # Append results to dataframe
    absolute_diff_df <- rbind(absolute_diff_df, data.frame(lemma = lemma, absolute_difference = mean_absolute_diff))
  }
  
  return(absolute_diff_df)
}

# Compute absolute differences for each lemma and store as dataframe
absolute_diff_df <- compute_absolute_differences_df(GB_tsne_common, SING_tsne_common)

# Print dataframe
print(absolute_diff_df)

absolute_diff_df %>% 
  arrange(absolute_difference)




## Inspection --------------------------------------------------------------


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




# New approaches ----------------------------------------------------------

## Feature matrix ----------------------------------------------------------

# I want to perform dimensionality reduction/clustering on my data set. There are two options:
# (a) perform it on the full data set with all variables
# (b) perform it on lemma data set with lemma-specific variables


## Let's start with (a) ----------------------------------------------------

### MCA ---------------------------------------------------------------------



## Load libraries

library(anacor)
library(ca)
library(FactoMineR)
library(Gifi)
library(lsa)
library(NMF)
library(svs)

## Remove rows with missing values

data <- NI_data_variable

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
                Text_category) %>% 
  drop_na() %>% 
  as_tibble()

## Let's try MCA

# Get dimensions

tmp_NI <- apply(model_data, 2, function(j) length(unique(j)))

ndm_NI <- sum(tmp_NI) - length(tmp_NI)

# Perform MCA

mca_NI <- mjca(model_data, nd = ndm_NI, lambda = "indicator")

summary(mca_NI) # damn, I'd need like 53 dimensions to only account for 50% of the variation in the data 

## Visualise the results
plot(mca_met, dim = c(1, 2), collabels = "level") # only account for 2.4%, absolutely useless

### SCA --------------------------------------------------------------------

# Load libraries

library(cluster)
library(dendextend)
library(lattice)
library(mixtools)
library(pvclust)
library(svs)

# Cross-tabulate frequencies

data_tab <- xtabs(~ ., data = model_data)

# Alternatively ...

tmp1 <- cbind(Freq = 1, model_data)

dat2 <- aggregate(Freq ~ ., data = tmp1, sum)

# Two-way frequency table

## Show omission patterns by lemma

#tab1 <- xtabs(Freq ~ Object_FE_Realisation + Telicity, data = dat2)

data_tab <- xtabs(~ Text_category + lemma, data = model_data)

# Visualise

mosaicplot(data_tab, shade = TRUE)

# Perform SCA

ndm <- min(nrow(data_tab), ncol(data_tab)) - 1

sca2 <- CA(data_tab, ncp = ndm, graph = FALSE)

summary(sca2) # Check dimensions

# Plot SCA (works!)

plot(sca2, axes = c(1, 2))

plot(sca2, axes = c(1, 3))

plot(sca2, axes = c(1, 4))

plot(sca2, axes = c(2, 3))

plot(sca2, axes = c(2, 4))



## (b) ---------------------------------------------------------------------

### PCA ---------------------------------------------------------------------

# Load libraries
library(corrplot)
library(lattice)
library(psych)
library(GPArotation)
library(Gifi)
library(FactoMineR)

# Clean data and remove categorical variables
psy_data <- readRDS("R_data/psy_full_ann_df.RDS")

psy_data2 <- psy_data[,c(-1, -5)]

psy_data3 <- psy_data2 %>% drop_na()

# Correlation matrix
cor1 <- cor(psy_data3)

# Correlation plot
corrplot(cor1, col = topo.colors(200), tl.col = "darkgrey")

# Heatmap
seq1 <- seq(-1, 1, by = 0.01)

levelplot(cor1, aspect = "fill", col.regions = topo.colors(length(seq1)),
          at = seq1, scales = list(x = list(rot = 45)),
          xlab = "", ylab = "")

# PCA
pca1 <- principal(psy_data3, nfactors = ncol(psy_data3), rotate = "none")

## Scree plot
barplot(pca1$values, main = "Scree plot", ylab = "Variances", xlab = "PC",
        names.arg = 1:length(pca1$values))
abline(h = 1, col = "blue", lty = "dotted") # 6 PCs

## Extract important PCs
pca2 <- principal(psy_data3, nfactors = 6, rotate = "none") # nfactors

## Get loadings
print(loadings(pca2))

diagram(pca2, main = NA) # very interesting

plot(pca2, labels = colnames(psy_data3), main = NA) # uninterpretable

## Get scores
head(pca2$scores, n = 5) # first 5 observations

## Visualise scores
biplot(pca2, choose = c(1, 2), 
       main = NA, pch = 20, col = c("darkgrey", "blue"))

biplot(pca2, choose = c(2, 3), 
       main = NA, pch = 20, col = c("darkgrey", "blue"))

biplot(pca2, choose = c(3, 4), 
       main = NA, pch = 20, col = c("darkgrey", "blue"))

biplot(pca2, choose = c(4, 5), 
       main = NA, pch = 20, col = c("darkgrey", "blue"))

biplot(pca2, choose = c(5, 6), 
       main = NA, pch = 20, col = c("darkgrey", "blue"))

## Transfer scores to main df

pca_scores <- as_tibble(pca2$scores)

psy_data3

psy_data_scores <- cbind(psy_data3, pca2$scores)

# Add lemma and variability

psy_lem_var <- psy_data %>% drop_na() %>% dplyr::select(variable)

psy_data_scores <- cbind(psy_data_scores, psy_lem_var)

# Check model

mod.glm <- glm(variable ~ ., data = psy_data_scores, family = "binomial")

summary(mod.glm)

### EFA ---------------------------------------------------------------------

## Without rotation
efa1 <- fa(psy_data3, nfactors = 6, rotate = "none", fm = "pa")

summary(efa1)

## Loadings
plot(efa1, labels = colnames(psy_data3), main = NA)

diagram(efa1, main = NA)

## Plot
biplot(efa1, choose = c(1, 2), main = NA,
       pch = 20, col = c("darkgrey", "blue")) # VERY interesting; DKL does its own thing

biplot(efa1, choose = c(2, 3), main = NA,
       pch = 20, col = c("darkgrey", "blue"))

biplot(efa1, choose = c(2, 4), main = NA,
       pch = 20, col = c("darkgrey", "blue"))

biplot(efa1, choose = c(5, 6), main = NA,
       pch = 20, col = c("darkgrey", "blue"))

## With rotation
efa2 <- fa(psy_data3, nfactors = 6, rotate = "Varimax", fm = "pa")

diagram(efa2, main = NA)

## Get scores (principal axes)
head(efa2$scores, n = 5)

## Plot scores and loadings
biplot(efa2, choose = c(1, 2), main = NA,
       pch = 20, col = c("darkgrey", "blue"))

biplot(efa2, choose = c(2, 3), main = NA,
       pch = 20, col = c("darkgrey", "blue"))

## Gifi (non-linear PCA) not working, computationally singular
#pca6 <- princals(as.matrix(psy_data3), ndim = 6, levels = "nominal", degrees = 3)


# MDS ---------------------------------------------------------------------

library(smacof)
library(svs)

dist_psy <- dist(psy_data3, method = "euclidean", diag = TRUE, upper = TRUE)  # Distance matrix

# Standadise

dist_psy_std <- scale(dist_psy)

mds1 <- mds(dist_psy, ndim = 2, type = "ordinal") # crashing

mds1
plot(mds1, plot.type = "Shepard", main = NA)
plot(mds1, plot.type = "confplot", plot.dim = c(2, 3), main = NA,
     xlim = c(-2, 2), ylim = c(-1, 1))


# SCOPE data --------------------------------------------------------------

# Get updated file from the SCOPE section of NI_Frequency.R
scope_df_filtered

scope_df_filtered %>%
  left_join(lemma_conc_df %>% 
              dplyr::select(lemma, frequency, Hnorm, DKL, log_frequency),
            by = c("Word" = "lemma")) %>%
  rename(
    ICE_frequency = frequency,
    ICE_Hnorm = Hnorm,
    ICE_DKL = DKL,
    ICE_log_frequency = log_frequency
  ) %>% 
  relocate(Word, Variability, ICE_frequency, ICE_Hnorm, ICE_DKL, ICE_log_frequency) -> scope_input_df

# I need to merge them here

# Remove categorical variables
scope_input_df %>% 
  dplyr::select(-Word, -Variability, -IPA, -Status) -> scope_input

# Convert logical vectors to numeric ones
convert_to_dbl <- function(df) {
  # For each column in the data frame ...
  for (col in colnames(df)) {
    # ... convert the column to a factor.
    df[[col]] <- as.numeric(df[[col]])
  }
  # Return the modified data frame
  return(df)
}

scope_input <- convert_to_dbl(scope_input)


# Handle NAs
## Impute missing values with the mean of each column
scope_input <- scope_input %>%
  mutate(across(everything(), ~ {
    # Replace NaN and infinite values with NA first
    cleaned_column <- ifelse(is.nan(.), NA, .)
    cleaned_column <- ifelse(is.infinite(cleaned_column), NA, cleaned_column)
    
    # Replace NA with the mean of the column
    ifelse(is.na(cleaned_column), mean(cleaned_column, na.rm = TRUE), cleaned_column)
  })) 

# Convert to matrix
scope_matrix <- as.matrix(scope_input) 

# Standardise
scope_std <- as.matrix(scale(scope_matrix))

# %%%%%%%%%%%%%%%%%%%%%%%%
# IMPUTE MEANS: Deal with NAs/NaNs
# Calculate column means, ignoring NAs and NaNs
col_means <- colMeans(scope_std, na.rm = TRUE)

# Function to replace NA and NaN with column mean
replace_na_nan <- function(x, mean_val) {
  ifelse(is.na(x) | is.nan(x), mean_val, x)
}
# Apply the function to each column
scope_std_clean <- apply(scope_std, 2, replace_na_nan, col_means)
# %%%%%%%%%%%%%%%%%%%%%%%%

# Final imputation
library("missMDA")

# Preparation
## Remove infinite values
scope_std[is.infinite(scope_std)] <- NA

## Remove all potential all-NA columns
all_na_cols <- colSums(is.na(scope_std)) == nrow(scope_std)
scope_std <- scope_std[, !all_na_cols]

## Remove all constant columns
constant_cols <- apply(scope_std, 2, function(x) var(x, na.rm = TRUE) == 0)
scope_std <- scope_std[, !constant_cols]

## Check for NaN values
scope_std[is.nan(scope_std)] <- NA

# Perform EM imputation
imputed_data <- imputePCA(scope_std, method = "EM")



#saveRDS(scope_std, "R_data/scope_std.RDS")

## Descriptive overview ------------------------------------------------------

# Correlation matrix
scope_cor <- cor(scope_std)

# Correlation plot
corrplot(scope_cor, col = topo.colors(200), tl.col = "darkgrey")

# Heatmap
seq1 <- seq(-1, 1, by = 0.01)

levelplot(scope_cor, aspect = "fill", col.regions = topo.colors(length(seq1)),
          at = seq1, scales = list(x = list(rot = 45)),
          xlab = "", ylab = "")


## PCA ---------------------------------------------------------------------

# PCA
pca1 <- PCA(imputed_data$completeObs, graph = FALSE)

pca2 <- principal(imputed_data$completeObs, nfactors = ncol(as.data.frame(imputed_data$completeObs)), rotate = "none") # 6 for 50%, 30 for 75%

# With principal()

# Redo with fewer dimensions
pca3 <- principal(imputed_data$completeObs, nfactors = 30, rotate = "none") # 33

## Transfer scores to main df
pca_scores <- as_tibble(pca3$scores)

pca_scope_df <- cbind(scope_input_df[,1:2], pca_scores) # Final output df; use for further analysis



## Quick model check

rf1 <- ranger(as.factor(Variability) ~ ., data = pca_scope_df[,-1], importance = "permutation")
# Note: you might wanna exclude PCs with negative importance values lol

rf2 <- ranger(as.factor(Variability) ~ ., data = efa_pca_scope_df[,-1], importance = "permutation") # interesting; the factors are actually very useful!

rf3 <- ranger(as.factor(Variability) ~ ., data = test1, importance = "permutation")



ggplot(
  enframe(
    rf3$variable.importance,
    name = "variable",
    value = "importance"
  ),
  aes(
    x = reorder(variable, importance),
    y = importance,
    fill = importance
  )
) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  ylab("Variable Importance") +
  xlab("") +
  ggtitle("Permutation Feature Importance") +
  guides(fill = "none") +
  scale_fill_gradient(low = "red", high = "blue") +
  theme_minimal()

lrm(Variability ~ . , data = pca_scope_df[,-1]) # R2 = 0.352


## EFA ---------------------------------------------------------------------

## On standardised data
efa1 <- fa(as.data.frame(imputed_data$completeObs), nfactors = 6, rotate = "none", fm = "pa")

efa1 # 4 PAs for 63%, 6 for 70%, 8 for 75%

print(loadings(efa1))

diagram(efa1, main = NA)

efa_var <- fa(as.data.frame(imputed_data$completeObs), nfactors = 6, rotate = "varimax", fm = "pa")

## Visualise

biplot(efa1, choose = c(1, 2), main = NA,
       pch = 20, col = c("darkgrey", "blue")) # don't see shit

## Get scores

efa_scores <- efa1$scores

efa_scores_rotated <- efa_var$scores

efa_pca_scope_df <- cbind(pca_scope_df, efa_scores) # Final output df; use for further analysis

efa_rot_pca_scope_df <- cbind(pca_scope_df, efa_scores_rotated)

## Quick model check

lrm(Variability ~ . , data = efa_pca_scope_df[,-1]) # R2 = 0.358, very nice, but none of the factors are significant; model R2 is only 0.006 better than the one without factors
# But they're very important in the random forest model ... I need to assess the predictive performance

lrm(Variability ~ PA1 + PA2 + PA3 + PA4 + PA5 + PA6, data = efa_pca_scope_df[,-1]) 

lrm(Variability ~ . , data = efa_rot_pca_scope_df[,-1]) # rotation doesn't improve the model at all

## Quick correlation check

corrplot(cor(efa_rot_pca_scope_df[,-c(1,2)]), col = topo.colors(200), tl.col = "darkgrey")

## yeah, the 6 PAs are perfectly correlated with the first 6 PCs

## Extreme test

test1 <- as_tibble(as.data.frame(imputed_data$completeObs[,-1]))

test1$Variability <- scope_input_df$Variability

lrm(Variability ~ . , data = test1)

rf3 <- ranger(as.factor(Variability) ~ ., data = test1, importance = "permutation") # very interesting

### Visualisation -----------------------------------------------------------

library(ggplot2)
library(reshape2)

# Assuming efa2 is your factor analysis result
loadings_matrix <- efa1$loadings
loadings_df <- as.data.frame(unclass(loadings_matrix))
loadings_melted <- melt(loadings_df)

# Factor Loading Heatmap: Create a heatmap of factor loadings. This can give you a good overview of which variables load strongly on which factors.
ggplot(loadings_melted, aes(x = variable, y = value, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1)) +
  theme_minimal() +
  theme(axis.text.y = element_blank())

# Top Loading Variables: For each factor, show only the top N (e.g., 10) variables with the highest absolute loadings.

top_loadings <- function(loadings, n = 10) {
  apply(loadings, 2, function(x) {
    sorted <- sort(abs(x), decreasing = TRUE)
    names(sorted)[1:n]
  })
}

top_vars <- top_loadings(loadings_matrix, 15) # actually very helpful

print(top_vars)

# Network Graph: Create a network graph where variables are nodes and edges represent strong factor loadings.

library(igraph)

# Create adjacency matrix
adj_matrix <- crossprod(loadings_matrix > 0.3)  # Adjust threshold as needed
diag(adj_matrix) <- 0  # Remove self-connections

# Create graph
g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)

# Plot
plot(g, vertex.size = 5, vertex.label = NA, edge.width = E(g)$weight)


# Parallel Coordinates Plot: This can be useful for seeing patterns across factors for subsets of variables.
library(GGally)

# Select a subset of variables
subset_vars <- sample(rownames(loadings_df), 50)  # Random 50 variables
subset_loadings <- loadings_df[subset_vars, ]

ggparcoord(subset_loadings, columns = 1:ncol(subset_loadings), 
           scale = "uniminmax", alphaLines = 0.3) +
  theme_minimal()

# Dimensionality Reduction of Loadings: Apply t-SNE or UMAP to the loadings matrix to create a 2D representation.

library(Rtsne)

tsne_result <- Rtsne(loadings_matrix, perplexity = 30, dims = 2)
tsne_df <- data.frame(x = tsne_result$Y[,1], y = tsne_result$Y[,2], 
                      variable = rownames(loadings_matrix))

ggplot(tsne_df, aes(x, y, label = variable)) +
  geom_text(check_overlap = TRUE, size = 3) +
  theme_minimal()

# Hierarchical Clustering of Loadings: Group variables based on their loading patterns.

hc <- hclust(dist(loadings_matrix))
plot(hc, labels = FALSE, main = "Hierarchical Clustering of Variables")

