

# XGB MODEL ---------------------------------------------------------------


# random stuff

# Keep the following pre-model code the same for every model script. The model data preparation should be done on a script-by-script basis.

# Load processed data file
source("NI_Loading.R")

# Load model data
source("NI_Model_Data.R") # NI_data_variable

# Reassign data

model_data_xgb <- model_data

# Optional: Read in fully processed file

#model_data_xgb <- readRDS("Vector_spaces/model_data_xgb.RDS")

#saveRDS(model_data_xgb, "Vector_spaces/model_data_xgb.RDS") # 25-05-2024


# Preparation -------------------------------------------------------------

# Optional step
# model_data_xgb <- model_data_xgb %>% drop_na()

# Remove response variable from X_train
X <- subset(model_data_xgb, select = -Object_FE_Realisation)

# Response variable
y <- as.numeric(model_data_xgb$Object_FE_Realisation) - 1

# Columns to convert to numeric
columns_to_convert <- c("Telicity", "Text_category", "lemma", "frame", "Object_Definiteness", "Object_FE_Animacy", "Cluster_vec", "Cluster_clara")

# Loop through each column and convert to numeric
for (col in columns_to_convert) {
  X[[col]] <- as.integer(X[[col]])
}

# Convert df to matrix
X <- as.matrix(X)

# Define train and test data (numeric)
set.seed(123)  # For reproducibility

# Partition into train and test data
index <- createDataPartition(y, times = 1, p = 0.7, list = FALSE)
# Training data
X_train <- X[index, ]
y_train <- y[index]
# Test data
X_test <- X[-index, ]
y_test <- y[-index]

# Fit model

xgb_model <- xgboost(data = X_train, label = y_train, missing = NA, nrounds = 50, objective = "binary:logistic") # or rather logitraw?

## Plot an individual tree
#xgb.plot.tree(model = xgb_model, trees = 0, show_node_id = TRUE)


# Evaluation --------------------------------------------------------------


# Create a DALEX explainer object for your model


explainer_xgb <- DALEX::explain(model = xgb_model, 
                                data = X_test, 
                                y = y_test,
                                label = "XGBoost Model",
                                type = "classification")


# Model performance (really good!)

eva_xgb <- DALEX::model_performance(explainer_xgb)

# Might wanna refit with different data samples
eva_xgb$measures$recall
eva_xgb$measures$precision
eva_xgb$measures$f1
eva_xgb$measures$accuracy
eva_xgb$measures$auc


# Bootstrap metrics -------------------------------------------------------


## Bootstrap-estimate performance metrics of the XGB model

XGB_boot_metrics <- function(data, indices) {
  
  # Bootstrapping sample
  d <- data[indices, ]
  
  # Remove response variable from X_train
  features <- d[,-1]
  
  # Response variable
  response <- as.integer(as.factor(d$Object_FE_Realisation)) - 1
  
  # Convert columns to proper numeric integers
  columns_to_convert <- c("Telicity", "Text_category", "lemma", "frame", "Object_Definiteness", "Object_FE_Animacy", "Cluster_vec", "Cluster_clara")
  
  # Loop through each column and convert to numeric
  for (col in columns_to_convert) {
    features[[col]] <- as.integer(features[[col]])
  }
  
  # Split the data into features and labels
  index <- createDataPartition(response, times = 1, p = 0.7, list = FALSE)
  X_train <- features[index, ]
  y_train <- response[index]
  X_test <- features[-index, ]
  y_test <- response[-index]
  
  # Disable messages
  flog.threshold(ERROR)
  
  # Fit the xgboost model
  xgb_model_i <- xgboost(data = as.matrix(X_train), label = y_train, missing = NA, nrounds = 50, objective = "binary:logistic", verbose = 0)
  
  # Create explainer object
  explainer <- DALEX::explain(model = xgb_model_i, 
                              data = as.matrix(X_test), 
                              y = y_test,
                              label = "XGBoost Model",
                              type = "classification")
  
  # Reset the logger threshold to INFO if needed
  flog.threshold(INFO)
  
  # Model performance
  eva <- DALEX::model_performance(explainer)
  
  # Desired metric
  metric <- eva$measures$recall
  
  # End
  return(metric)
}


# Possible evaluation metrics
#eva_xgb$measures$recall
#eva_xgb$measures$precision
#eva_xgb$measures$f1
#eva_xgb$measures$accuracy
#eva_xgb$measures$auc

# Run bootstrapping procedure
boot_results <- boot(data = model_data_xgb, statistic = XGB_boot_metrics, R = 500)

# Calculate 95% confidence intervals (first element only)
ci <- boot.ci(boot_results, type = "basic", conf = 0.95)

# Extract the bootstrap statistics
bootstrap_stats <- boot_results$t

# Print the confidence intervals
print(ci)

# Extract the bootstrap statistics
bootstrap_stats <- boot_results$t

# Compute the basic 95% confidence intervals for each statistic
ci <- apply(bootstrap_stats, 2, function(x) quantile(x, c(0.025, 0.975)))

# Print the confidence intervals
print(ci)

# Convert the confidence intervals to a tibble
ci_tibble <- tibble(
  original = boot_results$t0,
  lower = ci[1, ],
  upper = ci[2, ]
)

# Assign the results to separate dfs, but not very elegant
ci_acc <- ci_tibble 
ci_auc <- ci_tibble 
ci_f1 <- ci_tibble
ci_precision <- ci_tibble
ci_recall <- ci_tibble


# Confidence intervals ----------------------------------------------------

# Define the model fitting and prediction function

XGB_confint <- function(data, indices) {
  
  # Bootstrapping sample
  d <- data[indices, ]
  
  # Remove response variable from X_train
  features <- d[,-1]
  
  # Response variable
  response <- as.numeric(d$Object_FE_Realisation) - 1
  
  # Convert columns of interest to proper numeric integers
  columns_to_convert <- c("Telicity", "Text_category", "lemma", "frame", "Object_Definiteness", "Object_FE_Animacy", "Cluster_vec", "Cluster_clara")
  
  # Loop through each column and convert to numeric
  for (col in columns_to_convert) {
    features[[col]] <- as.integer(features[[col]])
  }
  
  # Split the data into features and labels
  index <- createDataPartition(response, times = 1, p = 0.7, list = FALSE)
  X_train <- features[index, ]
  y_train <- response[index]
  X_test <- features[-index, ]
  y_test <- response[-index]
  
  # Fit the xgboost model
  xgb_model_i <- xgboost(data = as.matrix(X_train), label = y_train, missing = NA, nrounds = 50, objective = "binary:logistic", verbose = 0)
  
  # Get probability predictions
  predictions <- predict(xgb_model_i, as.matrix(X_test))
  
  # Create a data frame with the predictions for a feature of interest
  predictions_df <- data.frame(predictions = predictions, Lemma = X_test$lemma)
  
  # Compute mean predictions by concreteness
  means_df <- aggregate(predictions ~ Lemma, data = predictions_df, FUN = mean)
  
  # Store statistic of interest
  yhat <- means_df$predictions[1:115]
  
  # Function return
  return(yhat)
}

# Combine X_test and y_test
#combined_test_df <- cbind(X_test, y_test)

# Run bootstrapping procedure
boot_results <- boot(data = model_data, statistic = XGB_confint, R = 500)

# Calculate 95% confidence intervals (first element only)
ci <- boot.ci(boot_results, type = "basic", conf = 0.95)

# Extract the bootstrap statistics
bootstrap_stats <- boot_results$t

# Print the confidence intervals
print(ci)

# Extract the bootstrap statistics
bootstrap_stats <- boot_results$t

# Compute the basic 95% confidence intervals for each statistic
ci <- apply(bootstrap_stats, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))

# Print the confidence intervals
print(ci)

# Convert the confidence intervals to a tibble
ci_tibble <- tibble(
  original = boot_results$t0,
  lower = ci[1, ],
  upper = ci[2, ]
)

concreteness_ci <- ci_tibble
concreteness_ci$concreteness <- unique(X_test$concreteness)

concreteness_mean_ci <- ci_tibble

lemmas_ci <- ci_tibble

# LEMMAS: Lookup table

# Unique lemma labels
lemma_labels <- unique(model_data$lemma)

# Create a lookup table mapping numeric values to lemma labels
lemma_lookup <- setNames(lemma_labels, as.numeric(lemma_labels))

# Example numeric values
numeric_values <- 1:nrow(ci_tibble)

# Convert numeric values back to lemma labels using the lookup table
lemma_labels_converted <- lemma_lookup[numeric_values]

# Create a new column with correct lemma labels
lemmas_ci$lemma <- lemma_lookup[1:nrow(ci_tibble)]

# Save results

saveRDS(lemmas_ci, "../Null Instantiation/Vector_spaces/lemmas_ci.Rds")

lemmas_ci <- readRDS("../Null Instantiation/Vector_spaces/lemmas_ci.Rds")
