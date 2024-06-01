
# Load processed data file
source("NI_Loading.R")

# Define data

model_data_xgb <- as_tibble(model_data)

#saveRDS(model_data_xgb, "Vector_spaces/model_data_xgb.RDS") # 25-05-2024

prop.table(table(model_data_xgb$Object_FE_Realisation))

unique(model_data_xgb$lemma)


model_data_xgb <- readRDS("Vector_spaces/model_data_xgb.RDS")

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


X <- as.matrix(X)


# Define train and test data (numeric)
set.seed(123)  # For reproducibility

index <- createDataPartition(y, times = 1, p = 0.7, list = FALSE)
X_train <- X[index, ]
y_train <- y[index]
X_test <- X[-index, ]
y_test <- y[-index]


# Fit model

xgb_model <- xgboost(data = X_train, label = y_train, missing = NA, nrounds = 50, objective = "binary:logistic") # or rather logitraw?

## Plot tree
xgb.plot.tree(model = xgb_model, trees = 0, show_node_id = TRUE)



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