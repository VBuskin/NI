
# Random forests ----------------------------------------------------------

# Keep the following pre-model code the same for every model script. The model data preparation should be done on a script-by-script basis.

# Load processed data file
source("NI_Loading.R")

# Load model data
source("NI_Model_Data.R") # NI_data_variable

# Reassign data

model_data_rf <- model_data


# RF MODEL ----------------------------------------------------------------

# RF can't handle NAs
model_data_rf <- model_data %>% drop_na()

# Convert to a numeric response variable
#model_data_rf$Object_FE_Realisation <- ifelse(model_data_rf$Object_FE_Realisation == "null", 1, 0)

# Set seed for reproducibility
set.seed(123)

# Generate random indices for the training set
train_indices <- sample(nrow(model_data_rf), size = round(0.7 * nrow(model_data_rf)), replace = FALSE)

# Subset the data frame into training and test sets based on the indices
train_data <- model_data_rf[train_indices, ]
test_data <- model_data_rf[-train_indices, ]
train_labels <- model_data_rf$Object_FE_Realisation[train_indices]

# Fit model

model.rf <- ranger(Object_FE_Realisation ~ ., prob = TRUE, train_data, importance = "permutation", classification = TRUE)

imp <- sort(ranger::importance(model.rf))


# DALEX explainer

explainer_rf <- DALEX::explain(model = model.rf, 
                               data = test_data, 
                               y = test_data$Object_FE_Realisation,
                               label = "RF",
                               type = "classification")

eva_rf <- DALEX::model_performance(explainer_rf)

#profile <- DALEX::predict_profile(explainer.rf, new_observation = test_data)

#vip_rf <- DALEX::model_parts(explainer = explainer.rf,  B = 50, N = NULL)

#plot(vip_rf)

# Compute partial dependence for a single feature ('feature_name')
#pdp <- DALEX::model_profile(explainer = explainer.rf, type = "partial", variables = "Telicity", response = "1")

#plot(pdp)

