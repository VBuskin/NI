
# Neural networks

use_python("~/anaconda3/bin/python3.11")
tensorflow::tf_config()

model_data_nn <- model_data

model_data_nn <- model_data_nn %>% drop_na()


# Prepare data and dummy coding

## Dummy variant
model_data_nn2 <- model_data_nn %>%
  dplyr::select(-lemma, -frame, -Object_FE_Meaning) %>% 
  mutate_at(c("Object_FE_Realisation", "Telicity", "Object_Definiteness", "Text_category"), as.factor) %>%
  dummy_cols() %>% 
  dplyr::select(-Object_FE_Realisation_overt, -Object_FE_Realisation_null, -Telicity, -Object_Definiteness, -Text_category)

## Non-dummy variant

model_data_nn3 <- model_data_nn %>%
  mutate_at(vars("Object_FE_Realisation", "Telicity", "lemma", "frame", "Object_FE_Meaning", "Object_Definiteness", "Text_category"), ~ as.integer(as.factor(.x)) - 1)


# Data preprocessing for Keras (this actually works!)


# DUMMY VARIANT 

## Full model data

model_data_full <- model_data_nn2 %>% 
  mutate(Object_FE_Realisation = as.integer(as.factor(model_data_nn$Object_FE_Realisation)) - 1)

## Response
model_Y <- model_data_full %>% dplyr::select(Object_FE_Realisation) %>% as.matrix()

## Predictors
model_X <- model_data_full %>% dplyr::select(-Object_FE_Realisation) %>% as.matrix()

# NON-DUMMY VARIANT (WORKS!)

model_Y <- model_data_nn3 %>% dplyr::select(Object_FE_Realisation) %>% as.matrix()

model_X <- model_data_nn3 %>% dplyr::select(-Object_FE_Realisation) %>% as.matrix()



# Split data into training and testing sets
set.seed(123)
train_indices_nn <- sample(nrow(model_data_nn), 0.7 * nrow(model_data_nn))
train_data_nn <- model_data_nn[train_indices_nn, ]
test_data_nn <- model_data_nn[-train_indices_nn, ]



# Define a masking layer for NAs
#masking_layer <- layer_masking(mask_value = -1)

# Define the neural network model
model_keras <- keras_model_sequential() %>% # Initialization
  layer_dense(units = 17, # 17 neurons in first hidden layer
              activation = "relu", # ReLU as activation function
              input_shape = c(7)) %>% # Ten inputs
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model_keras %>% compile(
  optimizer = optimizer_sgd(learning_rate = 0.01), # Simple SGD with learning rate 0.01
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

history <- model_keras %>% fit(
  model_X,
  model_Y,
  epochs = 40,
  validation_split = 0.2
)

# Wrap the Keras model in a DALEX explainer object

# Create a custom predict function that applies the log odds transformation
custom_predict <- function(model, newdata) {
  # Predict probabilities using the model
  predicted_probabilities <- predict(model, newdata)
  
  # Apply the log odds transformation to the predicted probabilities
  log_odds <- log(predicted_probabilities / (1 - predicted_probabilities))
  
  # Return the log odds
  return(log_odds)
}


explainer_nn <- DALEX::explain(model_keras,
                               data = model_X,  # Exclude the target variable
                               y = as.numeric(model_Y),
                               type = "classification",
                               label = "Keras Neural Network") # log odds prediction

# Print the explainer object
print(explainer_nn)

eva_nn <- model_performance(explainer_nn)

var_importance <- variable_importance(explainer_nn)

plot(var_importance)

mp_text <- model_profile(explainer_nn, variable = "Telicity")

plot(mp_text)

pp_keras <- predict_parts(explainer_nn, henry_keras, type = "break_down")