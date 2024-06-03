## Use this is if you do not split your data into training and test data sets (or if you only want to use training data in this procedure)

# Define the number of folds
num_folds <- 10  # You can adjust this number as needed

# Set up indices for k-fold cross-validation
set.seed(123)  # For reproducibility
folds <- cut(seq(1, nrow(PimaIndiansDiabetes)), breaks = num_folds, labels = FALSE)

# Create an empty vector to store accuracy values
accuracies <- numeric(num_folds)

# Perform k-fold cross-validation
for (i in 1:num_folds) {
  # Split data into training and validation sets
  validation_indices <- which(folds == i)
  validation_set <- PimaIndiansDiabetes[validation_indices, ]
  training_set <- PimaIndiansDiabetes[-validation_indices, ]
  
  # Train the model on the training set
  model <- ranger(diabetes ~ age, 
                  data = training_set, 
                  probability = FALSE, 
                  num.trees = 500)
  
  # Make predictions on the validation set
  predictions <- predict(model, data = validation_set, type = "response")$predictions
  
  # Calculate accuracy
  correct_predictions <- sum(predictions == validation_set$diabetes)
  total_predictions <- length(predictions)
  accuracy <- correct_predictions / total_predictions
  accuracies[i] <- accuracy
}

# Compute average accuracy
average_accuracy <- mean(accuracies)
print(paste("Average Accuracy:", average_accuracy))


## Use this if you split your data into training and test data sets

rf0 <- ranger(diabetes ~ age, data = PimaIndiansDiabetes, probability = T, oob.error = T, num.trees = 500)

# Split data into training set (75%) and test set (25%)
train_indices <- sample(1:nrow(PimaIndiansDiabetes), 0.75 * nrow(PimaIndiansDiabetes))
train_set <- PimaIndiansDiabetes[train_indices, ]
test_set <- PimaIndiansDiabetes[-train_indices, ]


# Define the number of folds
num_folds <- 5  # You can adjust this number as needed

# Set up indices for k-fold cross-validation using the training set
folds <- cut(seq(1, nrow(train_set)), breaks = num_folds, labels = FALSE)

# Create an empty vector to store accuracy values
accuracies <- numeric(num_folds)

# Perform k-fold cross-validation
for (i in 1:num_folds) {
  # Split training set into training and validation sets for this fold
  validation_indices <- which(folds == i)
  validation_set <- train_set[validation_indices, ]
  training_set_fold <- train_set[-validation_indices, ]
  
  # Train the model on the training set for this fold
  model <- ranger(diabetes ~ age, 
                  data = training_set_fold, 
                  probability = FALSE, 
                  num.trees = 500)
  
  # Make predictions on the validation set for this fold
  predictions <- predict(model, data = validation_set, type = "response")$predictions
  
  # Calculate accuracy for this fold
  correct_predictions <- sum(predictions == validation_set$diabetes)
  total_predictions <- length(predictions)
  accuracy <- correct_predictions / total_predictions
  accuracies[i] <- accuracy
}

# Compute average accuracy across all folds
average_accuracy <- mean(accuracies)
print(paste("Average Accuracy:", average_accuracy))

# Now, you can evaluate the model on the test set if needed
predictions_test <- predict(model, data = test_set, type = "response")$predictions
test_accuracy <- sum(predictions_test == test_set$diabetes) / nrow(test_set)
print(paste("Accuracy on Test Set:", test_accuracy))

