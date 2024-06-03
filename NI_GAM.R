

# Generalised additive models ---------------------------------------------

model_data_gam <- model_data

# Remove response variable from X_train
X <- subset(model_data_gam, select = -Object_FE_Realisation)

# Response variable
y <- model_data_gam$Object_FE_Realisation

# Define train and test data (categorical)
#set.seed(123)  # For reproducibility
#index <- createDataPartition(model_data_gam$Object_FE_Realisation, times = 1, p = 0.7, list = FALSE)
#X_train <- X[index, ]
#y_train <- y[index]
#X_test <- X[-index, ]
#y_test <- y[-index]


# Fit model

gam.1 <- bam(Object_FE_Realisation ~ Telicity +
               Object_Definiteness +
               s(lemma, bs = "re") +
               s(frame, bs = "re") +
               Text_category +
               s(concreteness),
             data = train_data,
             family = "binomial",
             na.action = "na.omit")

summary(gam.1)

# Create a DALEX explainer object for your model
explainer_gam <- DALEX::explain(model = gam.1, 
                                data = test_data, 
                                y = test_data$Object_FE_Realisation,
                                label = "GAM",
                                type = "classification")
#predict_function = custom_predict)

# Model performance

eva_gam <- DALEX::model_performance(explainer_gam)

