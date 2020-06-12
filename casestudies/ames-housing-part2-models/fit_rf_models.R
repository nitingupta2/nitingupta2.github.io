library(tidyverse)
library(rsample)
library(recipes)
library(yardstick)
library(doParallel)
library(caret)
library(randomForest)


cl <- makeCluster(8)
registerDoParallel(cl)

load("Ames_Housing_Preprocessed.Rda")


set.seed(1000)
vRandomSeeds <- sample(9999, 4)

# validate model with different seeds
dfModels <- expand_grid(seed = vRandomSeeds,
                        predictors = list("ALL")) %>%
    mutate(results = map2(seed, predictors, ~ validate_model(.x, dfTrain, .y, "rf"))) %>%
    mutate(metrics = map(results, ~ metrics(.x, truth = actual, estimate = estimate))) %>%
    select(-results) %>%
    unnest(cols = metrics) %>%
    select(-.estimator) %>%
    spread(.metric, .estimate) %>%
    mutate(predictors = map_chr(predictors, ~ paste(.x, collapse = " + "))) %>%  
    select(rmse, rsq, mae, predictors)
# group_by(predictors) %>%
# summarise_at(vars(rmse, rsq, mae), mean)

print(dfModels)

# Training on the entire dataset
lTransform <- get_transformed_data(dfTrain, dfTest, is_scaled = FALSE)
model_fit <- fit_model(x_train = lTransform$x_train, y_train = lTransform$y_train %>% log(), model_method = "rf")
data.frame(actual = lTransform$y_test %>% log(), estimate = predict(model_fit, lTransform$x_test)) %>% 
    metrics(truth = actual, estimate = estimate)

# Submission entry on entire training dataset
# create_submission_entry(model_fit, lTransform$x_test, vTestIDs, "submit_rf.csv")


stopCluster(cl)