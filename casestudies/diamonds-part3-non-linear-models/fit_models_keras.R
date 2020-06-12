library(tidyverse)
library(rsample)
library(keras)
library(recipes)
library(yardstick)

# change the order of color from lowest grade to highest grade
dfInput <- diamonds %>% 
    select(price, everything()) %>% 
    mutate(color = fct_rev(color))


get_partitioned_data <- function(randomSeed, formulaString, prop_train = 0.75) {
    
    model_formula <- as.formula(formulaString)
    
    set.seed(randomSeed)
    train_split <- dfInput %>% 
        select(price, everything()) %>% 
        initial_split(prop = prop_train, strata = "clarity")
    
    dfTrain <- training(train_split)
    dfTest <- testing(train_split)
    
    # Transform the data using a recipe using the upsampled training set to account for imbalance in the target variable
    recipe_obj <- recipe(model_formula, data = dfTrain) %>% 
        step_dummy(all_nominal(), -all_outcomes()) %>% 
        step_center(all_predictors(), -all_outcomes()) %>% 
        step_scale(all_predictors(), -all_outcomes()) %>% 
        prep(data = dfTrain)
    
    # Predictors
    x_train <- bake(recipe_obj, new_data = dfTrain) %>% select(-price)
    x_test  <- bake(recipe_obj, new_data = dfTest) %>% select(-price)
    
    # glimpse(x_train)
    
    # Outcome variable for training and testing sets
    y_train <- pull(dfTrain, price)
    y_test <- pull(dfTest, price)
    
    return(list(x_train = x_train, y_train = y_train, x_test = x_test, y_test = y_test))
}


# Fit keras neural network model
fit_keras_model <- function(randomSeed, formulaString, get_model = FALSE) {
    
    print(glue::glue("Running keras model: {formulaString} with seed: {randomSeed} at {Sys.time()}"))

    # get partitioned data
    lPartition <- get_partitioned_data(randomSeed, formulaString)
    x_train <- lPartition$x_train
    x_test <- lPartition$x_test
    y_train <- lPartition$y_train
    y_test <- lPartition$y_test
    
    # build the neural network
    model_keras <- keras_model_sequential() %>% 
        layer_dense(units = 32, activation = "relu", input_shape = ncol(x_train)) %>% 
        layer_dropout(rate = 0.1) %>%
        layer_dense(units = 32, activation = "relu") %>% 
        layer_dropout(rate = 0.1) %>%
        layer_dense(units = 1)
    
    model_keras %>% compile(
        optimizer = optimizer_rmsprop(lr = 0.01),
        loss = "mse",
        metrics = list("mean_absolute_error")
    )
    
    # model_keras
    
    # The patience parameter is the amount of epochs to check for improvement.
    early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)
    
    # Fit the keras model
    fit_keras <- fit(object = model_keras,
                  x = as.matrix(x_train),
                  y = y_train,
                  batch_size = 300,
                  epochs = 100,
                  validation_split = 0.333,
                  callbacks = list(early_stop))
    
    # predict on testing set to determine rmse  
    yhat_test <- predict(object = model_keras, x = as.matrix(x_test)) %>% as.vector()
    
    # predict on testing set
    dfResults <- tibble(actual = y_test, estimate = yhat_test)
    
    if(get_model) {
        return(list(model_keras = model_keras, fit_keras = fit_keras, dfResults = dfResults))
    } else {
        return(dfResults)
    }
}


set.seed(1000)
vRandomSeeds <- sample(9999, 4)
dfModels <- expand_grid(seed = vRandomSeeds, 
                        model = c("price ~ .",
                                  "price ~ carat",
                                  "price ~ carat + clarity",
                                  "price ~ carat + clarity + color",
                                  "price ~ carat + clarity + color + cut")) %>% 
    mutate(results = map2(seed, model, ~ fit_keras_model(.x, .y))) %>% 
    mutate(metrics = map(results, ~ metrics(.x, truth = actual, estimate = estimate))) %>%
    select(-results) %>% 
    unnest(cols = metrics) %>% 
    select(-.estimator) %>% 
    spread(.metric, .estimate) %>% 
    group_by(model) %>% 
    summarise_at(vars(rmse, rsq, mae), mean)

dfModels


# keras model architecture
formulaString_best <- "price ~ carat + clarity + color"
best_model_results <- fit_keras_model(1333, formulaString_best, get_model = T)

# LIME Interpretation
library(lime)

numSamples <- 6
numFeatures <- 5

formulaString_base <- "price ~ ."
lPartition <- get_partitioned_data(randomSeed = 1333, formulaString_base)

x_train <- lPartition$x_train
x_test <- lPartition$x_test

set.seed(1333)
x_sample <- x_test %>% sample_n(numSamples)

base_model_results <- fit_keras_model(1333, formulaString_base, get_model = T)
base_model_keras <- base_model_results$model_keras

# Setup lime::model_type() function for keras
model_type.keras.engine.sequential.Sequential <- function(x, ...) {
    "regression"
}

# Setup lime::predict_model() function for keras
predict_model.keras.engine.sequential.Sequential <- function(x, newdata, type, ...) {
    pred <- predict(object = x, x = as.matrix(newdata)) %>% as.vector()
    data.frame(price = pred, stringsAsFactors = F)
}

# # Test our predict_model() function
# predict_model(x = model_keras, newdata = x_sample, type = "raw")

# LIME Explanation
explainer_keras <- lime::lime(x_train, model = base_model_keras, bin_continuous = F, quantile_bins = F)

explanation_keras <- lime::explain(
    x = x_sample,
    explainer = explainer_keras, 
    n_features = numFeatures
)

# save data for Rmd
parentFolderPath <- here::here()
saveFilePath <- paste0(parentFolderPath, "/content/casestudies/diamonds-part3-non-linear-models/fit_models_keras.Rda")
save(dfModels, best_model_results, formulaString_best, numSamples, numFeatures, 
     explanation_keras, base_model_results, formulaString_base, file=saveFilePath)
