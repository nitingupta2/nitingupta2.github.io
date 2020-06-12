library(tidyverse)
library(rsample)
library(recipes)
library(yardstick)
library(doParallel)
library(caret)
library(xgboost)


cl <- makeCluster(8)
registerDoParallel(cl)

# change the order of color from lowest grade to highest grade
dfInput <- diamonds %>% 
    select(price, everything()) %>% 
    mutate(color = fct_rev(color))


get_partitioned_data <- function(randomSeed, formulaString) {
    
    model_formula <- as.formula(formulaString)
    
    set.seed(randomSeed)
    train_split <- dfInput %>% 
        select(price, everything()) %>% 
        initial_split(prop = 0.75, strata = "clarity")
    
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


# Fit random forest model
fit_xgb_model <- function(randomSeed, formulaString, get_model = FALSE) {
    
    print(glue::glue("{Sys.time()} - seed: {randomSeed} - Running xgboost model: {formulaString}"))
    
    #  get partitioned data
    lPartition <- get_partitioned_data(randomSeed, formulaString)
    x_train <- lPartition$x_train
    x_test <- lPartition$x_test
    y_train <- lPartition$y_train
    y_test <- lPartition$y_test
    
    # fit the model
    model_xgb <- train(x_train, y_train, method = "xgbLinear", trControl = trainControl(method = "cv", number = 3, allowParallel = T))
    
    # predict on testing set to determine rmse  
    yhat_test <- predict(model_xgb, x_test)
    
    # predict on testing set
    dfResults <- tibble(actual = y_test, estimate = yhat_test)
    
    if(get_model) {
        return(list(model_xgb = model_xgb, dfResults = dfResults))
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
    mutate(results = map2(seed, model, ~ fit_xgb_model(.x, .y))) %>% 
    mutate(metrics = map(results, ~ metrics(.x, truth = actual, estimate = estimate))) %>%
    select(-results) %>% 
    unnest(cols = metrics) %>% 
    select(-.estimator) %>% 
    spread(.metric, .estimate) %>% 
    group_by(model) %>% 
    summarise_at(vars(rmse, rsq, mae), mean)

dfModels


# best model summary
formulaString_best <- "price ~ carat + clarity + color"
best_model_results <- fit_xgb_model(1333, formulaString_best, get_model = T)

summary(best_model_results$model_xgb)


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

base_model_results <- fit_xgb_model(1333, formulaString_base, get_model = T)
model_xgb <- base_model_results$model_xgb

# LIME Explanation
explainer_xgb <- lime::lime(x_train, model = model_xgb, bin_continuous = F, quantile_bins = F)

explanation_xgb <- lime::explain(
    x = x_sample,
    explainer = explainer_xgb, 
    n_features = numFeatures
)


# stop parallel programming cluster
stopCluster(cl)

# save data for Rmd
parentFolderPath <- here::here()
saveFilePath <- paste0(parentFolderPath, "/content/casestudies/diamonds-part3-non-linear-models/fit_models_xgb.Rda")
save(dfModels, best_model_results, formulaString_best, numSamples, numFeatures, explanation_xgb, base_model_results, formulaString_base, file=saveFilePath)
