library(ngmodels)
library(tidyselect)
library(tidymodels)
library(keras)
library(furrr)

library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("slice", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("setdiff", "dplyr")
conflict_prefer("margin", "dials")

# load clean dataset
load("Ames_Housing_Clean.Rda")

# Determine skewed features
vFeaturesSkew <- dfClean %>% 
    select_if(is.numeric) %>% 
    select_if(function(Z) abs(skewness(Z, na.rm = T)) > 0.75) %>% 
    colnames() %>% 
    setdiff(c("Id","PropertyAge"))

# Remove Outliers and split clean data
########################################################################################################################################################
# dfClean <- dfClean %>% dplyr::filter(!(Id %in% c(524, 1299))) # glmnet cv_rmse improves to 0.117 but test_rmse is 0.135
dfTrain <- dfClean %>% filter(!(Id %in% vTestIDs))
dfTest <- dfClean %>% filter(Id %in% vTestIDs)

# Preprocessing Recipe
########################################################################################################################################################
get_preprocessing_recipe <- function(df) {
    
    preprocessing_recipe <- recipe(SalePrice ~ ., data = df) %>% 
        step_mutate_at(one_of(vFeaturesSkew), -all_outcomes(), fn = ~ log(. + 1L)) %>%
        step_rm("Id") %>% 
        step_dummy(all_nominal()) %>% 
        step_log(all_outcomes()) %>% 
        prep()
    return(preprocessing_recipe)
}

# Preprocessed data for training models
dfTrain_Proc <- get_preprocessing_recipe(dfTrain) %>% 
    bake(new_data = dfTrain) %>% 
    rename(target = SalePrice) %>% 
    select(target, everything())

dfTest_Proc <- get_preprocessing_recipe(dfTest) %>% 
    bake(new_data = dfTest) %>% 
    rename(target = SalePrice) %>% 
    select(target, everything())


# Create cv folds
########################################################################################################################################################
get_cv_folds <- function(df, numFolds, strataVar, randomSeed, shuffleData) {
    # shuffle the rows
    if(shuffleData) {
        set.seed(randomSeed)
        df <- df[sample(nrow(df)), ]
    }
    
    # split dataset into folds where each fold has the same distribution of target
    lFolds <- df %>% vfold_cv(v = numFolds, strata = all_of(strataVar))
    return(lFolds)
}


# Parsnip Model Specification
########################################################################################################################################################
get_model_spec <- function(modelType) {
    
    model_spec <- switch(modelType,
                         lm = linear_reg(mode = "regression") %>% 
                             set_engine("lm"),
                         
                         glmnet = linear_reg(mode = "regression", penalty = tune(), mixture = tune()) %>% 
                             set_engine("glmnet"),
                         
                         xgboost = boost_tree(mode="regression", trees=1000, min_n=tune(), tree_depth=tune(), learn_rate=tune()) %>% 
                             set_engine("xgboost", objective = "reg:squarederror"),
                         
                         kernlab = svm_rbf(mode="regression", cost=tune(), rbf_sigma=tune(), margin=tune()) %>% 
                             set_engine("kernlab"),
                         
                         randomForest = rand_forest(mode = "regression", trees = 1000, mtry = tune(), min_n = tune()) %>%
                             set_engine("randomForest", objective = "reg:squarederror"),
                         
                         ranger = rand_forest(mode = "regression", trees = 1000, mtry = tune(), min_n = tune()) %>%
                             set_engine("ranger"),
                         
                         mlp = mlp(mode = "regression", hidden_units = 5, dropout = tune(), epochs = 100) %>% 
                             set_engine("keras", verbose = 0, validation_split = 0.25, 
                                        optimizer = optimizer_rmsprop(lr = 0.01), loss = "mse", metrics = list("mean_absolute_error")))
    
    return(model_spec)
}


# Parameter Grid Search
########################################################################################################################################################
get_model_param_grid <- function(df, modelType, numParams, randomSeed) {
    
    set.seed(randomSeed)    
    param_spec <- switch(modelType,
                         glmnet = parameters(penalty(), mixture()),
                         kernlab = parameters(cost(), rbf_sigma(), margin()),
                         xgboost = parameters(min_n(), tree_depth(), learn_rate()),
                         randomForest = parameters(min_n(), finalize(mtry(), df)),
                         ranger = parameters(min_n(), finalize(mtry(), df)),
                         mlp = parameters(dropout(range = c(0, 0.1))))
    
    dfParamGrid <- grid_max_entropy(param_spec, size = numParams)
    return(dfParamGrid)
}


# Fitted model with parameters
########################################################################################################################################################
get_fitted_model <- function(df, model_spec, dfParams) {
    
    if(model_spec$engine == "lm") {
        fit_model <- model_spec %>% 
            fit(formula = target ~ ., data = df)
    } else {
        fit_model <- model_spec %>% 
            finalize_model(parameters = dfParams) %>% 
            fit(formula = target ~ ., data = df)
    }
    
    return(fit_model)
}


# Test Parameters on a training-validation fold
########################################################################################################################################################
test_parameters_on_fold <- function(fold_id, fold_split, model_spec, dfParams) {
    
    train_split <- analysis(fold_split)     # training data
    valid_split <- assessment(fold_split)   # validation data
    
    fit_model <- get_fitted_model(train_split, model_spec, dfParams)
    tibble("id" = fold_id, 
           "actual" = valid_split$target,
           "estimate" = predict(fit_model, new_data = valid_split) %>% unlist())
}


# Cross Validation Results
########################################################################################################################################################

cross_validate_model <- function(cv_folds, model_spec, param_grid) {
    
    dfResults <- param_grid %>% mutate(mean_rmse = NA)
    
    for(i in 1:nrow(dfResults)) {
        dfParams <- dfResults[i, ]
        if(model_spec$engine %in% c("lm", "xgboost")) {
            param_results <- pmap_dfr(list(cv_folds$id, cv_folds$splits), ~ test_parameters_on_fold(.x, .y, model_spec, dfParams))
        } else {
            param_results <- future_pmap_dfr(list(cv_folds$id, cv_folds$splits), ~ test_parameters_on_fold(.x, .y, model_spec, dfParams))
        }
        mean_rmse <- param_results %>% group_by(id) %>% rmse(truth = actual, estimate = estimate) %>% summarise(mean_rmse = mean(.estimate)) %>% pull()
        dfResults$mean_rmse[i] <- mean_rmse
    }
    return(dfResults)
}

# Create cross validation folds
cv_folds <- get_cv_folds(dfTrain_Proc, numFolds = 5, strataVar = "target", randomSeed = 2300, shuffleData = T)

# use `plan(sequential)` to effectively convert all
# subsequent `future_map*` calls to `map*`
# calls. this will result in sequential execution of 
# embarassingly parallel model fitting procedures
# but may prevent R from getting angry at parallelism
plan(multiprocess)

# lm model
model_spec_lm <- get_model_spec(modelType = "lm")
param_grid_lm <- tibble("dummy" = 10)
cv_results_lm <- cross_validate_model(cv_folds, model_spec_lm, param_grid_lm)

# glmnet model
model_spec_glmnet <- get_model_spec(modelType = "glmnet")
param_grid_glmnet <- get_model_param_grid(dfTrain_Proc, modelType = "glmnet", numParams = 25, randomSeed = 2300)
cv_results_glmnet <- cross_validate_model(cv_folds, model_spec_glmnet, param_grid_glmnet) %>% arrange(mean_rmse)
param_best_glmnet <- cv_results_glmnet %>% slice(1) %>% select(-mean_rmse)
model_final_glmnet <- get_fitted_model(dfTrain_Proc, model_spec_glmnet, param_best_glmnet)

# kernlab model
model_spec_kernlab <- get_model_spec(modelType = "kernlab")
param_grid_kernlab <- get_model_param_grid(dfTrain_Proc, modelType = "kernlab", numParams = 25, randomSeed = 2300)
cv_results_kernlab <- cross_validate_model(cv_folds, model_spec_kernlab, param_grid_kernlab) %>% arrange(mean_rmse)
param_best_kernlab <- cv_results_kernlab %>% slice(1) %>% select(-mean_rmse)
model_final_kernlab <- get_fitted_model(dfTrain_Proc, model_spec_kernlab, param_best_kernlab)

# randomForest model
model_spec_rf <- get_model_spec(modelType = "randomForest")
param_grid_rf <- get_model_param_grid(dfTrain_Proc, modelType = "randomForest", numParams = 25, randomSeed = 2300)
cv_results_rf <- cross_validate_model(cv_folds, model_spec_rf, param_grid_rf) %>% arrange(mean_rmse)
param_best_rf <- cv_results_rf %>% slice(1) %>% select(-mean_rmse)
model_final_rf <- get_fitted_model(dfTrain_Proc, model_spec_rf, param_best_rf)

# ranger model
model_spec_ranger <- get_model_spec(modelType = "ranger")
param_grid_ranger <- get_model_param_grid(dfTrain_Proc, modelType = "ranger", numParams = 25, randomSeed = 2300)
cv_results_ranger <- cross_validate_model(cv_folds, model_spec_ranger, param_grid_ranger) %>% arrange(mean_rmse)
param_best_ranger <- cv_results_ranger %>% slice(1) %>% select(-mean_rmse)
model_final_ranger <- get_fitted_model(dfTrain_Proc, model_spec_ranger, param_best_ranger)

# xgboost model
model_spec_xgboost <- get_model_spec(modelType = "xgboost")
param_grid_xgboost <- get_model_param_grid(dfTrain_Proc, modelType = "xgboost", numParams = 25, randomSeed = 2300)
cv_results_xgboost <- cross_validate_model(cv_folds, model_spec_xgboost, param_grid_xgboost) %>% arrange(mean_rmse)
param_best_xgboost <- cv_results_xgboost %>% slice(1) %>% select(-mean_rmse)
model_final_xgboost <- get_fitted_model(dfTrain_Proc, model_spec_xgboost, param_best_xgboost)


# save models and results
save(model_spec_lm, model_spec_glmnet, model_spec_rf, model_spec_ranger, model_spec_xgboost, model_spec_kernlab,
     model_final_glmnet, model_final_rf, model_final_ranger, model_final_xgboost, model_final_kernlab,
     cv_results_lm, cv_results_glmnet, cv_results_rf, cv_results_ranger, cv_results_xgboost, cv_results_kernlab,
     param_grid_glmnet, param_grid_rf, param_grid_ranger, param_grid_xgboost, param_grid_kernlab,
     param_best_glmnet, param_best_rf, param_best_ranger, param_best_xgboost, param_best_kernlab,
     dfTrain, dfTest, dfTrain_Proc, dfTest_Proc,
     get_preprocessing_recipe, cv_folds, vFeaturesSkew,
     file = "Ames_Tuning.Rda")

