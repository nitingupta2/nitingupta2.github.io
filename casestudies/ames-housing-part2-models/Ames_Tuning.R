library(ngmodels)
library(tidyselect)
library(tidymodels)
library(keras)

library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("setdiff", "dplyr")
conflict_prefer("margin", "dials")

# load clean dataset
load("Ames_Housing_Clean.Rda")

# Remove Outliers and split clean data
########################################################################################################################################################
# dfClean <- dfClean %>% dplyr::filter(!(Id %in% c(524, 1299))) # glmnet cv_rmse improves to 0.117 but test_rmse is 0.135
dfTrain <- dfClean %>% filter(!(Id %in% vTestIDs))
dfTest <- dfClean %>% filter(Id %in% vTestIDs)

# Preprocessing Data
########################################################################################################################################################
normalize_skewed_data <- function(Z) {
    vNorm <- Z
    if(abs(skewness(Z, na.rm = T)) > 0.75) {
        vNorm <- log(Z + 1)
    }
    return(vNorm)
}

# Features to Preprocess excluding Ids
vFeaturesProc <- dplyr::setdiff(vFeaturesNum, c("Id","PropertyAge"))

# Preprocessing Recipe
preprocessing_recipe <- recipe(SalePrice ~ ., data = dfClean) %>% 
    step_mutate_at(one_of(vFeaturesProc), -all_outcomes(), fn = normalize_skewed_data) %>%
    prep()

# Normalized & Dummified Data
dfNorm <- preprocessing_recipe %>% 
    bake(dfClean) %>% 
    mutate(target = log(SalePrice)) %>% 
    select(-SalePrice) %>% 
    select(target, everything())

# Processed data
dfTrain_Proc <- dfNorm %>% filter(!(Id %in% vTestIDs)) %>% select(-Id)
dfTest_Proc <- dfNorm %>% filter(Id %in% vTestIDs)

# Create cv folds
########################################################################################################################################################
get_cv_folds <- function(dfTrain_Proc, numFolds, strataVar, randomSeed, shuffleData) {
    # shuffle the rows
    if(shuffleData) {
        set.seed(randomSeed)
        dfTrain_Proc <- dfTrain_Proc[sample(nrow(dfTrain_Proc)), ]
    }
    
    # split dataset into folds where each fold has the same distribution of target
    lFolds <- dfTrain_Proc %>% vfold_cv(v = numFolds, strata = all_of(strataVar))
    
    return(lFolds)
}
########################################################################################################################################################

# Parsnip Model Specification
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
                         
                         mlp = mlp(mode = "regression", hidden_units = 5, dropout = tune(), epochs = 100) %>% 
                             set_engine("keras", verbose = 0, validation_split = 0.25, 
                                        optimizer = optimizer_rmsprop(lr = 0.01), loss = "mse", metrics = list("mean_absolute_error")))
    
    return(model_spec)
}

########################################################################################################################################################

# Parameter Grid Search
get_model_param_grid <- function(dfTrain_Proc, modelType, numParams, randomSeed) {
    
    set.seed(randomSeed)    
    param_spec <- switch(modelType,
                         glmnet = parameters(penalty(), mixture()),
                         kernlab = parameters(cost(), rbf_sigma(), margin()),
                         xgboost = parameters(min_n(), tree_depth(), learn_rate()),
                         randomForest = parameters(min_n(), finalize(mtry(), dfTrain_Proc)),
                         mlp = parameters(dropout(range = c(0, 0.1))))
    
    dfGrid <- grid_max_entropy(param_spec, size = numParams)
    return(dfGrid)
}

########################################################################################################################################################

# Cross Validation Results
get_cv_results <- function(modelFormula, cv_folds, model_spec, param_grid) {
    
    cv_results <- tune_grid(
        formula = target ~ .,
        resamples = cv_folds,
        model = model_spec,
        grid = param_grid,
        metrics = metric_set(mae, rmse, rsq),
        control = control_grid(verbose = TRUE)
    )
    return(cv_results)
}

# Finalize model with best parameters
########################################################################################################################################################
get_finalized_model <- function(dfTrain_Proc, model_spec, param_best) {
    
    model_final <- model_spec %>% 
        finalize_model(parameters = param_best) %>% 
        fit(formula = target ~ ., data = dfTrain_Proc)
    
    return(model_final)
}

# Create cv folds
########################################################################################################################################################
cv_folds <- get_cv_folds(dfTrain_Proc, numFolds = 4, strataVar = "target", randomSeed = 2300, shuffleData = TRUE)


# lm model
model_spec_lm <- get_model_spec(modelType = "lm")
cv_results_lm <- get_cv_results(as.formula("target ~ ."), cv_folds, model_spec_lm, param_grid = 10)
cv_best_lm <- cv_results_lm %>% show_best(metric = "rmse", n = 1, maximize = F)

# glmnet model
model_spec_glmnet <- get_model_spec(modelType = "glmnet")
param_grid_glmnet <- get_model_param_grid(dfTrain_Proc, modelType = "glmnet", numParams = 30, randomSeed = 2300)
# glmnet hyperparameter tuning
cv_results_glmnet <- get_cv_results(as.formula("target ~ ."), cv_folds, model_spec_glmnet, param_grid_glmnet)
cv_best_glmnet <- cv_results_glmnet %>% show_best(metric = "rmse", n = 10, maximize = F)
# glmnet finalized model
param_best_glmnet <- cv_results_glmnet %>% select_best(metric = "rmse", maximize = F)
model_final_glmnet <- get_finalized_model(dfTrain_Proc, model_spec_glmnet, param_best_glmnet)

# randomForest model
model_spec_rf <- get_model_spec(modelType = "randomForest")
param_grid_rf <- get_model_param_grid(dfTrain_Proc, modelType = "randomForest", numParams = 30, randomSeed = 2300)
# randomForest hyperparameter tuning
cv_results_rf <- get_cv_results(as.formula("target ~ ."), cv_folds, model_spec_rf, param_grid_rf)
cv_best_rf <- cv_results_rf %>% show_best(metric = "rmse", n = 10, maximize = F)
# randomForest finalized model
param_best_rf <- cv_results_rf %>% select_best(metric = "rmse", maximize = F)
model_final_rf <- get_finalized_model(dfTrain_Proc, model_spec_rf, param_best_rf)


# xgboost model
model_spec_xgboost <- get_model_spec(modelType = "xgboost")
param_grid_xgboost <- get_model_param_grid(dfTrain_Proc, modelType = "xgboost", numParams = 30, randomSeed = 2300)
# xgboost hyperparameter tuning
cv_results_xgboost <- get_cv_results(as.formula("target ~ ."), cv_folds, model_spec_xgboost, param_grid_xgboost)
cv_best_xgboost <- cv_results_xgboost %>% show_best(metric = "rmse", n = 10, maximize = F)
# xgboost finalized model
param_best_xgboost <- cv_results_xgboost %>% select_best(metric = "rmse", maximize = F)
model_final_xgboost <- get_finalized_model(dfTrain_Proc, model_spec_xgboost, param_best_xgboost)


# kernlab model
model_spec_kernlab <- get_model_spec(modelType = "kernlab")
param_grid_kernlab <- get_model_param_grid(dfTrain_Proc, modelType = "kernlab", numParams = 30, randomSeed = 2300)
# kernlab hyperparameter tuning
cv_results_kernlab <- get_cv_results(as.formula("target ~ ."), cv_folds, model_spec_kernlab, param_grid_kernlab)
cv_best_kernlab <- cv_results_kernlab %>% show_best(metric = "rmse", n = 10, maximize = F)
# kernlab finalized model
param_best_kernlab <- cv_results_kernlab %>% select_best(metric = "rmse", maximize = F)
model_final_kernlab <- get_finalized_model(dfTrain_Proc, model_spec_kernlab, param_best_kernlab)


# mlp model
model_spec_mlp <- get_model_spec(modelType = "mlp")
param_grid_mlp <- get_model_param_grid(dfTrain_Proc, modelType = "mlp", numParams = 30, randomSeed = 2300)
# mlp hyperparameter tuning
cv_results_mlp <- get_cv_results(as.formula("target ~ ."), cv_folds, model_spec_mlp, param_grid_mlp)
cv_best_mlp <- cv_results_mlp %>% show_best(metric = "rmse", n = 10, maximize = F)
# mlp finalized model
param_best_mlp <- cv_results_mlp %>% select_best(metric = "rmse", maximize = F)
model_final_mlp <- get_finalized_model(dfTrain_Proc, model_spec_mlp, param_best_mlp)


# save models and results
save(model_spec_lm, model_spec_glmnet, model_spec_rf, model_spec_xgboost, model_spec_mlp, model_spec_kernlab,
     model_final_glmnet, model_final_rf, model_final_xgboost, model_final_mlp, model_final_kernlab,
     cv_best_lm, cv_best_glmnet, cv_best_rf, cv_best_xgboost, cv_best_mlp, cv_best_kernlab,
     param_grid_glmnet, param_grid_rf, param_grid_xgboost, param_grid_mlp, param_grid_kernlab,
     param_best_glmnet, param_best_rf, param_best_xgboost, param_best_mlp, param_best_kernlab,
     dfTrain, dfTest, dfTrain_Proc, dfTest_Proc,
     preprocessing_recipe, cv_folds, normalize_skewed_data, vFeaturesProc,
     file = "Ames_Tuning.Rda")
