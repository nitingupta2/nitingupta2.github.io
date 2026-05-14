library(ngmodels)
library(tidyselect)
library(tidymodels)
library(keras)
library(randomForest)
library(stacks)

library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("slice", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("setdiff", "dplyr")
conflict_prefer("margin", "ggplot2")

# load clean dataset
load("Ames_Housing_Clean.Rda")

# Remove Outliers and split clean data
######################################################################################################################################################
# dfClean <- dfClean %>% dplyr::filter(!(Id %in% c(524, 1299))) # glmnet cv_rmse improves to 0.117 but test_rmse is 0.135
dfTrain <- dfClean %>% filter(!(Id %in% vTestIDs))
dfTest <- dfClean %>% filter(Id %in% vTestIDs)

set.seed(2300)
train_folds <- dfTrain %>% vfold_cv(10)

# Determine skewed features
vFeaturesSkew <- dfClean %>% 
    select_if(is.numeric) %>% 
    select_if(function(Z) abs(skewness(Z, na.rm = T)) > 0.75) %>% 
    colnames() %>% 
    setdiff(c("Id","PropertyAge"))

## OLS LINEAR MODEL
######################################################################################################################################################
lm_model <- linear_reg() %>% 
    set_engine("lm")

lm_recipe <- recipe(SalePrice ~ ., data = dfTrain) %>% 
    update_role(Id, new_role = "ID") %>% 
    step_mutate_at(LotFrontage, LotArea, MasVnrArea, BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF, `1stFlrSF`, `2ndFlrSF`,LowQualFinSF, 
                   BsmtHalfBath, KitchenAbvGr, TotRmsAbvGrd, WoodDeckSF, MiscVal, PorchSF, TotalSF,
                   fn = ~ log(. + 1L)) %>%
    step_dummy(all_nominal()) %>% 
    step_log(all_outcomes(), offset = 1)

lm_wf <- workflow() %>% 
    add_recipe(lm_recipe) %>% 
    add_model(lm_model)

lm_resamples <- lm_wf %>% 
    fit_resamples(train_folds)

lm_resamples %>% collect_metrics()

## Parallel Processing
######################################################################################################################################################
library(doParallel)
doParallel::registerDoParallel(cores = 7)

## Lasso Regression
######################################################################################################################################################
mset <- metric_set(rmse)

tune_glmnet_model = linear_reg(mode = "regression", penalty = tune(), mixture = tune()) %>% 
    set_engine("glmnet")

tune_glmnet_wf <- workflow() %>% 
    add_recipe(lm_recipe) %>% 
    add_model(tune_glmnet_model)

glmnet_tune <- tune_glmnet_wf %>% 
    tune_grid(train_folds,
              grid = grid_max_entropy(parameters(penalty(), mixture()), size = 20),
              metrics = mset,
              control = control_stack_grid())

glmnet_tune %>% collect_metrics() %>% arrange(mean)

autoplot(glmnet_tune)

glmnet_model <- tune_glmnet_model %>% 
    finalize_model(parameters = select_best(glmnet_tune))

glmnet_wf <- tune_glmnet_wf %>% 
    finalize_workflow(parameters = select_best(glmnet_tune))

glmnet_fit <- glmnet_wf %>% fit(dfTrain)
dfTest_proc <- lm_recipe %>% prep() %>% bake(new_data = dfTest)
predict(glmnet_fit, dfTest)

glmnet_chosen <- glmnet_tune %>% 
    filter_parameters(parameters = select_best(glmnet_tune))

## Random Forest
######################################################################################################################################################
tune_rf_model <- rand_forest(mode = "regression", trees = tune(), mtry = tune()) %>% 
    set_engine("randomForest")

rf_recipe <- recipe(SalePrice ~ ., data = dfTrain) %>% 
    update_role(Id, new_role = "ID") %>% 
    step_dummy(all_nominal()) %>% 
    step_log(all_outcomes(), offset = 1)

tune_rf_wf <- workflow() %>% 
    add_recipe(rf_recipe) %>% 
    add_model(tune_rf_model)

rf_tune <- tune_rf_wf %>% 
    tune_grid(train_folds, 
              grid = crossing(trees = c(200, 300, 500, 1000), 
                              mtry = seq(70, 120, by = 10)), 
              metrics = mset,
              control = control_stack_grid())

rf_tune %>% collect_metrics() %>% arrange(mean)

autoplot(rf_tune)

# Final Random Forest Model
rf_model <- tune_rf_model %>% 
    finalize_model(parameters = select_best(rf_tune))

# Final Random Forest workflow
rf_wf <- tune_rf_wf %>% 
    finalize_workflow(parameters = select_best(rf_tune))

rf_wf %>% fit(dfTrain) 

rf_chosen <- rf_tune %>% 
    filter_parameters(parameters = select_best(rf_tune))

## xgboost
######################################################################################################################################################
tune_xgb_model <- boost_tree(mode="regression", trees=1000, min_n=tune(), tree_depth=tune(), learn_rate=tune()) %>% 
    set_engine("xgboost", objective = "reg:squarederror")

xgb_recipe <- recipe(SalePrice ~ ., data = dfTrain) %>% 
    update_role(Id, new_role = "ID") %>% 
    step_dummy(all_nominal()) %>% 
    step_log(all_outcomes(), offset = 1)

tune_xgb_wf <- workflow() %>% 
    add_recipe(xgb_recipe) %>% 
    add_model(tune_xgb_model)

xgb_tune <- tune_xgb_wf %>% 
    tune_grid(train_folds,
              grid = crossing(learn_rate = exp(seq(-3, 0, by = 0.5)),
                              min_n = seq(2, 20, 2),
                              tree_depth = 1:5),
              metrics = mset,
              control = control_stack_grid())

xgb_tune %>% collect_metrics() %>% arrange(mean)

autoplot(xgb_tune)

xgb_model <- tune_xgb_model %>% 
    finalize_model(parameters = select_best(xgb_tune))

xgb_wf <- tune_xgb_wf %>% 
    finalize_workflow(parameters = select_best(xgb_tune))

xgb_chosen <- xgb_tune %>% 
    filter_parameters(parameters = select_best(xgb_tune))

## Save models and results
######################################################################################################################################################
save(lm_recipe, lm_wf, lm_resamples,
     tune_glmnet_model, tune_glmnet_wf, glmnet_tune, glmnet_chosen,
     rf_recipe, tune_rf_model, tune_rf_wf, rf_tune, rf_chosen,
     tune_xgb_model, tune_xgb_wf, xgb_tune, xgb_chosen,
     file = "Ames_Results.Rda")


## Ensemble
######################################################################################################################################################
ensemble <- stacks() %>% 
    add_candidates(glmnet_chosen) %>% 
    add_candidates(rf_chosen) %>% 
    add_candidates(xgb_chosen)

ensemble_blended <- ensemble %>% blend_predictions()

ensemble_fit <- ensemble_blended %>% fit_members()

ensemble_fit %>% predict(lm_recipe %>% prep() %>% bake(new_data = head(dfTest)))

ensemble_blended_full <- ensemble_blended
ensemble_blended_full$train <- dfTrain

ensemble_fit_full <- ensemble_blended_full %>% fit_members()
ensemble_fit_full %>% predict(dfTest)
