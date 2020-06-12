
library(tidymodels)
library(tidyr)
library(modeldata)
library(keras)

library(furrr)

# load tuning results
load("Ames_Tuning.Rda")


# Helper functions
########################################################################################################################################################
fit_on_fold <- function(spec, prepped) {
    
    x <- juice(prepped, all_predictors())
    y <- juice(prepped, all_outcomes())

    # fit_xy(spec, x, y)
    spec %>% fit(target ~ ., data = bind_cols(y, x))
}

# Meta learner model
meta_learner <- function(models_df, preproc_recipe, meta_model_spec, train_df, random_seed = 2300) {
    
    set.seed(random_seed)
    cv_folds <- vfold_cv(train_df, v = 4, strata = "target")
    
    cv_fits <- expand_grid(cv_folds, models_df) %>%
        mutate(prepped = future_map(splits, prepper, preproc_recipe),
               fit = future_pmap(list(spec, prepped), fit_on_fold))
    
    prepped <- prep(preproc_recipe, training = train_df)
    
    full_fits <- models_df %>% 
        mutate(prepped = list(prepped)) %>% 
        mutate(fit = future_pmap(list(spec, prepped), fit_on_fold))
    
    holdout_preds <- cv_fits %>% 
        mutate(preds = future_pmap(list(fit, splits, prepped), predict_helper)) %>% 
        select(model_id, preds) %>% 
        unnest(preds) %>% 
        spread(model_id, .pred) %>% 
        select(-obs)
    
    y <- juice(prepped, all_outcomes())

    # meta_model <- fit_xy(meta_model_spec, holdout_preds, y)
    meta_model <- meta_model_spec %>% fit(target ~ ., data = bind_cols(y, holdout_preds))
    
    sl <- list(full_fits = full_fits, meta_model = meta_model, recipe = prepped)
    class(sl) <- "meta_model"
    sl
}


predict_helper <- function(fit, new_data, preproc_recipe) {
    
    # new_data can either be an rsample::rsplit object
    # or a data frame of genuinely new data
    
    if (inherits(new_data, "rsplit")) {
        obs <- as.integer(new_data, data = "assessment")
        
        # never forget to bake when predicting with recipes!
        new_data <- bake(preproc_recipe, assessment(new_data))
    } else {
        obs <- 1:nrow(new_data)
        new_data <- bake(preproc_recipe, new_data)
    }
    
    # if you want to generalize this code to a regression
    # super learner, you'd need to set `type = "response"` here
    
    predict(fit, new_data) %>% 
        tibble::add_column(obs = obs, .before = TRUE)
}

predict.meta_model <- function(x, new_data) {
    
    new_preds <- x$full_fits %>% 
        mutate(preds = future_map(fit, predict_helper, new_data, x$recipe)) %>% 
        select(model_id, preds) %>% 
        unnest(preds) %>% 
        spread(model_id, .pred) %>% 
        select(-obs)
    
    predict(x$meta_model, new_preds)
}

# Preprocessing recipe
dfNorm <- dfTrain_Proc %>% bind_rows(dfTest_Proc %>% select(-Id))

ames_preproc_recipe <- recipe(target ~ ., data = dfNorm)

ames_models_df <- map2_df(list(model_spec_glmnet, model_spec_kernlab, model_spec_rf, model_spec_xgboost), 
                          list(param_best_glmnet, param_best_kernlab, param_best_rf, param_best_xgboost), 
                          merge) %>% 
    dplyr::rename(spec = x) %>% 
    mutate(model_id = row_number())

ames_meta_model_spec <- linear_reg(mode = "regression", penalty = 0.00483, mixture = 0.9216) %>%
    set_engine("glmnet")

# ames_meta_model_spec <- boost_tree(mode = "regression", trees = 1000) %>%
#     set_engine("xgboost", objective = "reg:squarederror") %>%
#     finalize_model(parameters = param_best_xgboost)

plan(multicore)
ames_ml <- meta_learner(ames_models_df, ames_preproc_recipe, ames_meta_model_spec, dfTrain_Proc)

results_df <- predict(ames_ml, dfTest_Proc) %>% bind_cols(dfTest_Proc)

results_df %>% metrics(target, .pred)
results_df %>% 
    ggplot(aes(x = target, y = .pred)) + 
    geom_point(color = "#00AED8", alpha = 0.3) + 
    geom_abline(lty = 2) + 
    labs(x = "actual", y = "predicted")


