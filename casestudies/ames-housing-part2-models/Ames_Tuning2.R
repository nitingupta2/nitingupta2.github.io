# Hyperparameter tuning for the Ames Housing case study (Part 2).
# Produces Ames_Tuning.Rda, which index.Rmd loads to build the post.
#
# Why this script is built the way it is (speed):
# The dataset is small (1,460 rows x ~150 predictors after encoding), so run time is driven by the
# number of model fits (5 models x 25 grid points x 5 folds) and by how expensive each fit is, not by data size.
# The earlier version took ~35 minutes; this version takes ~11 minutes with 6 workers, tuning the same models and
# hyperparameters (with randomForest replaced by the equivalent ranger engine). The changes:
#   1. tune_grid() parallelises over every fold x grid-point combination ("everything"), instead of a manual
#      for-loop over grid points that only ran the 5 folds in parallel and left the remaining cores idle.
#   2. Random forest uses only the `ranger` engine with 500 trees. The `randomForest` package was tuned in addition
#      to ranger and gave the same cross-validated RMSE (0.135 vs 0.134). On one fold with mtry = 84, min_n = 2:
#        randomForest, 1,000 trees: 46.9s, RMSE 0.1305
#        ranger,       1,000 trees: 15.1s, RMSE 0.1303
#        ranger,         500 trees:  7.2s, RMSE 0.1293
#      Random forest error levels off well before 500 trees on data of this size, so 1,000 trees only doubled the cost.
#   3. xgboost searches a sensible learning-rate range, instead of spending much of the grid on learning rates as
#      small as 1e-10 that cannot converge in 1,000 rounds (see get_model_param_grid). Early stopping was tried and
#      rejected; see the note in get_model_spec.
#   4. The number of parallel workers is capped by available RAM as well as CPU cores (see "Parallel backend").
#
# Unrelated to speed: the earlier version used Id as a predictor. It is now excluded (see get_workflow).
# It also had 21 predictors that were exact linear combinations of others, which lm reported as a rank-deficient
# fit. They are now removed (see "Collinear predictors" and get_preprocessing_recipe), leaving a full-rank design.
# Ordered factors are encoded as integer scores instead of polynomial contrasts (see get_preprocessing_recipe).

library(nglib)
library(tidyselect)
library(tidymodels)
library(keras)
library(future)

library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("slice", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("setdiff", "dplyr")
# conflict_prefer("margin", "ggplot2")

# load clean dataset
load("Ames_Housing_Clean.Rda")

# Collinear predictors
########################################################################################################################################################
# With the earlier set of features, 21 of the ~250 predictors were exact linear combinations of others (the design
# matrix had rank 229 of 250), so lm could not estimate them and warned of a rank-deficient fit. Tree models are not
# affected by duplicated information, but linear models are: the coefficients of collinear predictors are unstable.
# These features are dropped because other features already carry their information (see the association matrix
# in Part 1):
#   HasGarage, HasFireplace, HasMasVnr: the same as the "None" category of GarageType, FireplaceQu and MasVnrType.
#   MSSubClass: combines the building type and style, already in BldgType and HouseStyle (bias-corrected Cramer's V
#               of 0.89 and 0.83). Its "duplex" category is identical to BldgType's.
#   HalfBath:   TotalBath = FullBath + 0.5 * HalfBath + BsmtBath exactly. Dropping HalfBath rather than TotalBath
#               keeps the total, one of the most important features for the tree models, and loses no information
#               because HalfBath can be recovered from the other three.
vFeaturesCollinear <- c("HasGarage", "HasFireplace", "HasMasVnr", "MSSubClass", "HalfBath")

# Categories that no property has (e.g. a Poor exterior quality) are dropped too, so that the scores of ordered factors
# are consecutive (see get_preprocessing_recipe).
dfClean <- dfClean %>%
    select(-all_of(vFeaturesCollinear)) %>%
    mutate(across(where(is.factor), droplevels))

# Determine skewed features
vFeaturesSkew <- dfClean %>%
    select_if(is.numeric) %>%
    select_if(function(Z) abs(timeDate::skewness(Z, na.rm = T)) > 0.75) %>%
    colnames() %>%
    setdiff(c("Id","PropertyAge"))

# Remove Outliers and split clean data
########################################################################################################################################################
# dfClean <- dfClean %>% dplyr::filter(!(Id %in% c(524, 1299))) # glmnet cv_rmse improves to 0.117 but test_rmse is 0.135
dfTrain <- dfClean %>% filter(!(Id %in% vTestIDs))
dfTest <- dfClean %>% filter(Id %in% vTestIDs)

# Preprocessing Recipe
########################################################################################################################################################
# Ordered factors (quality and condition grades, etc.) are encoded as integer scores, one predictor per feature, in
# the order of their categories. The alternative, step_dummy's polynomial contrasts, creates one predictor per category
# but one. Grades that only a handful of properties have can then be missing from a cross validation fold, which makes
# the highest-order contrasts collinear: with that encoding lm reported a rank-deficient fit in 4 of the 5 folds. Scores
# assume the grades are equally spaced, which is a reasonable approximation for these grades, and they cut the
# number of predictors from ~214 to ~151. dfEncodingComparison below compares the two encodings.
#
# step_lincomb removes any remaining exact linear combinations among the predictors. These can arise because properties
# without a basement or garage share the "None" category of every basement or garage feature. An intercept column is
# added for the check, because such combinations can include a constant, and removed afterwards.
get_preprocessing_recipe <- function(df, ordinalScores = TRUE) {

    preprocessing_recipe <- recipe(SalePrice ~ ., data = df) %>%
        update_role(Id, new_role = "ID") %>%
        step_mutate_at(one_of(vFeaturesSkew), -all_outcomes(), fn = ~ log(. + 1L))
    if(ordinalScores) {
        preprocessing_recipe <- preprocessing_recipe %>%
            step_ordinalscore(all_ordered_predictors())
    }
    preprocessing_recipe <- preprocessing_recipe %>%
        step_dummy(all_nominal()) %>%
        step_intercept() %>%
        step_lincomb(all_numeric_predictors()) %>%
        step_rm(any_of("intercept")) %>%
        step_log(all_outcomes()) %>%
        prep()
    return(preprocessing_recipe)
}

# The recipe is prepared on the training set only, and the same recipe is applied to the testing set (and to any new
# data), so that both have exactly the same predictors.
preprocessing_recipe <- get_preprocessing_recipe(dfTrain)

# Preprocessed data for training models
dfTrain_Proc <- preprocessing_recipe %>%
    bake(new_data = NULL) %>%
    rename(target = SalePrice) %>%
    select(target, everything())

dfTest_Proc <- preprocessing_recipe %>%
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

                         # No early stopping. It was tried (stop after 50 rounds without improvement on a 10% validation split)
                         # and roughly halved xgboost tuning time, but the final model was unstable: refitting the same best
                         # parameters with different random validation splits stopped anywhere from 195 to 687 rounds, with test
                         # RMSE between 0.133 and 0.151, versus 0.131 for a fixed 1,000 trees. With only ~146 validation rows the
                         # stopping point is too noisy. The narrower learn_rate range already removes the wasted fits.
                         # nthread = 1 because parallelism comes from running many fits at once (one per worker).
                         xgboost = boost_tree(mode="regression", trees=1000, min_n=tune(), tree_depth=tune(), learn_rate=tune()) %>%
                             set_engine("xgboost", objective = "reg:squarederror", nthread = 1),

                         kernlab = svm_rbf(mode="regression", cost=tune(), rbf_sigma=tune(), margin=tune()) %>%
                             set_engine("kernlab"),

                         # ranger replaces the randomForest engine: same algorithm, ~3x faster implementation, and 500 trees
                         # instead of 1,000 with no loss of accuracy (see the timings at the top of this script).
                         # num.threads = 1 for the same reason as xgboost's nthread above.
                         ranger = rand_forest(mode = "regression", trees = 500, mtry = tune(), min_n = tune()) %>%
                             set_engine("ranger", num.threads = 1),

                         mlp = mlp(mode = "regression", hidden_units = 5, dropout = tune(), epochs = 100) %>%
                             set_engine("keras", verbose = 0, validation_split = 0.25,
                                        optimizer = optimizer_rmsprop(learning_rate = 0.01), loss = "mse", metrics = list("mean_absolute_error")))

    return(model_spec)
}


# Parameter Grid Search
########################################################################################################################################################
get_model_param_grid <- function(df, modelType, numParams, randomSeed) {

    # mtry is finalized on the predictors only, so its upper bound is the number of predictors (excludes target and Id)
    dfPredictors <- df %>% select(-target, -Id)

    set.seed(randomSeed)
    param_spec <- switch(modelType,
                         glmnet = parameters(penalty(), mixture()),
                         kernlab = parameters(cost(), rbf_sigma(), svm_margin()),
                         # learn_rate is on a log10 scale. The dials default (1e-10 to 1e-1) spends most of the grid on rates
                         # so small that 1,000 trees barely move from the starting prediction: those fits take the full time
                         # and are never competitive. 10^-2.5 to 10^-0.5 (~0.003 to ~0.3) covers the useful range; the best
                         # rate found with the default range was ~0.07, comfortably inside it.
                         xgboost = parameters(min_n(), tree_depth(), learn_rate(range = c(-2.5, -0.5))),
                         ranger = parameters(min_n(), finalize(mtry(), dfPredictors)),
                         mlp = parameters(dropout(range = c(0, 0.1))))

    dfParamGrid <- grid_space_filling(param_spec, size = numParams, type = "max_entropy")
    return(dfParamGrid)
}


# Workflow for a model
########################################################################################################################################################
# The data is already preprocessed, so the workflow only names the outcome and predictors.
# Id is excluded from the predictors: it is a row identifier, and with training Ids 1-1460 and test Ids 1461-2919,
# tree models would otherwise split on it and treat every test property as lying beyond the training range.
get_workflow <- function(model_spec) {
    workflow() %>%
        add_variables(outcomes = target, predictors = -Id) %>%
        add_model(model_spec)
}


# Cross Validation Results
########################################################################################################################################################
# tune_grid() fits every grid point on every fold. With parallel_over = "everything", each (fold, grid point)
# combination is a separate task, so all workers stay busy until the end of the run.
# The mean RMSE across folds is the same metric as before (mean of the per-fold RMSE).
cross_validate_model <- function(cv_folds, model_spec, param_grid) {

    tune_results <- get_workflow(model_spec) %>%
        tune_grid(resamples = cv_folds,
                  grid = param_grid,
                  metrics = metric_set(rmse),
                  control = control_grid(parallel_over = "everything"))

    dfResults <- tune_results %>%
        collect_metrics() %>%
        select(all_of(names(param_grid)), mean_rmse = mean) %>%
        arrange(mean_rmse)
    return(dfResults)
}


# Fitted model with parameters
########################################################################################################################################################
# Returns a fitted workflow; predict() on it selects the predictor columns from new data, so Id can stay in the data.
# The seed makes the final ranger and xgboost fits reproducible between runs.
get_fitted_model <- function(df, model_spec, dfParams, randomSeed = 2300) {

    model_wf <- get_workflow(model_spec)
    if(!is.null(dfParams)) model_wf <- model_wf %>% finalize_workflow(dfParams)
    set.seed(randomSeed)
    fit(model_wf, data = df)
}


# Parallel backend
########################################################################################################################################################
# Each worker is a separate R session with tidymodels and the model packages loaded. In a full run with 5 workers,
# no R process exceeded 0.6 GB and all R processes together (main session + workers) peaked at 3.2 GB. Budgeting
# 0.75 GB per worker leaves headroom. Workers are capped at cores - 1 (to keep the machine responsive) and at what fits in 70% of the RAM
# available when the run starts, so the run cannot push the machine into swapping even if other applications are open.
gb_per_worker <- 0.75
gb_available <- ps::ps_system_memory()$avail / 1024^3
num_workers <- max(1, min(parallel::detectCores() - 1, floor(0.7 * gb_available / gb_per_worker)))
message(sprintf("Using %d parallel workers (%.1f GB RAM available)", num_workers, gb_available))
plan(multisession, workers = num_workers)

# Create cross validation folds
cv_folds <- get_cv_folds(dfTrain_Proc, numFolds = 5, strataVar = "target", randomSeed = 2300, shuffleData = T)

# Compare the encodings of ordered factors with a linear model on the same folds (reported in the post)
compare_encoding <- function(ordinalScores) {

    df <- get_preprocessing_recipe(dfTrain, ordinalScores) %>%
        bake(new_data = NULL) %>%
        rename(target = SalePrice) %>%
        select(target, everything())
    folds <- get_cv_folds(df, numFolds = 5, strataVar = "target", randomSeed = 2300, shuffleData = T)
    vAliased <- map_int(folds$splits, ~ sum(is.na(coef(lm(target ~ ., data = analysis(.x) %>% select(-Id))))))

    tibble(encoding = if(ordinalScores) "ordinal scores" else "polynomial contrasts",
           predictors = ncol(df) - 2,
           folds_rank_deficient = sum(vAliased > 0),
           lm_cv_rmse = suppressWarnings(get_workflow(get_model_spec("lm")) %>%
                                             fit_resamples(resamples = folds, metrics = metric_set(rmse)) %>%
                                             collect_metrics() %>%
                                             pull(mean)))
}
dfEncodingComparison <- bind_rows(compare_encoding(ordinalScores = FALSE), compare_encoding(ordinalScores = TRUE))
print(dfEncodingComparison)

# Time each model so the post can report relative run times
dfTimings <- tibble(model = character(), seconds = numeric())
time_model <- function(model, expr) {
    t0 <- Sys.time()
    result <- force(expr)
    dfTimings <<- dfTimings %>% add_row(model = model, seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")))
    result
}

# lm model (no hyperparameters, so fit_resamples instead of tune_grid)
model_spec_lm <- get_model_spec(modelType = "lm")
cv_results_lm <- time_model("lm",
    get_workflow(model_spec_lm) %>%
        fit_resamples(resamples = cv_folds, metrics = metric_set(rmse)) %>%
        collect_metrics() %>%
        select(mean_rmse = mean))

# glmnet model
model_spec_glmnet <- get_model_spec(modelType = "glmnet")
param_grid_glmnet <- get_model_param_grid(dfTrain_Proc, modelType = "glmnet", numParams = 25, randomSeed = 2300)
cv_results_glmnet <- time_model("glmnet", cross_validate_model(cv_folds, model_spec_glmnet, param_grid_glmnet))
param_best_glmnet <- cv_results_glmnet %>% slice(1) %>% select(-mean_rmse)
model_final_glmnet <- get_fitted_model(dfTrain_Proc, model_spec_glmnet, param_best_glmnet)

# kernlab model
model_spec_kernlab <- get_model_spec(modelType = "kernlab")
param_grid_kernlab <- get_model_param_grid(dfTrain_Proc, modelType = "kernlab", numParams = 25, randomSeed = 2300)
cv_results_kernlab <- time_model("kernlab", cross_validate_model(cv_folds, model_spec_kernlab, param_grid_kernlab))
param_best_kernlab <- cv_results_kernlab %>% slice(1) %>% select(-mean_rmse)
model_final_kernlab <- get_fitted_model(dfTrain_Proc, model_spec_kernlab, param_best_kernlab)

# random forest model (ranger engine; objects keep the _rf names used in index.Rmd)
model_spec_rf <- get_model_spec(modelType = "ranger")
param_grid_rf <- get_model_param_grid(dfTrain_Proc, modelType = "ranger", numParams = 25, randomSeed = 2300)
cv_results_rf <- time_model("ranger", cross_validate_model(cv_folds, model_spec_rf, param_grid_rf))
param_best_rf <- cv_results_rf %>% slice(1) %>% select(-mean_rmse)
model_final_rf <- get_fitted_model(dfTrain_Proc, model_spec_rf, param_best_rf)

# xgboost model
model_spec_xgboost <- get_model_spec(modelType = "xgboost")
param_grid_xgboost <- get_model_param_grid(dfTrain_Proc, modelType = "xgboost", numParams = 25, randomSeed = 2300)
cv_results_xgboost <- time_model("xgboost", cross_validate_model(cv_folds, model_spec_xgboost, param_grid_xgboost))
param_best_xgboost <- cv_results_xgboost %>% slice(1) %>% select(-mean_rmse)
model_final_xgboost <- get_fitted_model(dfTrain_Proc, model_spec_xgboost, param_best_xgboost)

plan(sequential)
print(dfTimings)


# save models and results
save(model_spec_lm, model_spec_glmnet, model_spec_rf, model_spec_xgboost, model_spec_kernlab,
     model_final_glmnet, model_final_rf, model_final_xgboost, model_final_kernlab,
     cv_results_lm, cv_results_glmnet, cv_results_rf, cv_results_xgboost, cv_results_kernlab,
     param_grid_glmnet, param_grid_rf, param_grid_xgboost, param_grid_kernlab,
     param_best_glmnet, param_best_rf, param_best_xgboost, param_best_kernlab,
     dfTrain, dfTest, dfTrain_Proc, dfTest_Proc, dfTimings,
     get_preprocessing_recipe, preprocessing_recipe, vFeaturesCollinear, dfEncodingComparison, cv_folds, vFeaturesSkew,
     file = "Ames_Tuning.Rda")
