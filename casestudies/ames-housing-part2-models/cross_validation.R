library(groupdata2)


# Center and scale the training data and apply the same transformation to the validation set
transform_data <- function(model_train, model_validate, isScaled) {
    
    model_formula <- as.formula("target ~ .")
    
    if(isScaled) {
        recipe_obj <- recipe(model_formula, data = model_train) %>%
            step_center(all_predictors(), -all_outcomes()) %>%
            step_scale(all_predictors(), -all_outcomes()) %>%
            prep(data = model_train)
    } else {
        recipe_obj <- recipe(model_formula, data = model_train) %>%
            prep(data = model_train)
    }
    
    # Predictors - remove target from predictors
    x_train <- bake(recipe_obj, new_data = model_train) %>% select(-target) %>% janitor::remove_empty("cols")
    x_validate  <- bake(recipe_obj, new_data = model_validate) %>% select(-target) %>% janitor::remove_empty("cols")
    
    # target variable for training and validation sets
    y_train <- model_train %>% pull(target)
    y_validate <- model_validate %>% pull(target)
    
    return(list(x_train = x_train, y_train = y_train, x_validate = x_validate, y_validate = y_validate))
}


# train model with caret
train_model <- function(x_train, y_train, modelMethod) {
    # model_fit <- train(x_train, y_train, method = modelMethod, trControl = trainControl(method = "repeatedcv", number = 4, allowParallel = T))
    model_fit <- train(x_train, y_train, method = modelMethod, trControl = trainControl(method = "none", allowParallel = T))
}


add_cross_validation_folds <- function(dfInput, numFolds, randomSeed, shuffleData, cvMethod = "regression") {
    # shuffle the rows
    if(shuffleData) {
        set.seed(randomSeed)
        dfInput <- dfInput[sample(nrow(dfInput)), ]
    }
    
    # divide the training data in folds
    if(cvMethod == "regression") {
        set.seed(randomSeed)
        # split dataset into folds where each fold has the same distribution of target
        dfInput <- dfInput %>% groupdata2::fold(k = numFolds, num_col = "target", handle_existing_fold_cols = "remove")
    }
    else if(cvMethod == "classification") {
        set.seed(randomSeed)
        dfInput <- dfInput %>% groupdata2::fold(k = numFolds, cat_col = "target", handle_existing_fold_cols = "remove")
    }
    else {
        # cvMethod not defined
        err <- simpleError("cvMethod is not defined")
        stop(err)
    }
    
    return(dfInput)
}

# Cross validate model
cross_validate_model <- function(dfInput, modelMethod, numFolds, randomSeed, shuffleData, cvMethod = "regression", isScaled = FALSE) {
    
    # predictorString <- paste(vPredictors, collapse = " + ")
    # print(glue::glue("{Sys.time()} - seed: {randomSeed} - Running {modelMethod} model with predictors: {predictorString}"))

    print(glue::glue("{Sys.time()} - seed: {randomSeed} - Running {modelMethod} model"))

    # add cross validation folds to the dataset
    dfInput <- add_cross_validation_folds(dfInput, numFolds, randomSeed, shuffleData, cvMethod)

    lResults <- list()
    # for each fold, get preprocessed data and run the model
    for(idxFold in 1:numFolds) {
        
        # get training and validation sets
        model_train <- dfInput %>% dplyr::filter(.folds == idxFold) %>% ungroup() %>% select(-.folds)
        model_validate <- dfInput %>% dplyr::filter(.folds != idxFold) %>% ungroup() %>% select(-.folds)

        # get centered and scaled predictors and targets
        lTransform <- transform_data(model_train, model_validate, isScaled)
        
        # transformed x_train and x_validate
        x_train <- lTransform$x_train
        x_validate <- lTransform$x_validate
        
        # convert the target variable to log terms
        y_train <- lTransform$y_train %>% log()
        y_validate <- lTransform$y_validate %>% log()
        
        # fit the model
        model_fit <- train_model(x_train, y_train, modelMethod)
        
        # save the model with the fold number and the modelling method
        save(model_fit, file = glue::glue("model_{modelMethod}_fold_{idxFold}.Rda"))
        
        # predict on validation set
        yhat_validate <- predict(model_fit, newdata = x_validate)
        
        # predict on testing set
        lResults[[idxFold]] <- data.frame(actual = y_validate, estimate = yhat_validate)
    }
    return(lResults)
}

# # How to run?
# lResults <- cross_validate_model(dfTrain, "rf", numFolds = 4, randomSeed = 1000, shuffleData = TRUE)
# map(lResults, function(Z) Z %>% metrics(truth = actual, estimate = estimate))

lResults <- cross_validate_model(dfTrain, "xgbLinear", numFolds = 4, randomSeed = 1000, shuffleData = TRUE, isScaled = TRUE)
map(lResults, function(Z) Z %>% metrics(truth = actual, estimate = estimate))
