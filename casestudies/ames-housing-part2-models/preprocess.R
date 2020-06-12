

split_data <- function(dfInput, nrows_train, formulaString = "target ~ .") {
    model_formula <- as.formula(formulaString)
    
    recipe_obj <- recipe(model_formula, data = dfInput) %>% 
        step_dummy(all_nominal(), -all_outcomes()) %>% 
        prep(data = dfInput)
    
    dfInput <- bake(recipe_obj, new_data = dfInput)
    
    dfTrain <- dfInput %>% slice(1:nrows_train) %>% select(-Id)
    dfTest <- dfInput %>% slice(-(1:nrows_train))
    vTestIDs <- dfTest$Id
    dfTest <- dfTest %>% select(-Id)
    
    return(list(dfTrain = dfTrain, dfTest = dfTest, vTestIDs = vTestIDs))
}
