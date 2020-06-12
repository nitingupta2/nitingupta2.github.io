library(tidymodels)
library(modeldata)

data(credit_data)

set.seed(7075)
data_split <- initial_split(credit_data, strata = "Status", p = 0.75)

credit_train <- training(data_split)
credit_test  <- testing(data_split)

credit_rec <- 
    recipe(Status ~ ., data = credit_train) %>%
    step_knnimpute(Home, Job, Marital, Income, Assets, Debt) %>%
    step_dummy(all_nominal(), -Status) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors()) %>%
    prep(training = credit_train, retain = TRUE)

# juice() will be used to get the processed training set back

test_normalized <- bake(credit_rec, new_data = credit_test, all_predictors())

# Keras neural net
set.seed(57974)
nnet_fit <-
    mlp(epochs = 100, hidden_units = 5, dropout = 0.1) %>%
    set_mode("classification") %>% 
    # Also set engine-specific arguments: 
    set_engine("keras", verbose = 0, validation_split = .20) %>%
    fit(Status ~ ., data = juice(credit_rec))

nnet_fit

# test results
test_results <- 
    credit_test %>%
    select(Status) %>%
    as_tibble() %>%
    mutate(nnet_class = predict(nnet_fit, new_data = test_normalized) %>% pull(.pred_class),
           nnet_prob  = predict(nnet_fit, new_data = test_normalized, type = "prob") %>% pull(.pred_good))

test_results %>% roc_auc(truth = Status, nnet_prob)
test_results %>% accuracy(truth = Status, nnet_class)
test_results %>% conf_mat(truth = Status, nnet_class)
test_results %>% metrics(truth = Status, nnet_class)
