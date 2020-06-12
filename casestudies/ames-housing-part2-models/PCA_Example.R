library(tidyverse)

data("mtcars")

set.seed(1)
n_samples <- sample(nrow(mtcars), nrow(mtcars)*0.75)
y_train <- mtcars[n_samples,][1]
y_valid <- mtcars[-n_samples,][1]
pca_train <- mtcars[n_samples,] %>% select(-mpg)
pca_valid <- mtcars[-n_samples,] %>% select(-mpg)

# conduct PCA on training dataset
pca <- prcomp(pca_train, retx=TRUE, center=TRUE, scale=TRUE)
round(pca$sdev^2/sum(pca$sdev^2)*100) %>% cumsum() # percent explained variance

# training set for regression model are the first 2 principal components that explain 90% of the variance
x_train <- pca$x[,c(1:2)] %>% as.data.frame()

# prediction of PCs for validation dataset
x_valid <- predict(pca, newdata=pca_valid)[,c(1:2)] %>% as.data.frame()

# fit linear model on training dataset
fit_lm <- lm(mpg ~ ., data = cbind(y_train, x_train))
summary(fit_lm)

yhat_valid <- predict(fit_lm, x_valid)

y_valid %>% mutate(estimate = yhat_valid) %>% yardstick::metrics(truth = mpg, estimate = estimate)

y_valid %>% mutate(estimate = yhat_valid) %>% 
    ggplot(aes(x = mpg, y = estimate)) +
    geom_point(color = "midnightblue") +
    geom_abline(lty = 2, color = "gray50", size = 1)
