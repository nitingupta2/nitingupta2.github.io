
library(tidytext)

get_preprocessing_recipe <- function(df) {
    
    preprocessing_recipe <- recipe(SalePrice ~ ., data = df) %>% 
        update_role(Id, new_role = "id") %>% 
        step_mutate_at(one_of(vFeaturesSkew), -all_outcomes(), fn = ~ log(. + 1L)) %>%
        step_dummy(all_nominal()) %>% 
        step_log(all_outcomes()) %>% 
        step_normalize(all_predictors()) %>% 
        step_pca(all_predictors(), num_comp = 75) %>% 
        prep()
    return(preprocessing_recipe)
}

recipe_pca <- get_preprocessing_recipe(dfTrain)

recipe_pca %>% 
    tidy(5) %>% 
    mutate(component = fct_inorder(component)) %>% 
    filter(component %in% c("PC1","PC2","PC3","PC4")) %>% 
    group_by(component) %>% 
    top_n(20, abs(value)) %>% 
    ungroup() %>% 
    mutate(terms = reorder_within(terms, abs(value), component)) %>% 
    ggplot(aes(x = abs(value), y = terms, fill = value > 0)) +
    geom_col() +
    scale_fill_quant(palette = "metro") +
    scale_y_reordered() +
    facet_wrap(~component, scales = "free_y") + 
    labs(x = "Absolute value of contribution",
         y = NULL, 
         fill = "Positive?")


dfTrain_pca <- juice(recipe_pca)

# Use this dfTrain_Proc to train models
dfTrain_Proc <- dfTrain_pca %>% 
    select(-Id) %>% 
    rename(target = SalePrice)

ggplot(dfTrain_pca, aes(x = PC01, y = PC02, label = Id)) +
    geom_point(color = "red", alpha = 0.2) +
    geom_text(check_overlap = T, family = "Roboto Condensed")

# Standard deviation of principal components
sdev <- recipe_pca$steps[[5]]$res$sdev

# Cumulative % standard deviation - 115 terms explain over 90% of the standard deviation
cum_sdev <- cumsum(sdev/sum(sdev))

