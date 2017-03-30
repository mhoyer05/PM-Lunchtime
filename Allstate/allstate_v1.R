#=================================================================================================#
# Kaggle Competition 
# Allstate Claims Severity

# 2016-10-21 MDH
#=================================================================================================#


#=================================================================================================#
#### Global Setup #### 
#=================================================================================================#

## Set Working directory
setwd('C:/Users/michael.hoyer/Documents/R/Kaggle/Allstate')

## Load pacakges
library(plyr)
library(dplyr)
library(ggplot2)
library(Rmisc)
library(moments)

## Load data
train <- read.csv('train.csv', header = TRUE, stringsAsFactors = TRUE)
test  <- read.csv( 'test.csv', header = TRUE, stringsAsFactors = TRUE)

#=================================================================================================#
#### Exploratory Data Analysis #### 
#=================================================================================================#

## Look at data structure
str(train)
names(train)
str(test)
names(test)

## What is the distribution of the loss?
ggplot(data = train, aes(x = loss)) +
    geom_density(size = 1)

## Probably oughta use log(loss)
ggplot(data = train, aes(x = log1p(loss))) +
    geom_density(size = 1)


#=================================================================================================#
## Continous Variables

## Examine the distribution of continuous variables
train %>% 
    select(starts_with('cont')) %>%
    summary

test %>% 
    select(starts_with('cont')) %>%
    summary

## Examine the distributions of continous variables and compare train to test
cont_plots <- list()
cont_plots_test <- list()
cont_cols <- names(train)[grep('cont', colnames(train))]

for (col in cont_cols) {
    
    ## Histograms for the training set
    plot_df <- data.frame(x = train[[col]], y = log(train$loss))
    
    p <- ggplot(data = plot_df, aes(x = x)) +
        geom_histogram(alpha = 0.5, bins = 50) +
        xlab(paste0(col, '\n', 'Skewness: ', round(skewness(plot_df$x, na.rm = TRUE), 2)))
    
    cont_plots <- c(cont_plots, list(p)) 
    
    ## Histograms for the testing set for comparison
    plot_df_test <- data.frame(x = test[[col]])
    
    p_test <- ggplot(data = plot_df_test, aes(x = x)) +
        geom_histogram(alpha = 0.5, bins = 50) +
        xlab(paste0(col, ' test set', '\n', 'Skewness: ', round(skewness(plot_df$x, na.rm = TRUE), 2)))
    
    cont_plots_test <- c(cont_plots_test, list(p_test))
    
}

multiplot(plotlist = c(cont_plots[ 1: 6], cont_plots_test[ 1: 6]), cols = 4)
multiplot(plotlist = c(cont_plots[ 7:12], cont_plots_test[ 7:12]), cols = 4)
multiplot(plotlist = c(cont_plots[13:14], cont_plots_test[13:14]), cols = 2)



#=================================================================================================#
## Categorical Variables

## Examine cardinality of levels
train %>% 
    select(starts_with('cat')) %>%
    sapply(nlevels) %>%
    table %>% 
    barchart

cat_levs_cnt <- train %>%
    select(starts_with('cat')) %>%
    sapply(nlevels)

two_lev_cats <- names(cat_levs_cnt)[cat_levs_cnt == 2]
# two_lev_plots <- list()
# for (col in two_lev_cats) {
#     df <- data.frame(x = train[[col]])
#     
#     p <- ggplot(df, aes(x = x)) +
#         geom_bar(aes(y = ..count.. / sum(..count..))) +
#         ylab('proportion') +
#         xlab(col)
#     
#     two_lev_plots <- c(two_lev_plots, list(p))
# }
# 
# multiplot(plotlist = two_lev_plots, cols = 7)

three_lev_cats <- names(cat_levs_cnt)[cat_levs_cnt == 3]
three_lev_plots <- list()
for (col in three_lev_cats) {
    df <- data.frame(x = train[[col]])
    
    p <- ggplot(df, aes(x = x)) +
        geom_bar(aes(y = ..count.. / sum(..count..))) +
        ylab('proportion') +
        xlab(col)
    
    three_lev_plots <- c(three_lev_plots, list(p))
}

multiplot(plotlist = three_lev_plots, cols = 2)

four_lev_cats <- names(cat_levs_cnt)[cat_levs_cnt == 4]
four_lev_plots <- list()
for (col in four_lev_cats) {
    df <- data.frame(x = train[[col]])
    
    p <- ggplot(df, aes(x = x)) +
        geom_bar(aes(y = ..count.. / sum(..count..))) +
        ylab('proportion') +
        xlab(col)
    
    four_lev_plots <- c(four_lev_plots, list(p))
}

multiplot(plotlist = four_lev_plots, cols = 4)

high_card_cats <- names(cat_levs_cnt)[cat_levs_cnt > 4]
high_card_plots <- list()
for (col in high_card_cats) {
    df <- data.frame(x = train[[col]])
    
    p <- ggplot(df, aes(x = x)) +
        geom_bar(aes(y = ..count.. / sum(..count..))) +
        ylab('proportion') +
        xlab(col)
    
    high_card_plots <- c(high_card_plots, list(p))
}

multiplot(plotlist = high_card_plots, cols = 7)


## Plot the relationships between categorical features to the log(loss)
cat_plots <- list()
cat_cols <- names(train)[grep('cat', colnames(train))]

for (col in cat_cols) {
    plot_df <- data.frame(x = train[[col]], y = log(train$loss))

    p <- ggplot(data = plot_df, aes(x = x, y = y)) +
        geom_boxplot() +
        xlab(col) +
        ylab('log(loss)')
    
    cat_plots <- c(cat_plots, list(p)) 
}


#=================================================================================================#
#### Feature Engineering #### 
#=================================================================================================#

## Add a flag to the training set for identification after any data edits
train$train <- 1

## Add loss column and train flat to test so any data change can be made to both datasets
test <- test %>% 
    mutate(loss = NA,
           train = 0)

full_data <- rbind(train, test)

## Collapse some of the sparse factor levels
collapseLevels <- function(x, p) {
    t <- table(x)
    levs <- names(which(prop.table(t) < p))
    
    levels(x)[levels(x) %in% levs] <- 'Other'
    
    return(x)
}

for (col in names(cat_levs_cnt)[!names(cat_levs_cnt) %in% two_lev_cats]) {
    full_data[[col]] <- collapseLevels(x = full_data[[col]], p = 0.05)
}

## Re-examine the cardinality after the collapse
full_data %>% 
    select(starts_with('cat')) %>%
    sapply(nlevels) %>%
    table %>% 
    barchart

## Run basic random forest for feature importance
library(randomForest)

set.seed(8675309)

dTrain <- full_data %>% filter(train == 1)
dTest  <- full_data %>% filter(train == 0)

feature_names <- colnames(dTrain)[which(!colnames(dTrain) %in% c('id', 'loss', 'train'))]

rf_basic <- randomForest(x = dTrain[, feature_names], 
                         y = log1p(dTrain$loss),
                         ntree = 10,
                         mtry = 8,
                         do.trace = TRUE)

varImpPlot(rf_basic)






