#==================================================================================================#
## Project:     PM @ Lunchtime - Housing Prices
## Purpose:     For learning and improving predictive modeling expertise
## Authored by: Mike Hoyer
## Date:        03/29/2017
##
## Updates:
##   - 
##
#==================================================================================================#


#==================================================================================================#
#### Global Setup ####
#==================================================================================================#

## Set working directory
setwd('M:/Predictive Modeling/PM @ Lunchtime/HousingPrices')

## Load necessary packages
library(data.table)   # fread for quick data loading
library(plyr)
library(dplyr)        # data manipulation
library(tidyr)        # data manipulation
library(ggplot2)      # plots
library(Rmisc)        # multiplot
library(glmnet)       # regularized glms
library(randomForest) # random forest alg
library(xgboost)      # xgboost
library(caret)        # createDataPartition
library(corrplot)     # corrplot

## Set random seed
set.seed(12)

#==================================================================================================#
#### Function Definitions ####
#==================================================================================================#



#==================================================================================================#
#### Load Data ####
#==================================================================================================#

## Read in data
train.data <- fread('train.csv', stringsAsFactors = TRUE, data.table = FALSE)

## Try adding a zero to SalePrice
#View(train.data %>% filter(`1stFlrSF` > 4000)) 
edit.id <- train.data %>% filter(`1stFlrSF` > 4000) %>% select(Id) %>% unlist
train.data[edit.id, 'SalePrice'] <- train.data[edit.id, 'SalePrice'] * 10


#==================================================================================================#
#### Preliminary EDA ####
#==================================================================================================#

## Structure and feel of data
str(train.data)
head(train.data)
tail(train.data)

## Explore the response variable

## Graph the target variable SalePrice
ggplot(train.data, aes(x = SalePrice))+
  geom_histogram(bins = 20, alpha = 0.5, color = 1, fill = 4) 

## Graph the log target variable SalePrice
## note: data taking the log seems to remove skew, we can use this in modeling
ggplot(train.data, aes(x = log(SalePrice)))+
  geom_histogram(bins = 20, alpha = 0.5, color = 1, fill = 4) 


## There are some "continuous" variables that should be factors
str(train.data %>% 
      select(BsmtFullBath, BsmtHalfBath, FullBath, KitchenAbvGr, HalfBath, TotRmsAbvGrd, GarageCars,
             BedroomAbvGr, Fireplaces, MoSold, YrSold))
cont.to.cat <- c('MSSubClass', 'BsmtFullBath', 'BsmtHalfBath', 'FullBath', 'KitchenAbvGr', 
                 'HalfBath', 'TotRmsAbvGrd', 'GarageCars', 'BedroomAbvGr', 'Fireplaces', 'MoSold', 
                 'YrSold')
train.data[, cont.to.cat] <- lapply(train.data[, cont.to.cat], FUN = as.factor)
str(train.data %>% 
      select(BsmtFullBath, BsmtHalfBath, FullBath, KitchenAbvGr, HalfBath, TotRmsAbvGrd, GarageCars,
             BedroomAbvGr, Fireplaces, MoSold, YrSold))


#==================================================================================================#
#### Continuous Plots ####
#==================================================================================================#

## Create vectors of column names for categorical and continuous variables
classes <- sapply(train.data, FUN = class)
unique(classes)

cont.cols <- names(train.data)[classes == 'integer']
cont.cols <- cont.cols[-which(cont.cols %in% c('Id', 'SalePrice'))]


## Plot distribution for each continous variable
cont.plots <- list()
for (col in cont.cols) {
  
  ## Genericize smaller data frame for plotting
  plot.data <- data.frame(x = train.data[[col]], y = train.data$SalePrice)
  plot.data <- plot.data[!is.na(plot.data$x), ]
  
  ## Create plot
  p <- ggplot(plot.data, aes(x = x)) +
    geom_histogram(bins = 20, alpha = 0.5, color = 1, fill = 4) + 
    xlab(col)
  
  ## Store plot in plot list
  cont.plots <- c(cont.plots, list(p))
}
suppressWarnings(multiplot(plotlist = cont.plots[ 1: 6], cols = 3))
suppressWarnings(multiplot(plotlist = cont.plots[ 7:12], cols = 3))
suppressWarnings(multiplot(plotlist = cont.plots[13:18], cols = 3))
suppressWarnings(multiplot(plotlist = cont.plots[19:24], cols = 3))
suppressWarnings(cont.plots[25])


## Plot relationship to SalePrice for each continous variable
cont.plots <- list()
for (col in cont.cols) {
  
  ## Genericize smaller data frame for plotting
  plot.data <- data.frame(x = train.data[[col]], y = train.data$SalePrice)
  plot.data <- plot.data[!is.na(plot.data$x), ]
  
  ## Create plot
  p <- ggplot(plot.data, aes(x = x, y = log1p(y))) +
    geom_point(alpha = 0.5) + 
    geom_jitter() +
    geom_smooth() +
    xlab(col) + 
    ylab('log1p(SalePrice)') 
  
  ## Store plot in plot list
  cont.plots <- c(cont.plots, list(p))
}
suppressWarnings(multiplot(plotlist = cont.plots[ 1: 6], cols = 3))
suppressWarnings(multiplot(plotlist = cont.plots[ 7:12], cols = 3))
suppressWarnings(multiplot(plotlist = cont.plots[13:18], cols = 3))
suppressWarnings(multiplot(plotlist = cont.plots[19:24], cols = 3))
suppressWarnings(cont.plots[25])


#==================================================================================================#
#### Categorical Plots ####
#==================================================================================================#

cat.cols <- names(train.data)[classes == 'factor']

## Plot distribution for each categorical variable
cat.plots <- list()
for (col in cat.cols) {
  
  ## Genericize smaller data frame for plotting
  plot.data <- data.frame(x = train.data[[col]], y = train.data$SalePrice)
  plot.data <- plot.data[!is.na(plot.data$x), ]
  
  ## Create plot
  p <- ggplot(plot.data, aes(x = x)) +
    geom_bar(aes(y = ..count.. / sum(..count..)), alpha = 0.5, color = 1, fill = 4) + 
    ylab('Proportion') +
    xlab(col)
  
  ## Store plot in plot list
  cat.plots <- c(cat.plots, list(p))
}
suppressWarnings(multiplot(plotlist = cat.plots[ 1: 9], cols = 3))
suppressWarnings(multiplot(plotlist = cat.plots[10:18], cols = 3))
suppressWarnings(multiplot(plotlist = cat.plots[19:27], cols = 3))
suppressWarnings(multiplot(plotlist = cat.plots[28:36], cols = 3))
suppressWarnings(multiplot(plotlist = cat.plots[37:45], cols = 3))
suppressWarnings(multiplot(plotlist = cat.plots[46:54], cols = 3))

## Plot relationship to SalePrice for each categorical variable
cat.plots <- list()
for (col in cat.cols) {
  
  ## Genericize smaller data frame for plotting
  plot.data <- data.frame(x = train.data[[col]], y = train.data$SalePrice)
  plot.data <- plot.data[!is.na(plot.data$x), ]
  
  ## Create plot
  p <- ggplot(plot.data, aes(x = x, y = log1p(y))) +
    geom_boxplot(alpha = 0.5, color = 1, fill = 4) + 
    ylab('log1p(SalePrice)') +
    xlab(col)
  
  ## Store plot in plot list
  cat.plots <- c(cat.plots, list(p))
}
suppressWarnings(multiplot(plotlist = cat.plots[ 1: 9], cols = 3))
suppressWarnings(multiplot(plotlist = cat.plots[10:18], cols = 3))
suppressWarnings(multiplot(plotlist = cat.plots[19:27], cols = 3))
suppressWarnings(multiplot(plotlist = cat.plots[28:36], cols = 3))
suppressWarnings(multiplot(plotlist = cat.plots[37:45], cols = 3))
suppressWarnings(multiplot(plotlist = cat.plots[46:54], cols = 3))

## Correlation Matrix
train.matrix <- data.matrix(train.data[, cont.cols]) * 1

## Plot correlation matrix
corr <- round(cor(train.matrix), 1)
corrplot(corr)