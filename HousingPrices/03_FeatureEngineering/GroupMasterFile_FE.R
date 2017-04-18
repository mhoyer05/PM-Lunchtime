
#==================================================================================================#
## Project:     PM @ Lunchtime - Housing Prices
## Purpose:     For learning and improving predictive modeling expertise
## Authored by: PM @ Lunchtime crew
## Date:        04/03/2017
#==================================================================================================#

#==================================================================================================#
#### Update Log ####
#==================================================================================================#

## Updates @ 04/03/2017 by MCN
##   - updated code with all Hoyer's visualiation
##   - added code to save visualizations
##   - added code to examine data
##   - added tests of normality (shapiro-wilks, qqplot) for target variable

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
train.data <- fread('train.csv', stringsAsFactors = TRUE, data.table = FALSE, na.strings=c("NA","N/A","null"))

## View a particular outlier in the dataset
View(train.data %>% filter(`1stFlrSF` > 4000)) ## Try adding a zero to SalePrice

train.data[train.data$Id == 1299, ] <- train.data %>% 
  filter(Id == 1299) %>% 
  mutate(SalePrice = SalePrice * 10)

#==================================================================================================#
#### Known Data Edits ####
#==================================================================================================#

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
#### Experimenting with Features ####
#==================================================================================================#

