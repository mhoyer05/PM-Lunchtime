################################################################################################################################
############################################# Documentation ####################################################################
################################################################################################################################
# Date:  201703
# Model: Master R File for Housing Prediction

################################################################################################################################
############################################# Load Libraries ###################################################################
################################################################################################################################

#set seed (to ensure reproducibility)
set.seed(12)

#load required libraries for algorithms and functionality
library(randomForest)   #Random Forest
library(gbm)            #Gradient boosting
library(xgboost)        #XGboost
library(glmnet)         #Elasticnet
library(data.table)     #fread function and data analysis capabilities


#set directory for importing/exporting files

