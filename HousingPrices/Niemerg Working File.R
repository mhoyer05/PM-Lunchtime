################################################################################################################################
############################################# Documentation ####################################################################
################################################################################################################################
# Date:  201703
# Model: Master R File for Housing Prediction

################################################################################################################################
############################################# Load Libraries and Import File  ##################################################
################################################################################################################################

#set seed (to ensure reproducibility)
set.seed(12)

#load required libraries for algorithms and functionality
library(randomForest)   #Random Forest
library(gbm)            #Gradient boosting
library(xgboost)        #XGboost
library(glmnet)         #Elasticnet
library(data.table)     #fread function and data analysis capabilities
library(ggplot2)        #used for graphing

#set directory for importing/exporting files
setwd("M:/Predictive Modeling/PM @ Lunchtime/HousingPrices")

################################################################################################################################
############################################# Inspect File #####################################################################
################################################################################################################################

#get data dimensions: rows x columns
dim(data)

#inspect structure of data
str(data)

#look at first 5 and last 5 records
head(data)
tail(data)

#list of all column names
colnames(data)

################################################################################################################################
############################################# Example ##########################################################################
################################################################################################################################
#this example demonstrates what happens when a continuous variable that might have a linear fit has 0's that aren't true zeros
#but simply indicate no observation

#import file as "Housing_Raw"
example<- fread("example.csv", sep=",", stringsAsFactors=T)

#attach data, not neccesary but then you can call strings directly (example$x vs x)
attach(example)

#view data
example

#create a simple linear fix 
model1 <- lm(y~x)
summary(model1)

#create linear fit but with additional binary indicator for when x=0
model2 <- lm(y~x+binary)
summary(model2)

#create prediction
c1 <- predict(model1, example, c="response")
c2 <- predict(model2, example, c="response")

#append predictions to original dataset and view 
example <- cbind(example, c1, c2)
example


################################################################################################################################
############################################# Random thoughts ##################################################################
################################################################################################################################

#Note: see some NAs in the continuous and categorical variables - need to figure out how we handle them
#See many zeros in some continuous variables - need to figure out how to handle: add categorical non-zero indicator?
#What do we use as loss function (L1 or L2) or some categorical transformation of the value


################################################################################################################################
############################################# Exploratory Graphs ###############################################################
################################################################################################################################

#graph the target variable SalePrice
ggplot(data, aes(x=SalePrice))+
  geom_histogram(bins=20, alpha=.5, color=1,fill=4) 

#graph the log target variable SalePrice
#note: data taking the log seems to remove skew, we can use this in modeling
ggplot(data, aes(x=log(SalePrice)))+
  geom_histogram(bins=20, alpha=.5, color=1,fill=4) 

#log target with overlapping normal density
ggplot(data, aes(x=log(SalePrice)))+
  geom_histogram(aes(y = ..density..), bins=20, alpha=.5, color=1,fill=4) + 
  stat_function(
    fun = dnorm, 
    args = with(data, c(mean = mean(log(SalePrice)), sd = sd(log(SalePrice)))), color= 10
  ) + 
  scale_x_continuous("log(SalePrice)")

#qqplot of log SalePrice data
qqnorm(log(data$SalePrice))
qqline(log(data$SalePrice), col = 2)

#force data to be data.frame to make slicing easy
data <- as.data.frame(data)

#create dataframe of factors only
fctr.col <- data[sapply(data, is.factor)] 

#create dataframe of numeric columns only
num.col <- data[sapply(data, is.numeric)] 

#graph of continuous values using normal plotting function
par(mfrow=c(3,3))

for (i in c(1:9)) {
  hist(num.col[[i]], main=colnames(num.col)[i], xlab=colnames(num.col)[i])
}


#graph of continuous values using ggplot2 - can only graph one at a time
i = 4

ggplot(num.col, aes(x=num.col[[i]]))+
  geom_histogram(bins=20, alpha=.5, color=1,fill=4) +
  labs(title=colnames(num.col)[[i]], x=colnames(num.col)[[i]])


#graph of categorical values using ggplot2 - can only graph one at a time

#boxplot
ggplot(data, aes(x=MSZoning, y=SalePrice)) +
  geom_boxplot()

#violin plot
ggplot(data, aes(x=MSZoning, y=SalePrice)) +
  geom_violin()


#create correlation matrix - note: many NA values
corr <- round(cor(num.col), 1)

#plot correlation matrix -- not working yet
library(ggcorrplot)
ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)