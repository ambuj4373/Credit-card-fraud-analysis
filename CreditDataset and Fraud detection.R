#1 Library and Dataset Load

library(ranger)
library(caret)
library(data.table)

creditcard_data <- read.csv("~/Downloads/creditcard.csv", header=TRUE)


#Exploratory Data Analysis

dim(creditcard_data) 
head(creditcard_data, 10)
tail(creditcard_data, 10)


table(creditcard_data$Class)
summary(creditcard_data$Amount)
names(creditcard_data)
var(creditcard_data$Amount)

sd(creditcard_data$Amount)

plot(creditcard_data$Amount)

#Data Manipulation

#We will scale the amount of the credit_card data frame to understand the spread of the data set and find any outliers.
##In R, the scale() function is used to standardize a numeric data vector or matrix by subtracting the mean and dividing
#by the standard deviation, resulting in a dataset with a mean of 0 and a standard deviation of 1.


creditcard_data$Amount = scale(creditcard_data$Amount)  
New_Data = creditcard_data[,-c(1)]
head(New_Data, 10)

#Data Modeling

#The Data set is now standardized and now we will prepare the entire data set into training and test partitions in ratio of 80:20

library(caTools)
set.seed(123)
data_sample<- sample.split(New_Data$Class, SplitRatio = 0.80)
Train_data = subset(New_Data, data_sample ==TRUE)
Test_data = subset(New_Data, data_sample ==FALSE)
dim(Train_data)
dim(Test_data)

#Logistics Model

Logistic_Model =glm(Class~.,Test_data, family =binomial())
summary(Logistic_Model)


#Training Logistics Regression Model
Logistic_Model = glm(Class~., Train_data, family = binomial())
plot(logistic_Model)

#Making predictions on Test_data
library(pROC)
lr.predict <- predict(Logistic_Model, newdata = Test_data, type = "response")

#Computing AUC
auc.gbm = roc(Test_data$Class, lr.predict, plot =TRUE, col ="blue")

