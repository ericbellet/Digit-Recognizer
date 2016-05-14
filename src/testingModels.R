######################################################################################
#Installing packages depend
install("e1071")
######################################################################################
#Importing libraries
library(e1071)
######################################################################################
#Loading data set
######################################################################################
setwd("C:/Users/deyban.perez/Documents/Repos/Proyecto")
source("src/functions.r")
train = read.csv("data/train.csv")
######################################################################################
#Making a Sample
######################################################################################
#This must be checked, because we need to do a sub sample because SVM is too much slow 
set.seed(22)
subIndex = sample(nrow(train), floor(nrow(train) * 0.8), replace = F)
train = train[subIndex, ]
training.data = train[subIndex, ]
testing.data = train[-subIndex, ]
######################################################################################
#Converting labels to factor type
training.data$label = as.factor(training.data$label)
######################################################################################
#Lets start to apply SVM models
######################################################################################
#Linear SVM Kernel
######################################################################################
load(file = "models/model_linear.rda")
#Making  confussion matrix
confusionMatrix_linear = table(True = testing.data[,1],
                               Pred = predict(model_linear, testing.data[,-1], type = "class"))
#Calculating the hit rate
hitRate_linear = sumDiag(confusionMatrix_linear) / nrow(testing.data) * 100
#Calculating the miss rate
missRate_linear = 100 - hitRate_linear
#Showinng rates
confusionMatrix_linear
hitRate_linear
missRate_linear
save(model_polynomial, file = "models/model_polynomial.rda")
######################################################################################
#Polynomial Kernel
######################################################################################
load(file = "models/model_polynomial.rda")
#Making confussion matrix
confusionMatrix_polynomial = table(True = testing.data[,1],
                                   Pred = predict(model_polynomial, testing.data[,-1], type = "class"))
#Calculating the hit rate
hitRate_polynomial = sumDiag(confusionMatrix_polynomial) / nrow(testing.data) * 100
#Calculating the miss rate
missRate_polynomial = 100 - hitRate_polynomial
#Showing rates
confusionMatrix_polynomial
hitRate_polynomial
missRate_polynomial
######################################################################################
#Radial Kernel
######################################################################################
load(file = "models/model_radial.rda")
#Making confussion matrix
confusionMatrix_radial = table(True = testing.data[,1],
                               Pred = predict(model_radial, testing.data[,-1], type = "class"))
#Calculating the hit rate
hitRate_radial = sumDiag(confusionMatrix_radial) / nrow(testing.data) * 100
#Calculating the miss rate
missRate_radial = 100 - hitRate_radial
#Showing rates
confusionMatrix_radial
hitRate_radial
missRate_radial
######################################################################################
#Sigmoid Kernel
######################################################################################
load(file = "models/model_sigmoid.rda")
#Making confussion matrix
confusionMatrix_sigmoid = table(True = testing.data[,1],
                                Pred = predict(model_sigmoid, testing.data[,-1], type = "class"))
#Calculating the hit rate
hitRate_sigmoid = sumDiag(confusionMatrix_sigmoid) / nrow(testing.data) * 100
#Calculating the miss rate
missRate_sigmoid = 100 - hitRate_sigmoid
#Showing rates
confusionMatrix_sigmoid
hitRate_sigmoid
missRate_sigmoid
