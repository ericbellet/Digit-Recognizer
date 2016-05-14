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
#Making the model
#Linear formula = u'*v
model_linear = svm(label ~ ., data = training.data, kernel= "linear", scale = F)
save(model_linear, file = "models/model_linear.rda")
######################################################################################
#Polynomial kernel
######################################################################################
#Making the model
load("parameters/parameters_polynomial.rda")
model_polynomial = svm(label ~ .,
                       data = training.data,
                       kernel = "polynomial",
                       scale = F,
                       cost = parameters_polynomial$best.model$cost,
                       degree = parameters_polynomial$best.model$degree,
                       gamma = parameters_polynomial$best.model$gamma,
                       coef0 = parameters_polynomial$best.model$coef0)
save(model_polynomial, file = "models/model_polynomial.rda")
######################################################################################
#Radial Kernel
######################################################################################
load("parameters/parameters_radial.rda")
#Making the model
model_radial = svm(label ~ .,
                   data = training.data,
                   kernel = "radial",
                   scale = F,
                   gamma = parameters_radial$best.model$gamma)
save(model_radial, file = "models/model_radial.rda")
######################################################################################
#Sigmoid Kernel
######################################################################################
#Making the model
load("parameters/parameters_sigmoid.rda")
model_sigmoid = svm(label ~ .,
                    data = training.data,
                    kernel = "sigmoid",
                    scale = F,
                    gamma = parameters_sigmoid$best.model$gamma,
                    coef0 = parameters_sigmoid$best.model$coef0)
save(model_sigmoid, file = "models/model_sigmoid.rda")