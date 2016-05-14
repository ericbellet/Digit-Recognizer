######################################################################################
#Installing packages depend
install("e1071")
######################################################################################
#Importing libraries
library(e1071)
######################################################################################
#Loading data set
######################################################################################
#This line must be change for the locate of your file in your computer
setwd("C:/Users/deyban.perez/Documents/Repos/Proyecto")
source("src/functions.r")
train = read.csv("data/train.csv")
######################################################################################
#Making a Sample
######################################################################################
#This must be checked, because we need to do a sub sample because SVM is too much slow 
set.seed(22)
subIndex = sample(nrow(train), 1000, replace = F)
train = train[subIndex, ]
######################################################################################
#Counting number of ocurrencies of each number in training
vectorOcurrences = vector(mode = "numeric",length = 10)

for (i in seq(1:length(vectorOcurrences)))
{
  vectorOcurrences[i] = sum(train$label == (i-1))
}
######################################################################################
#Calculating probability of each number in training set
vectorProbabilities = vector(mode = "numeric",length = 10)

for (i in seq(1:length(vectorProbabilities)))
{
  vectorProbabilities[i] = 1/vectorOcurrences[i]
  
}
######################################################################################
#Assigning probability to each example in training set
vectorProbabilities.data = vector(mode = "numeric", length = nrow(train))

for (i in seq(1:nrow(train)))
{
  if(train$label[i] == 0)
  {
    vectorProbabilities.data[i] = vectorProbabilities[1]
  }else if(train$label[i] == 1)
  {
    vectorProbabilities.data[i] = vectorProbabilities[2]
  }else if(train$label[i] == 2)
  {
    vectorProbabilities.data[i] = vectorProbabilities[3]
  }else if(train$label[i] == 3)
  {
    vectorProbabilities.data[i] = vectorProbabilities[4]
  }else if(train$label[i] == 4)
  {
    vectorProbabilities.data[i] = vectorProbabilities[5]
  }else if(train$label[i] == 5)
  {
    vectorProbabilities.data[i] = vectorProbabilities[6]
  }  else if(train$label[i] == 6)
  {
    vectorProbabilities.data[i] = vectorProbabilities[7]
  }else if(train$label[i] == 7)
  {
    vectorProbabilities.data[i] = vectorProbabilities[8]
  }else if(train$label[i] == 8)
  {
    vectorProbabilities.data[i] = vectorProbabilities[9]
  }else if(train$label[i] == 9)
  {
    vectorProbabilities.data[i] = vectorProbabilities[10]
  }
}
######################################################################################
#Splitting data into training.data and testing.data sets
subIndex = sample(nrow(train), floor(nrow(train)* 0.8),
                  prob = vectorProbabilities.data, replace = F)
training.data = train[subIndex, ]
testing.data = train[-subIndex, ]
######################################################################################
#Converting labels to factor type
training.data$label = as.factor(training.data$label)
######################################################################################
#Polynomial kernel
######################################################################################
#Calculating best parameters
#Plynomial formula: (gamma*u'*v + coef0)^degree
parameters_polynomial = tune.svm(label ~ .,
                                 data = training.data,
                                 kernel = "polynomial",
                                 scale = F,
                                 gamma = 10^(-6:-1),
                                 cost = 10^(-1:1),
                                 degree = (2:4),
                                 coef0 = (-1:1))
parameters_polynomial$best.model
save(parameters_polynomial, file = "parameters/parameters_polynomial.rda")
######################################################################################
#Radial Kernel
######################################################################################
#Calculating best parameters
#Radial formula = exp(-gamma*|u-v|^2)
parameters_radial = tune.svm(label ~ .,
                             data = training.data,
                             kernel = "radial",
                             scale = F,
                             gamma = 10^(-6:-1))
parameters_radial$best.model
save(parameters_radial, file = "parameters/parameters_radial.rda")
######################################################################################
#Sigmoid Kernel
######################################################################################
#Calculating best parameters
#Sigmoid formula: "tanh(gamma*u'*v + coef0)"
parameters_sigmoid = tune.svm(label ~ .,
                              data = training.data,
                              kernel = "sigmoid",
                              scale = F,
                              gamma = 10^(-6:-1),
                              coef0 = (-1:1))
parameters_sigmoid$best.model
save(parameters_sigmoid, file = "parameters/parameters_sigmoid.rda")