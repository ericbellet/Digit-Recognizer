######################################################################################
#Declaring functions to be used
######################################################################################
#Install a package, if it is no installed previously
install = function(pkg)
{
  # If is is installed does not install packages
  if (!require(pkg, character.only = TRUE))
  {
    install.packages(pkg)
    if (!require(pkg, character.only = TRUE))
      stop(paste("load failure:", pkg))
  }
}

#Take a rown from data set and plot the digit in a image 28X28 pixels
plotDigit = function(row, dataSet)
{
  image.data = matrix(data.matrix(dataSet[row, 2:ncol(dataSet)]), 28, 28)
  image.data = t(image.data)
  image.data = t(image.data)[ ,nrow(image.data):1]
  image(x = image.data, col = c(0,1))
}

#Function that returns the sum of the diagonal of whatever confussion matrix
sumDiag = function(confusionMatrix)
{
  returnValue = 0
  
  for (i in (1:nrow(confusionMatrix)))
    returnValue = returnValue + confusionMatrix[i,i]
  
  return(returnValue)
}
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
setwd("C:/Users/deyban.perez/Documents/Repos/Digit-Recognizer/data")
train = read.csv("train.csv")
test = read.csv("test.csv")
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
#Lets start to apply SVM models
######################################################################################
#Linear SVM Kernel
######################################################################################
#Making the model
#Linear formula = u'*v
model_linear = svm(label ~ ., data = training.data, kernel= "linear", scale = F)
#Showing the model summary
summary(model_linear)
#Making  confussion matrix
confusionMatrix_linear = table(True = testing.data[,1],
                               Pred = predict(model_linear, testing.data[,-1], type = "class"))
#Calculating the hit rate
hitRate_linear = sumDiag(confusionMatrix_linear) / nrow(testing.data) * 100
#Calculating the miss rate
missRate_linear = 100 - hitRate_linear
#Showinng rates
hitRate_linear
missRate_linear
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
#Making the model
model_polynomial = svm(label ~ .,
                       data = training.data,
                       kernel = "polynomial",
                       scale = F,
                       cost = parameters_polynomial$best.model$cost,
                       degree = parameters_polynomial$best.model$degree,
                       gamma = parameters_polynomial$best.model$gamma,
                       coef0 = parameters_polynomial$best.model$coef0)
#Making confussion matrix
confusionMatrix_polynomial = table(True = testing.data[,1],
                                   Pred = predict(model_polynomial, testing.data[,-1], type = "class"))
#Calculating the hit rate
hitRate_polynomial = sumDiag(confusionMatrix_polynomial) / nrow(testing.data) * 100
#Calculating the miss rate
missRate_polynomial = 100 - hitRate_polynomial
#Showing rates
hitRate_polynomial
missRate_polynomial
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
#Making the model
model_radial = svm(label ~ .,
                   data = training.data,
                   kernel = "radial",
                   scale = F,
                   gamma = parameters_radial$best.model$gamma)

#Making confussion matrix
confusionMatrix_radial = table(True = testing.data[,1],
                               Pred = predict(model_radial, testing.data[,-1], type = "class"))
#Calculating the hit rate
hitRate_radial = sumDiag(confusionMatrix_radial) / nrow(testing.data) * 100
#Calculating the miss rate
missRate_radial = 100 - hitRate_radial
#Showing rates
hitRate_radial
missRate_radial
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
#Making the model
model_sigmoid = svm(label ~ .,
                    data = training.data,
                    kernel = "sigmoid",
                    scale = F,
                    gamma = 10^(-5),
                    coef0 = 0)
#Making confussion matrix
confusionMatrix_sigmoid = table(True = testing.data[,1],
                                Pred = predict(model_sigmoid, testing.data[,-1], type = "class"))
#Calculating the hit rate
hitRate_sigmoid = sumDiag(confusionMatrix_sigmoid) / nrow(testing.data) * 100
#Calculating the miss rate
missRate_sigmoid = 100 - hitRate_sigmoid
#Showing rates
hitRate_sigmoid
missRate_sigmoid
######################################################################################
#Making by:
#Eric Bellet
#Deyban Perez
#Leonardo Santella
######################################################################################
# "Maximum Effort" - Deadpool 
######################################################################################
#End
######################################################################################
