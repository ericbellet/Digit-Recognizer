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
######################################################################################
#Installing packages depend
install("e1071")
######################################################################################
#Importing libraries
library(e1071)
######################################################################################
#Loading data set
setwd("C:/Users/deyban.perez/Dropbox/9no_semestre/Mineria/Project")
train = read.csv("train.csv")
test = read.csv("test.csv")
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
set.seed(22)
subIndex = sample(nrow(train), floor(nrow(train)* 0.8),
                  prob = vectorProbabilities.data, replace = F)
training.data = train[subIndex, ]
testing.data = train[-subIndex, ]
######################################################################################
model_linear = svm(formula = label ~ .,
                  data = training.data,
                  kernel = "linear")

model_polynomial = tune.svm(formula = label ~ .,
                         data = training.data,
                         kernel = "polynomial",
                         gamma = 10^(-6:-1),
                         cost = 10^(-1:1))

summary(model)
