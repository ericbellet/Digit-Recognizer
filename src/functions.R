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
  image.data = matrix(data.matrix(dataSet[row, 1:ncol(dataSet)]), 28, 28)
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

#function made to test the models
testModel = function(row, dataSet)
{
  plotDigit(row, dataSet)
  write(sprintf("Modelo lineal: %s", as.matrix(predict(model_linear, dataSet[row,]))[1][1]), stdout())
  linear <- as.matrix(predict(model_linear, dataSet[row,]))[1][1]
  
  write(sprintf("Modelo polinomico: %s", as.matrix(predict(model_polynomial, dataSet[row,]))[1][1]), stdout())
  polynomial <- as.matrix(predict(model_polynomial, dataSet[row,]))[1][1]
  
  write(sprintf("Modelo radial: %s", as.matrix(predict(model_radial, dataSet[row,]))[1][1]), stdout())
  radial <- as.matrix(predict(model_radial, dataSet[row,]))[1][1]
  
  write(sprintf("Modelo sigmoid: %s", as.matrix(predict(model_sigmoid, dataSet[row,]))[1][1]), stdout())
  sigmoid <- as.matrix(predict(model_sigmoid, dataSet[row,]))[1][1]
  
  write(sprintf("Segun el esquema de votacion el numero es: %s", names(sort(summary(as.factor(c(linear, polynomial, radial, sigmoid))), decreasing=T)[1])), stdout())

  #return(c(linear, polynomial, radial, sigmoid))
  
}