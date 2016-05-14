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
test = read.csv("data/test.csv")
######################################################################################
#Layout to test models
######################################################################################
load(file = "models/model_linear.rda")
load(file = "models/model_polynomial.rda")
load(file = "models/model_radial.rda")
load(file = "models/model_sigmoid.rda")

testModel(1190, test)
######################################################################################
#Made by:
#Eric Bellet
#Deyban Perez
#Leonardo Santella
######################################################################################
# "Maximum Effort" - Deadpool 
######################################################################################
#End
######################################################################################