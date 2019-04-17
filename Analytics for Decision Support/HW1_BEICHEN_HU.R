Stores = read.csv("Stores.csv")
str(Stores)
#install.packages("caTools")
library(caTools)
#install.packages("corrplot")
library(corrplot)
#Part.1 (a)


#Creating Training and Testing Sets
#install.packages("caTools")

set.seed(80)        #Set the the same random value in seed (80)
mean(Stores$STOR)     #mean(Stores$STOR) is 30.5 so we can divided it into Traning set 2/3 and Testing set 1/3)
splitratio = sample.split(Stores$STOR, SplitRatio = 2/3)      
#str(splitratio)

#splitratio
StoresTrain = subset(Stores,splitratio == TRUE)
StoresTest = subset(Stores,splitratio == FALSE)
write.csv(StoresTrain, "StoresTrain.csv")   #Export the Train set of EXCEL
write.csv(StoresTest, "StoresTest.csv")     #Export the Test set of EXCEL
str(StoresTest)       #Set up the test set
str(StoresTrain)      #Set up the training set

#Create the correlation matrix
cor(StoresTrain)
StoresTrain2 <- StoresTrain[,c(-1)]  #Delete the invalid column(Store Number) 
t = cor(StoresTrain2) 
z <- t[-c(1,0),] #Delete the first row, independent variable (EARN)
which(t[,1]==max(z[,1]) , arr.ind = TRUE)
max(z[,1])
#So we get that "P15" is the most positively correlated with dependet variable "EARN" , is 0.6215327

#Part.1 (b)
#Make the Plots graph
plot(StoresTrain$EARN ~ StoresTrain$P15)


#Part.1 (c)
#Creat the Training Set simple linear regression model
StoresSimpleModel = lm (EARN ~ P15, data = StoresTrain)
summary(StoresSimpleModel)    #R^2 is 0.3863 (That is not great, I am not satisfied with it)
#Refine the Training Set simple linear regression model
StoresSimpleModel = lm (EARN ~ 0 + P15, data = StoresTrain)
summary(StoresSimpleModel)    #Set up 0 to Intercept because of the p-value is 0.889 (bad)
#R^2 is 0.8202, and the linear regression equation is EARN = 0.048590*P15


#Part.1 (d)
#Make a prediction by using StoreTest set
StoresPrediction2 = predict(StoresSimpleModel, newdata = StoresTest)
#str(StoresPrediction)
#summary(StoresPrediction)
#Compute the R^2 of this model
SSE = sum((StoresTest$EARN - StoresPrediction2)^2)
SST = sum((StoresTest$EARN - mean(StoresTrain$EARN))^2)
RS = 1 - SSE/SST
RS
#The R^2 is 0.4112391, I am not satisfied with this model


#Part1. (e)
#Read the file "PotentialStores.csv"
PotentialStores = read.csv("PotentialStores.csv")
str(PotentialStores)
#Predict the potential store Earning 
StoresPrediction1 = predict(StoresSimpleModel, newdata = PotentialStores)
summary(StoresPrediction1)
str(StoresPrediction1)
#Add the value into EARN column
PotentialStores$EARN = c(StoresPrediction1)
#PotentialStores$EARN
RadioEtoC = PotentialStores$EARN / PotentialStores$K    #The radio of predicted operating earnings to capital
table(PotentialStores$STOR , RadioEtoC > 0.26)     # Select the store that radio higher than 0.26
#There is no suitable store which the radio of predicted operating earnings to capital higher than 0.26


#Part.1 (f) (i)
#Creat the Training Set model
StoresModel = lm (EARN ~ SIZE + EMPL + total + P15 + P25 + P35 + P45 + P55 + INC + COMP + NCOMP + NREST + PRICE + CLI, data = StoresTrain)
summary(StoresModel)      
#R^2 is 0.8816 
#The "Intercept" is statistically significant, upper 95.0%
#The "SIZE" is statistically significant, upper 99.9%
#The "INC" is statistically significant, upper 99.9%
#The "NREST" is statistically significant, upper 99.9%
#The "PRICE" is statistically significant, upper 90.0%

#Part.1 (f) (ii)
#Refine the Training Set model
StoresModel = lm (EARN ~ SIZE + EMPL + total + P15 + P25 + P35 + P45 + P55 + INC + COMP + NREST + PRICE + CLI, data = StoresTrain)
summary(StoresModel)       #Delete NCOMP , the highest pr-value is 0.854071
StoresModel = lm (EARN ~ SIZE + total + P15 + P25 + P35 + P45 + P55 + INC + COMP + NREST + PRICE + CLI, data = StoresTrain)
summary(StoresModel)     #Delete EMPL , the highest pr-value is 0.784214
StoresModel = lm (EARN ~ SIZE + total + P15 + P25 + P35 + P55 + INC + COMP + NREST + PRICE + CLI, data = StoresTrain)
summary(StoresModel)       #Delete P45, the highest pr-value is 0.699016
StoresModel = lm (EARN ~ SIZE + total + P15 + P25 + P35 + P55 + INC + NREST + PRICE + CLI, data = StoresTrain)
summary(StoresModel)      #Delete COMP, the highest pr-value is 0.70216
StoresModel = lm (EARN ~ SIZE + total + P15 + P25 + P55 + INC + NREST + PRICE + CLI, data = StoresTrain)
summary(StoresModel)      #Delete P35, the highest pr-value is 0.61220
StoresModel = lm (EARN ~ SIZE + total + P15 + P25 + P55 + INC + NREST + PRICE, data = StoresTrain)
summary(StoresModel)      #Delete CLI, the highest pr-value is 0.52312
StoresModel = lm (EARN ~ SIZE + P15 + P25 + P55 + INC + NREST + PRICE, data = StoresTrain)
summary(StoresModel)      #Delete total, the highest pr-value is 0.3257
StoresModel = lm (EARN ~ SIZE + P15 + P25 + INC + NREST + PRICE, data = StoresTrain)
summary(StoresModel)      #Delete P55, the highest pr-value is 0.529
StoresModel = lm (EARN ~ SIZE + P15 + INC + NREST + PRICE, data = StoresTrain)
summary(StoresModel)      #Delete P25, the highest pr-value is 0.255
StoresModel = lm (EARN ~ SIZE + P15 + INC + NREST, data = StoresTrain)
summary(StoresModel)      #Delete PRICE, the highest pr-value is 0.0287 which is lower than 99%
StoresModel
# The final linear regression equation is EARN = -4.385e+02 + 8.528e-01*SIZE + 4.204e-02*P15 + 9.978e+00*INC + 1.444e+00*NREST
# The R^2 is 0.8457

#Part.1 (f) (iii)
summary(StoresSimpleModel) #p-value: 4.116e-16
summary(StoresModel)       #p-value: 9.932e-14
#both of them has the independent variable P15, so the independent variable "P15" in simple linear regression model correlated with mulitple independent variables in linear regression

#Part.1 (g)

#Make a prediction by using Store Test set
StoresPrediction = predict(StoresModel, newdata = StoresTest)
#str(StoresPrediction)
#summary(StoresPrediction)
#Compute the R^2 of this model
SSE = sum((StoresTest$EARN - StoresPrediction)^2)
SST = sum((StoresTest$EARN - mean(StoresTrain$EARN))^2)
RS = 1 - SSE/SST
RS
#The R^2 is 0.7525086, I think it is better than simple linear regression before. 


#Part.1 (e)
#Read the file "PotentialStores.csv"
PotentialStores = read.csv("PotentialStores.csv")
str(PotentialStores)
#Predict the potential store Earning 
StoresPrediction0 = predict(StoresModel, newdata = PotentialStores)
summary(StoresPrediction0)
str(StoresPrediction0)
#Add the value into EARN column
PotentialStores$EARN = c(StoresPrediction0)
#PotentialStores$EARN
RadioEtoC = PotentialStores$EARN / PotentialStores$K    #The radio of predicted operating earnings to capital
table(PotentialStores$STOR , RadioEtoC > 0.26)     # Select the store that radio higher than 0.26
#The suitable store is D and J, it is different from the stores in (e)

