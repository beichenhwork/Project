Data = read.csv("Network_PUF_2018_11.5.csv")
str(Data)

#install.packages("dplyr")
library(caTools)
library(dplyr)

#Delete the Dental Only Plan with "" 
Data2 = filter(Data, DentalOnlyPlan != "")
str(Data2)

#Translate the factor as number and integer
D1 = ifelse (Data2$DentalOnlyPlan == "Yes",1,0)
# So we predict if it is dental only plan as "1", else is "0".
Data2$DentalOnlyPlan = as.integer(D1)
str(Data2)
Data2$StateCode = as.numeric(Data2$StateCode)
Data2$SourceName = as.numeric(Data2$SourceName)
Data2$ImportDate = as.numeric(Data2$ImportDate)
Data2$NetworkName = as.numeric(Data2$NetworkName)
Data2$NetworkId = as.numeric(Data2$NetworkId)
Data2$NetworkURL = as.numeric(Data2$NetworkURL)
Data2$MarketCoverage = as.numeric(Data2$MarketCoverage)
str(Data2)

#Set up the training set and testing set
set.seed(66)
spl = sample.split(Data2$DentalOnlyPlan,SplitRatio = 0.75)
Trainset = subset(Data2, spl == TRUE)
Testset = subset(Data2, spl == FALSE)

#Predict model
Model = glm(DentalOnlyPlan ~ StateCode + IssuerId + SourceName + ImportDate + NetworkName + NetworkId + NetworkURL + MarketCoverage, data = Trainset,family = binomial)
summary(Model)

#Refine the model
Model = glm(DentalOnlyPlan ~ StateCode + IssuerId + ImportDate + NetworkName + NetworkId + NetworkURL + MarketCoverage, data = Trainset,family = binomial)
summary(Model) # Delete SourceName
Model = glm(DentalOnlyPlan ~ StateCode + IssuerId + ImportDate + NetworkName + NetworkId + MarketCoverage, data = Trainset,family = binomial)
summary(Model) # Delete NetworkURL
Model = glm(DentalOnlyPlan ~ 0 + StateCode + IssuerId + ImportDate + NetworkName + NetworkId + MarketCoverage, data = Trainset,family = binomial)
summary(Model) # Delete Intercept
Model = glm(DentalOnlyPlan ~ 0 + StateCode + ImportDate + NetworkName + NetworkId + MarketCoverage, data = Trainset,family = binomial)
summary(Model) # Delete IssuerId
#Refine finish, and all varibles significant is higher than 99%

library(ROCR)
PredictTrain = predict(Model, type="response") 
#AUC for training set
ROCRpred = prediction(PredictTrain, Trainset$DentalOnlyPlan) #this transforms input data into a standardized format
ROCCurve = performance(ROCRpred, "tpr", "fpr") #computes performance measures true positive rate (tpr) and false positive rate (fpr)
plot(ROCCurve)
plot(ROCCurve, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
# The Threshold is 0.5 is better because the ROC shows mostly a vertical drop after t=0.5, 
#meaning we decrease the TPR without any meaningful improvement on the FPR so it doesn’t make sense to pick t>0.5, 
#and the curve for t<=0.5 shows a horizontal drop to t=0.5 showing we improve (decrease) FPR without hurting TPR 
#so it doesn’t make sense to select t<0.5.
as.numeric(performance(ROCRpred, "auc")@y.values) # AUC = 0.8203274
table(Trainset$DentalOnlyPlan, PredictTrain > 0.5)
#   FALSE TRUE
#0    88   33
#1    38  118
TrainAccuracy = (88 + 118)/ (88+33+38+118)
TrainAccuracy # = 0.7436823


#AUC for testing set
PredictTest = predict(Model, type="response", newdata = Testset)
table(Testset$DentalOnlyPlan, PredictTest > 0.5)
#   FALSE TRUE
#0    29   11
#1    14   38
ROCRpred2 = prediction(PredictTest, Testset$DentalOnlyPlan)
ROCCurve2 = performance(ROCRpred2, "tpr", "fpr")
plot(ROCCurve2)
plot(ROCCurve2, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(ROCRpred2, "auc")@y.values) # AUC = 0.7860577
# Accurarcy in test set 
TestAccuracy = (29 + 38)/(29 + 11 + 14 +38)
TestAccuracy # = 0.7282609
