#Q (a)
#Load the mice, VIM and lattice Packages. 
library(mice)
library(VIM)
library(lattice)
#Read the file named "Homework2.csv".
LoanData = read.csv("Homework2.csv")
str(LoanData)
summary(LoanData)   
#Summary the data in file
#"Age" has 4 missing data
#"ParticipatesInCommunity" has 3 missing data
#"SocialFabric" has 2 missing data
#"MonthlyPayment" has 105 missing data

Simple = LoanData[c("Age","ParticipatesInCommunity","SocialFabric","MonthlyPayment")]
str(Simple)
summary(Simple)
#Select the indepedent variables together which have missing data

md.pattern(Simple)          #To see the pattern of the missing data
Simple_aggr = aggr(Simple, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(Simple), cex.axis=.4, gap=2, ylab=c("Proportion of missingness","Missingness Pattern"))
# Variables sorted by number of missings: 
#     Variable                    Count
# MonthlyPayment              0.448717949
# Age                         0.017094017
# ParticipatesInCommunity     0.012820513
# SocialFabric                0.008547009

# Visualize the missing data.
# The missing data in "MonthlyPayment" is 44.87%
# The missing data in "Age" is 1.71%
# The missing data in "ParticipatesInCommunity" is 1.28%
# The missing data in "SocialFabric" is 0.85%
# Therefore, "MonthlyPayment" has too many missing data points to useful

LoanData = LoanData[,-15]
str(LoanData)
summary(LoanData)
# Delete the "MonthlyPayment" from the "LoanData" data set.

md.pattern(LoanData)          #To see the pattern of the missing data
Simple_aggr = aggr(LoanData, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(Simple1), cex.axis=.4, gap=2, ylab=c("Proportion of missingness","Missingness Pattern"))
# Visualize the missing data. And it is better than before data set.


#Q (b)
# Load the ggplot Package.
library(ggplot2)
ggplot(LoanData, aes(x = MonthlyIncome, y = LoanSizeAvg, color = NotPayBackLoan)) + geom_point()
#scatterplot0 = qplot(x = MonthlyIncome, y = LoanSizeAvg, data = LoanData)
#scatterplot0 + geom_point(color = "red")
# Using ggplot to create a scatterplot "LoanSizeAvg as a function of MonthlyIncome".
# By using the point
# From the scatterplot, almost points are limited in 0 <= MonthlyIncome <=20000 and 0 <= LoanSiZeAvg <=20000. 
# Therefore, it still has four outliers. 
# When MonthlyIncome equals to 50000, but the LoanSizeAvg just equals to 9000. 
# Also, the MonthlyIncome is the same or smaller than 10000, but LoanSizeAvg has more than 30000. 
# These points are difficult to come true, so they are outlier in the whole scatterplot



#Q (c)
summary(LoanData)
str(LoanData)
# Check the instance before missing data 
Loan1 = na.omit(LoanData)       # Remove the missing data by "na.omit"
summary(Loan1)
str(Loan1)
# After remove the missing data, there are still 227 instances left with.
# Also there have 234 initial data set before removing the missing data.
# Therefore, it has deleted 7 instances together.

library(caTools)
set.seed(667)
spl = sample.split(Loan1$NotPayBackLoan, SplitRatio = 0.65)
LoanTrain = subset(Loan1, spl==TRUE)
LoanTest = subset(Loan1, spl==FALSE)
write.csv(LoanTrain,"LoanTrain.csv")
write.csv(LoanTest,"LoanTest.csv")
# Split the new data between a training set(65% of the data) and a testing set(35% of the data)
 str(LoanTrain)
 str(LoanTest)

LoanModel = glm(NotPayBackLoan ~ Sex + Age + FamilySize + YearsAtThisHome + MainHousehold + MonthlyIncome + MonthlyExpenses + ParticipatesInCommunity + SocialFabric + LoanOpinion +  LoanSizeAvg  +  UpfrontPaymentAvg  + LoanPeriodMonths, data = LoanTrain, family=binomial)
summary(LoanModel)
# Build up the logistic regression to predict the "NotPayBackloan"

# Refine the logistic regression
LoanModel = glm(NotPayBackLoan ~ Sex + Age + FamilySize + YearsAtThisHome + MainHousehold + MonthlyIncome + ParticipatesInCommunity + SocialFabric + LoanOpinion +  LoanSizeAvg  +  UpfrontPaymentAvg  + LoanPeriodMonths, data = LoanTrain, family=binomial)
summary(LoanModel)    #Delete the MonthlyExpenses because the pr is 0.87034

LoanModel = glm(NotPayBackLoan ~ Sex + Age + YearsAtThisHome + MainHousehold + MonthlyIncome + ParticipatesInCommunity + SocialFabric + LoanOpinion +  LoanSizeAvg  +  UpfrontPaymentAvg  + LoanPeriodMonths, data = LoanTrain, family=binomial)
summary(LoanModel)    #Delete the FamilySize because the pr is 0.82804

LoanModel = glm(NotPayBackLoan ~ Sex + Age + YearsAtThisHome + MonthlyIncome + ParticipatesInCommunity + SocialFabric + LoanOpinion +  LoanSizeAvg  +  UpfrontPaymentAvg  + LoanPeriodMonths, data = LoanTrain, family=binomial)
summary(LoanModel)    #Delete the MainHousehold because the pr is 0.81908

LoanModel = glm(NotPayBackLoan ~ 0 + Sex + Age + YearsAtThisHome + MonthlyIncome + ParticipatesInCommunity + SocialFabric + LoanOpinion +  LoanSizeAvg  +  UpfrontPaymentAvg  + LoanPeriodMonths, data = LoanTrain, family=binomial)
summary(LoanModel)    #Delete the Intercept because the pr is 0.71458

LoanModel = glm(NotPayBackLoan ~ 0 + Age + YearsAtThisHome + MonthlyIncome + ParticipatesInCommunity + SocialFabric + LoanOpinion +  LoanSizeAvg  +  UpfrontPaymentAvg  + LoanPeriodMonths, data = LoanTrain, family=binomial)
summary(LoanModel)    #Delete the SexM because the pr is 0.93850

LoanModel = glm(NotPayBackLoan ~ 0 + Age + MonthlyIncome + ParticipatesInCommunity + SocialFabric + LoanOpinion +  LoanSizeAvg  +  UpfrontPaymentAvg  + LoanPeriodMonths, data = LoanTrain, family=binomial)
summary(LoanModel)    #Delete the YearsAtThisHome because the pr is 0.81457

LoanModel = glm(NotPayBackLoan ~ 0 + Age + MonthlyIncome + ParticipatesInCommunity + SocialFabric + LoanOpinion +  LoanSizeAvg  +  UpfrontPaymentAvg, data = LoanTrain, family=binomial)
summary(LoanModel)    #Delete the LoanPeriodMonths because the pr is 0.45648

LoanModel = glm(NotPayBackLoan ~ 0 + Age + ParticipatesInCommunity + SocialFabric + LoanOpinion +  LoanSizeAvg  +  UpfrontPaymentAvg, data = LoanTrain, family=binomial)
summary(LoanModel)    #Delete the MonthlyIncome because the pr is 0.32555

LoanModel = glm(NotPayBackLoan ~ 0 + ParticipatesInCommunity + SocialFabric + LoanOpinion +  LoanSizeAvg  +  UpfrontPaymentAvg, data = LoanTrain, family=binomial)
summary(LoanModel)    #Delete the Age because the pr is 0.313862

LoanModel = glm(NotPayBackLoan ~ 0 + ParticipatesInCommunity + LoanOpinion +  LoanSizeAvg  +  UpfrontPaymentAvg, data = LoanTrain, family=binomial)
summary(LoanModel)    #Delete the SocialFabric because the pr is 0.32570

LoanModel = glm(NotPayBackLoan ~ 0 + LoanOpinion +  LoanSizeAvg  +  UpfrontPaymentAvg, data = LoanTrain, family=binomial)
summary(LoanModel)    #Delete the ParticipatesInCommunity because the pr is 0.444577
# After refine the logistic regression, all independent variables are significant at the 90% level or higher.
#  Coefficients:
#                      Estimate Std. Error z value Pr(>|z|)    
#  LoanOpinion       -0.4162032  0.0834831  -4.985 6.18e-07 ***
#  LoanSizeAvg        0.0003742  0.0001010   3.703 0.000213 ***
#  UpfrontPaymentAvg -0.0003880  0.0001167  -3.324 0.000888 *** 
# P(y=1) = 1 / (1 + e ^ -(-0.4162032 * LoanOpinion + 0.0003742 * LoanSizeAvg - 0.0003880 * UpfrontPaymentAvg))


#Q (d)
#install.packages("ROCR")
library(ROCR)
LoanPredictTrain = predict(LoanModel, type="response")
# Evaluate the model

# ROC for training set
ROCRpred = prediction(LoanPredictTrain, LoanTrain$NotPayBackLoan)
ROCCurve = performance(ROCRpred, "tpr", "fpr")
plot(ROCCurve)
plot(ROCCurve, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))  
# We add colors and labels corresponding to the threshold values

# AUC for training set
as.numeric(performance(ROCRpred, "auc")@y.values) # The AUC result equal to 0.8710768
# After selecting the threshold "t", 0.2 is better and recommended
table(LoanTrain$NotPayBackLoan, LoanPredictTrain > 0.2)
#     FALSE TRUE
# 0    71    38
# 1    2     36

OutofSampleAcc = (71 + 36)/(71 + 2 + 38 + 36)
BaselineAcc = (71 + 38)/(71 + 2 + 38 + 36)
OutofSampleAcc
BaselineAcc 
# OutofSampleAcc = 0.7278912 = 72.79%
# BaselineAcc = 0.7414966 = 74.15%


#Q (e)
# ROC for testing set
LoanPredictTest = predict(LoanModel, type="response", newdata=LoanTest)
table(LoanTest$NotPayBackLoan, LoanPredictTest > 0.2)
ROCRpred2 = prediction(LoanPredictTest, LoanTest$NotPayBackLoan)
ROCCurve2 = performance(ROCRpred2, "tpr", "fpr")
plot(ROCCurve2)
plot(ROCCurve2, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))

# AUC for testing set
as.numeric(performance(ROCRpred2, "auc")@y.values) # The AUC result equal to 0.6826087
table(LoanTest$NotPayBackLoan, LoanPredictTest > 0.2)
OutofSampleAcc1 = (33 + 18)/(33 + 3 + 26 + 18)
BaselineAcc1 = (33 + 26)/(33 + 3 + 26 + 18)
OutofSampleAcc1
BaselineAcc1
# OutofSampleAcc1 = 0.6375 = 63.75%
# BaselineAcc1 = 0.7375 = 73.75%
table(LoanTest$NotPayBackLoan, LoanPredictTest > 0.3)
OutofSampleAcc4 = (49 + 11)/(49 + 10 + 10 +11)
BaselineAcc4 = (49 + 10)/(49 + 10 + 10 +1)
OutofSampleAcc4
BaselineAcc4
# OutofSampleAcc4 = 0.75 = 75%
# BaselineAcc4 = 0.8428571 = 84.29%
# If t is still 0.2, then the ROC in test set is not satisfied with my test model, because t = 0.3 is better than t = 0.2 
# If t is 0.3, it will be more close to True Positive Rate = 1 and the Out of Sample Accuracy is higher than t = 0.2 


#Q(f)
library(mice)
Simple = LoanData[c("Age","ParticipatesInCommunity","SocialFabric")]
str(Simple)
summary(Simple)
#Delete the MonthlyPayment from the simple data set
md.pattern(Simple)
# Check the missing data again

imp = mice(Simple, m=5, printFlag=FALSE, maxit = 30, seed=80)
# Set up the replicate 5 times and reiterate 30 times, seed is 80 that the variables can be random the same value.

imputed = complete(imp)
summary(imputed)
Loan2 = LoanData
str(Loan2)
summary(Loan2)
#Create a new loan to receive the new missing data from imp

Loan2$Age = imputed$Age
Loan2$ParticipatesInCommunity = imputed$ParticipatesInCommunity
Loan2$SocialFabric = imputed$SocialFabric
summary(Loan2)
#fit.mi = with(data=imp, exp = lm(NotPayBackLoan ~ SocialFabric +  LoanSizeAvg  + LoanPeriodMonths +  MonthlyPayment))
# Using function with(), and including an expression for the statistical analysis approach

#combFit = pool(fit.mi) 
# Combine the 5 results by pool function

#round(summary(combFit),4)
# Printing out summary coefficients with all numbers having only 4 decimals for greater readability

set.seed(999)
spl = sample.split(Loan2$NotPayBackLoan, SplitRatio = 0.65)
LoanTrain2 = subset(Loan2, spl==TRUE)
LoanTest2 = subset(Loan2, spl==FALSE)
write.csv(LoanTrain2,"LoanTraining2.csv")
write.csv(LoanTest2,"LoanTesting2.csv")
# Split the new data between a training set(65% of the data) and a testing set(35% of the data)
str(LoanTrain2)
summary(LoanTrain2)
str(LoanTest2)
summary(LoanTest2)

# Build up the new logistic regression
LoanModel1 = glm(NotPayBackLoan ~ Sex + Age + FamilySize + YearsAtThisHome + MainHousehold + MonthlyIncome + MonthlyExpenses + ParticipatesInCommunity + SocialFabric + LoanOpinion +  LoanSizeAvg  +  UpfrontPaymentAvg  + LoanPeriodMonths, data = LoanTrain2, family=binomial)
summary(LoanModel1)

#Refine the logistic regression
LoanModel1 = glm(NotPayBackLoan ~ Sex + Age + FamilySize + MainHousehold + MonthlyIncome + MonthlyExpenses + ParticipatesInCommunity + SocialFabric + LoanOpinion +  LoanSizeAvg  +  UpfrontPaymentAvg  + LoanPeriodMonths, data = LoanTrain2, family=binomial)
summary(LoanModel1)      #Delete the YearsAtThisHome because the pr is 0.776

LoanModel1 = glm(NotPayBackLoan ~ Sex + Age + FamilySize + MainHousehold + MonthlyIncome + MonthlyExpenses + SocialFabric + LoanOpinion +  LoanSizeAvg  +  UpfrontPaymentAvg  + LoanPeriodMonths, data = LoanTrain2, family=binomial)
summary(LoanModel1)      #Delete the ParticipatesInCommunity because the pr is 0.745

LoanModel1 = glm(NotPayBackLoan ~ Sex + Age + FamilySize + MainHousehold + MonthlyIncome + MonthlyExpenses + SocialFabric +  LoanSizeAvg  +  UpfrontPaymentAvg  + LoanPeriodMonths, data = LoanTrain2, family=binomial)
summary(LoanModel1)      #Delete the LoanOpinion because the pr is 0.655

LoanModel1 = glm(NotPayBackLoan ~ 0 + Sex + Age + FamilySize + MainHousehold + MonthlyIncome + MonthlyExpenses + SocialFabric +  LoanSizeAvg  +  UpfrontPaymentAvg  + LoanPeriodMonths, data = LoanTrain2, family=binomial)
summary(LoanModel1)      #Delete the Intercept because the pr is 0.5919

LoanModel1 = glm(NotPayBackLoan ~ 0 + Age + FamilySize + MainHousehold + MonthlyIncome + MonthlyExpenses + SocialFabric +  LoanSizeAvg  +  UpfrontPaymentAvg  + LoanPeriodMonths, data = LoanTrain2, family=binomial)
summary(LoanModel1)      #Delete the SexF because the pr is 0.5919

LoanModel1 = glm(NotPayBackLoan ~ 0 + Age + FamilySize + MainHousehold + MonthlyExpenses + SocialFabric +  LoanSizeAvg  +  UpfrontPaymentAvg  + LoanPeriodMonths, data = LoanTrain2, family=binomial)
summary(LoanModel1)      #Delete the MonthlyIncome because the pr is 0.5245

LoanModel1 = glm(NotPayBackLoan ~ 0 + Age + FamilySize + MainHousehold + SocialFabric +  LoanSizeAvg  +  UpfrontPaymentAvg  + LoanPeriodMonths, data = LoanTrain2, family=binomial)
summary(LoanModel1)      #Delete the MonthlyExpenses because the pr is 0.9047

LoanModel1 = glm(NotPayBackLoan ~ 0 + FamilySize + MainHousehold + SocialFabric +  LoanSizeAvg  +  UpfrontPaymentAvg  + LoanPeriodMonths, data = LoanTrain2, family=binomial)
summary(LoanModel1)      #Delete the Age because the pr is 0.3595

LoanModel1 = glm(NotPayBackLoan ~ 0 + MainHousehold + SocialFabric +  LoanSizeAvg  +  UpfrontPaymentAvg  + LoanPeriodMonths, data = LoanTrain2, family=binomial)
summary(LoanModel1)      #Delete the FamilySize because the pr is 0.11872

LoanModel1 = glm(NotPayBackLoan ~ 0 + SocialFabric +  LoanSizeAvg  +  UpfrontPaymentAvg  + LoanPeriodMonths, data = LoanTrain2, family=binomial)
summary(LoanModel1)      #Delete the MainHousehold because the pr is 0.169669

LoanModel1 = glm(NotPayBackLoan ~ 0 + SocialFabric +  LoanSizeAvg  +  UpfrontPaymentAvg, data = LoanTrain2, family=binomial)
summary(LoanModel1)      #Delete the LoanPeriodMonths because the pr is 0.126
# After refine the logistic regression, all independent variables are significant at the 90% level or higher.
# Coefficients:
#                     Estimate Std. Error z value Pr(>|z|)    
# SocialFabric      -0.9600098  0.1568813  -6.119 9.40e-10 ***
# LoanSizeAvg        0.0009225  0.0001635   5.643 1.68e-08 ***
# UpfrontPaymentAvg -0.0008873  0.0001638  -5.416 6.11e-08 ***
# P(y=1) = 1 / (1 + e ^ -(-0.9600098 * SocialFabric + 0.0009225 * LoanSizeAvg - 0.0008873 * UpfrontPaymentAvg))


#Q (g)
library(ROCR)
LoanPredictTrain1 = predict(LoanModel1, type="response")
# Evaluate the model

# ROC for training set
ROCRpred = prediction(LoanPredictTrain1, LoanTrain2$NotPayBackLoan)
ROCCurve = performance(ROCRpred, "tpr", "fpr")
plot(ROCCurve)
plot(ROCCurve, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))  #We add colors and labels corresponding to the threshold values

# AUC for training set
as.numeric(performance(ROCRpred, "auc")@y.values) # The AUC result equal to 0.880303
#After selecting the threshold "t", 0.2 is better and recommended
table(LoanTrain2$NotPayBackLoan, LoanPredictTrain1 > 0.2)
#      FALSE TRUE
# 0     91    19
# 1     5     37
OutofSampleAcc2 = (91 + 37)/(91 + 19 + 5 + 37)
BaselineAcc2 = (91 + 19)/(91 + 19 + 5 + 37)
OutofSampleAcc2
BaselineAcc2
# OutofSampleAcc2 = 0.8421053 = 84.21%
# BaselineAcc2 = 0.7236842 = 72.37%


#Q (h)
# ROC for testing set
LoanPredictTest1 = predict(LoanModel1, type="response", newdata=LoanTest2)
table(LoanTest2$NotPayBackLoan, LoanPredictTest1 > 0.2)
ROCRpred2 = prediction(LoanPredictTest1, LoanTest2$NotPayBackLoan)
ROCCurve2 = performance(ROCRpred2, "tpr", "fpr")
plot(ROCCurve2)
plot(ROCCurve2, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))

# AUC for testing set
as.numeric(performance(ROCRpred2, "auc")@y.values) # The AUC result equal to 0.8378788
table(LoanTest2$NotPayBackLoan, LoanPredictTest1 > 0.2)
OutofSampleAcc2 = (47 + 21)/(47 + 13 + 1 +21)
BaselineAcc2 = (47 + 13)/(47 + 13 + 1 +21)
OutofSampleAcc2
BaselineAcc2
# OutofSampleAcc1 = 0.8292683 = 82.93%
# BaselineAcc1 = 0.7317073 = 73.17%
# If t is 0.2, then the ROC in test set is satisfied with my test model
# It is more close to True Positive Rate = 1 And the Out of Sample Accuracy is the same as the training set 
# Therefore, comparing with the missing data ommited that out of sample accuracy is 75% and the Baseline accuracy is 84.29%,
# the missing data filled using mice that out of sample accuracy is 82.93% and the Baseline accuracy is 73.17%
# We can see that the missing data filled using mice, the out of sample accuracy is higher than ommited ones.
# Although its Baseline accuracy is a little lower than ommited ones, I would advocate using the model with the missing data filled using mice.



