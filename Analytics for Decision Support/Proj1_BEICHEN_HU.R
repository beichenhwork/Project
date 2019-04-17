Clickset = read.csv("emis7357proj1fl18-100ids.csv")
# Read the file and transfer the factor as int or char
# Using the smaller file because of the laptop
summary(Clickset)
str(Clickset)
# Transport the factor as number in these several independent varibles
Clickset$site_id=as.numeric(Clickset$site_id)
Clickset$site_domain=as.numeric(Clickset$site_domain)
Clickset$site_category=as.numeric(Clickset$site_category)
Clickset$app_id=as.numeric(Clickset$app_id)
Clickset$app_domain=as.numeric(Clickset$app_domain)
Clickset$app_category=as.numeric(Clickset$app_category)
Clickset$device_id=as.numeric(Clickset$device_id)
Clickset$device_ip=as.numeric(Clickset$device_ip)
Clickset$device_model=as.numeric(Clickset$device_model)

str(Clickset)


##### Split the data in training, validation and testing set.
##### Introduction, (b)
library(caTools)
set.seed(80)        #Set the the same random value in seed (80)
splitratio = sample.split(Clickset$click, SplitRatio = 2/3)      
#str(splitratio)

#splitratio
ClickTrain = subset(Clickset,splitratio == TRUE)
ClickVT = subset(Clickset,splitratio == FALSE)    #ClickVT means Validation and Test together at first
#split the train and vt firstly.
set.seed(666667)
splitratio1 = sample.split(ClickVT$click, SplitRatio = 1/2)
#change the ratio by ClickVT
ClickValidation = subset(ClickVT,splitratio1 == TRUE)
ClickTest = subset(ClickVT,splitratio1 == FALSE)
#Finally, split the set into training (2/3 of data), 
#validation (1/6 of data)
#testing sets (1/6 of data)
str(Clickset)
str(ClickTrain)
str(ClickValidation)
str(ClickTest)

write.csv(ClickTrain, "ClickTrain.csv")   #Export the Train set of EXCEL
write.csv(ClickValidation, "ClickValidation.csv")     #Export the Validation set of EXCEL
write.csv(ClickTest, "ClickTest.csv")     #Export the Test set of EXCEL
##### Finish the spliting data set.


#####Average click-through rate
# Click train click-through rate
CTRClickTrain = (sum(ClickTrain$click==TRUE)) / nrow(ClickTrain)
# Click validation click-through rate
CTRClickValidation = (sum(ClickValidation$click==TRUE)) / nrow(ClickValidation)
# Click test click-through rate
CTRClickTest = (sum(ClickTest$click==TRUE)) / nrow(ClickTest)
# Average of Click-through Rate
AverageCTR = (CTRClickTest + CTRClickTrain + CTRClickValidation) / 3
# CTRClickTrain = 0.1686866 = 16.86866%
# CTRClickTest = 0.168706 = 16.8706%
# CTRClickValidation = 0.1686672 = 16.86672%
# AverageCTR = 0.1686866 = 16.86866%


##### Descriptive analytics
library(ggplot2)

#####Histogram
#hist(Clickset$id) 
#Finish in the tableau

#####Boxplot
#boxplot(Clickset$click ~ Clickset$site_category, 
#        ylab = "Click", 
#        xlab = "Site Category", 
#        main = "Ralation between site and click")
#Finish in the tableau

#####Scatterplot
ggplot(Clickset, aes(x = site_category, 
                     y =  banner_pos,
                     color = as.factor(click))) + geom_point()
#From the scatterplot, we can see according to the click,
#we get the relationship between site_category and banner_pos
#So 

ggplot(Clickset, aes(x = id, 
                     y = app_category,
                     color = as.factor(click))) + geom_point()

ggplot(Clickset, aes(x = id, 
                     y = site_category,
                     color = as.factor(click))) + geom_point()

#####Logistic regression
#Because of the factor in site_id, site_domain, site_category,
#app_id, app_domain, app_category,
#device_id, device_ip, device_model, there exists some problems liked: Error: vector memory exhaused
#Therefore, We need to transport the factor as a numeric by using "as.numeric()" at begining reading file process
#And then we can do the logistic regression 
ClickModel = glm(click ~ C1 + banner_pos + site_id + site_domain
                 + site_category + app_id + app_domain + 
                   app_category + device_id + device_ip + 
                   device_model + device_type + device_conn_type + 
                   C14 + C15 + C16 + C17 + C18 + C19 + C20 + C21, 
                 data = ClickTrain, family=binomial)
summary(ClickModel)

#Refine the logistic regression
ClickModel = glm(click ~ C1 + banner_pos + site_id + site_domain
                 + site_category + app_id + app_domain + 
                   app_category + device_ip + 
                   device_model + device_type + device_conn_type + 
                   C14 + C15 + C16 + C17 + C18 + C19 + C20 + C21, 
                 data = ClickTrain, family=binomial)  # delete the device_id
summary(ClickModel)
ClickModel = glm(click ~ C1 + banner_pos + site_id + site_domain
                 + site_category + app_id + app_domain + 
                   app_category + device_ip + 
                  + device_type + device_conn_type + 
                   C14 + C15 + C16 + C17 + C18 + C19 + C20 + C21, 
                 data = ClickTrain, family=binomial)  # delete the device_model
summary(ClickModel)
ClickModel = glm(click ~ C1 + banner_pos + site_domain
                 + site_category + app_id + app_domain + 
                   app_category + device_ip + 
                   + device_type + device_conn_type + 
                   C14 + C15 + C16 + C17 + C18 + C19 + C20 + C21, 
                 data = ClickTrain, family=binomial)  # delete the site_id
summary(ClickModel)
ClickModel = glm(click ~ banner_pos + site_domain
                 + site_category + app_id + app_domain + 
                   app_category + device_ip + 
                   + device_type + device_conn_type + 
                   C14 + C15 + C16 + C17 + C18 + C19 + C20 + C21, 
                 data = ClickTrain, family=binomial)  # delete the C1
summary(ClickModel)
ClickModel = glm(click ~ 0 + banner_pos + site_domain
                 + site_category + app_id + app_domain + 
                   app_category + device_ip + 
                   + device_type + device_conn_type + 
                   C14 + C15 + C16 + C17 + C18 + C19 + C20 + C21, 
                 data = ClickTrain, family=binomial)  # delete the Intercept
summary(ClickModel)
# Finish and all indepedent variables are statistically significant at the level of your choice (90%, 95%, 99%, 99.9%)

#install.packages("ROCR")
library(ROCR)
PredictClickTrain = predict(ClickModel, type="response")
# Evaluate the model

# ROC for training set
ROCRpred = prediction(PredictClickTrain, ClickTrain$click)
ROCCurve = performance(ROCRpred, "tpr", "fpr")
plot(ROCCurve)
plot(ROCCurve, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))  
# We add colors and labels corresponding to the threshold values

# AUC for training set
as.numeric(performance(ROCRpred, "auc")@y.values) # The AUC result equal to 0.6404609
# After selecting the threshold "t", 0.2 is better and recommended
# Because the ROC shows mostly a vertical drop after t=0.2, and we decide to pick up the TPR above 0.4
#and the FPR below 0.4, which means we guess correctly more often than wrong, both when actually 0 was
#realized and when actually 1 was realized. 
#Although if t = 0.1 the TPR is higher than 0.2, the worse FPR it is. So we waive t = 0.1
table(ClickTrain$click, PredictClickTrain > 0.2)
#      FALSE  TRUE
# 0    63019  22859
# 1    9701   7725

OutofSampleAcc = (63019 + 7725)/(63019+22859+9701+7725)
BaselineAcc = (63019+22859)/(63019+22859+9701+7725)
CTRLogR = (22859 + 7725) / (63019+22859+9701+7725)
OutofSampleAcc # The accuracy is 0.6848138
BaselineAcc   # The baseline is 0.8313134
CTRLogR   # The click-through rate is 0.2960582


##### Classfication trees
#Output to the file
sink("ClickClassficationTrees.txt",append=TRUE,split=TRUE)
library(rpart)
library(rpart.plot)
ClickTree50 = rpart(click ~ C1 + banner_pos + site_id + site_domain
                    + site_category + app_id + app_domain + 
                      app_category + device_id + device_ip + 
                      device_model + device_type + device_conn_type + 
                      C14 + C15 + C16 + C17 + C18 + C19 + C20 + C21,
                     data = ClickTrain, method="class",  minbucket = 50, cp = -1)
summary(ClickTree50)
# Initial the minbucket = 50 at first. 
# Because of the huge data, we don't graph at beginning. And we need to pick up the "cp" to refine the classification tree
# And then we need to pick up the suitable cp value accoring to the first tree 
# Then we can see correct tree according to the cp value in training set 
# which can reduce some overfit problem in validation and testing set
plotcp(ClickTree50) # cp = 0.00096 is the best

# Refine the tree by cp = 0.00096 without the minbucket
ClickTree = rpart(click ~ C1 + banner_pos + site_id + site_domain
                    + site_category + app_id + app_domain + 
                      app_category + device_id + device_ip + 
                      device_model + device_type + device_conn_type + 
                      C14 + C15 + C16 + C17 + C18 + C19 + C20 + C21,
                    data = ClickTrain, method="class", cp = 0.00096)
summary(ClickTree)
# Learning how to graph a tree
prp(ClickTree)
# and if we really need to output numbers at the prompt
rpart.plot(ClickTree)
print(ClickTree)
# Close the "Trees.txt" file
sink()
#file.show("Trees.txt")

# Make prediction on training set
PredictClickTree = predict(ClickTree, type = "class")
MyClickTree=table(ClickTrain$click,PredictClickTree)
MyClickTree
AccuTrain=(MyClickTree[1,1]+MyClickTree[2,2])/sum(MyClickTree)
CTRClassiTree = (MyClickTree[1,2] + MyClickTree[2,2])/sum(MyClickTree)
AccuTrain # The accuracy is 0.8329784
CTRClassiTree # The click-through rate is 0.008228142



##### Random forests
library(randomForest)
# Build random forest model
#At first it appear an error if (!ClickTrain && length(unique(y)) <= 5) {
#warning("The response has five or fewer unique values. Are you sure you want to do regression?")}
# It states that the "y" or click value's unique is less than 5, which means that we need to use R to classify the "y" value at first
# So I use factor(click) to classify the y value. 

ClickRF = randomForest(factor(click) ~ C1 + banner_pos + site_id + site_domain
                                  + site_category + app_id + app_domain + 
                                    app_category + device_id + device_ip + 
                                    device_model + device_type + device_conn_type + 
                                    C14 + C15 + C16 + C17 + C18 + C19 + C20 + C21,
                                  data = ClickTrain, ntree=200, nodesize=25)
summary(ClickRF)
PredictClickRF = predict(ClickRF , data = ClickTrain)
p = table(ClickTrain$click, PredictClickRF)
AccTrainRF = (p[1,1] + p[2,2]) / sum(p)
CTRRandomF = (p[1,2] + p[2,2]) / sum(p)
AccTrainRF  # The accuracy is 0.833685
CTRRandomF   # The click-through rate is 0.01362




#####Improving models
## Introducing dummy varibales
#Using test format
#z = strptime(14102914,format='%y%m%d%H')
#t = strftime(z, '%p')
#t
# So I pick up a new sebset from training which is the most often advertisements in training set. 
MostOften = subset(ClickTrain, ClickTrain$id > 1.00e+19)
#CTsmall = ClickTrain[sample(nrow(ClickTrain), 10000), ]
#str(CTsmall)
# Create an subset of the training set to improve my model

mydate = strptime(MostOften$hour,format='%y%m%d%H')
mydate  # Translate the date from '14102914' into ''

mydate1 = strftime(mydate, 'The time is: %p')
mydate1 # Translate the date from 'Year-Month-Day Hour:Minute:Second' into AM or PM
# Rewrite the date into the training set
MostOften$hour = c(mydate1)
str(MostOften)
# Do a new data set in analytic Logistic Regression
ClickModelMostOften = glm(click ~ hour + C1 + banner_pos + site_id + site_domain
                 + site_category + app_id + app_domain + 
                   app_category + device_id + device_ip + 
                   device_model + device_type + device_conn_type + 
                   C14 + C15 + C16 + C17 + C18 + C19 + C20 + C21, 
                 data = MostOften, family=binomial)
summary(ClickModelMostOften)
# Refine the Logistic Regression
ClickModelMostOften = glm(click ~ hour + C1 + banner_pos + site_id + site_domain
                          + site_category + app_id + app_domain + 
                            app_category + device_id + device_ip 
                             + device_type + device_conn_type + 
                            C14 + C15 + C16 + C17 + C18 + C19 + C20 + C21, 
                          data = MostOften, family=binomial)
summary(ClickModelMostOften) # Delete the device_model the pr is 0.32402
ClickModelMostOften = glm(click ~ C1 + banner_pos + site_id + site_domain
                          + site_category + app_id + app_domain + 
                            app_category + device_id + device_ip 
                          + device_type + device_conn_type + 
                            C14 + C15 + C16 + C17 + C18 + C19 + C20 + C21, 
                          data = MostOften, family=binomial)
summary(ClickModelMostOften) # Delete the hour the pr is 0.30657
ClickModelMostOften = glm(click ~ C1 + banner_pos + site_domain
                          + site_category + app_id + app_domain + 
                            app_category + device_id + device_ip 
                          + device_type + device_conn_type + 
                            C14 + C15 + C16 + C17 + C18 + C19 + C20 + C21, 
                          data = MostOften, family=binomial)
summary(ClickModelMostOften) # Delete the site_id the pr is 0.29429
ClickModelMostOften = glm(click ~ C1 + banner_pos + site_domain
                          + site_category + app_id + app_domain + 
                            app_category + device_ip 
                          + device_type + device_conn_type + 
                            C14 + C15 + C16 + C17 + C18 + C19 + C20 + C21, 
                          data = MostOften, family=binomial)
summary(ClickModelMostOften) # Delete the device_id the pr is 0.28247
ClickModelMostOften = glm(click ~ C1 + banner_pos + site_domain
                          + site_category + app_id + app_domain  
                             + device_ip + device_type + device_conn_type + 
                            C14 + C15 + C16 + C17 + C18 + C19 + C20 + C21, 
                          data = MostOften, family=binomial)
summary(ClickModelMostOften) # Delete the app_category the pr is 0.12335
# Finish the refine of the logistic regression

PredictMostOften = predict(ClickModelMostOften, type="response")
# Evaluate the model

# ROC for training set
ROCRpred = prediction(PredictMostOften, MostOften$click)
ROCCurve = performance(ROCRpred, "tpr", "fpr")
plot(ROCCurve)
plot(ROCCurve, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))  
# We add colors and labels corresponding to the threshold values

# AUC for training set
as.numeric(performance(ROCRpred, "auc")@y.values) # The AUC result equal to 0.6399004
# After selecting the threshold "t", 0.2 is better and recommended
# Because the ROC shows mostly a vertical drop after t=0.2, and we decide to pick up the TPR above 0.4
#and the FPR below 0.4, which means we guess correctly more often than wrong, both when actually 0 was
#realized and when actually 1 was realized. 
#Although if t = 0.1 the TPR is higher than 0.2, the worse FPR it is. So we waive t = 0.1
table(MostOften$click, PredictMostOften > 0.2)
#      FALSE  TRUE
# 0    55774  20169
# 1    8610   6766

OutofSampleAcc = (55774 + 6766)/(55774+20169+8610+6766)
BaselineAcc = (55774+20169)/(55774+20169+8610+6766)
CTRLogR = (20169 + 6766) / (55774+20169+8610+6766)
OutofSampleAcc # The accuracy is 0.684852
BaselineAcc   # The baseline is 0.8316232
CTRLogR   # The click-through rate is 0.294955



#####Comparing the models in validation set
# Logistic Regression
PredictValidation = predict(ClickModel, type="response", newdata=ClickValidation)
ROCRpredb2 = prediction(PredictValidation, ClickValidation$click)
ROCCurveb2 = performance(ROCRpredb2, "tpr", "fpr")
plot(ROCCurveb2)
plot(ROCCurveb2, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(ROCRpredb2, "auc")@y.values)  # The AUC result equal to 0.6320749
k = table(ClickValidation$click, PredictValidation > 0.2)
#FALSE  TRUE
#0 15660  5810
#1  2471  1885
OutofSampleAcc = (k[1,1] + k[2,2])/sum(k)
BaselineAcc = (k[1,1] + k[1,2])/sum(k)
OutofSampleAcc # The accuracy is 0.6793541
BaselineAcc   # The baseline is 0.8313328
CTRLogRinVal = (k[1,2] + k[2,2])/sum(k)
CTRLogRinVal  # The click-through rate is 0.2979555



# Classfication Tree
PredictClickTreeVal = predict(ClickTree, newdata = ClickValidation)
pred50 = prediction(PredictClickTreeVal[,2], ClickValidation$click)
perf50 = performance(pred50, "tpr", "fpr")
plot(perf50)
as.numeric(performance(pred50, "auc")@y.values)
# AUC is 0.5426854

MyClickTreeVal = table(ClickValidation$click,PredictClickTreeVal)
MyClickTreeVal
AccuTrainVal = (MyClickTreeVal[1,1]+MyClickTreeVal[2,2])/sum(MyClickTreeVal)
AccuTrainVal # The Accuracy is 0.8329978
CTRClassTVal = (MyClickTreeVal[1,2]+MyClickTreeVal[2,2])/sum(MyClickTreeVal)
CTRClassTVal # The click-through rate is 0.008170061

# Random Forest
PredictClickRFVal = predict(ClickRF , newdata = ClickValidation)
tableRF = table(ClickValidation$click, PredictClickRFVal)
AccValidationRF = (tableRF[1,1] + tableRF[2,2]) / sum(tableRF)
AccValidationRF  # The accuracy is 0.8348563
CTRRFVal = (tableRF[1,2] + tableRF[2,2]) / sum(tableRF)
CTRRFVal  # The click-through is 0.01328119

#Improving Model
PredictValidationIM = predict(ClickModelMostOften, type="response", newdata=ClickValidation)
ROCRpredb3 = prediction(PredictValidationIM, ClickValidation$click)
ROCCurveb3 = performance(ROCRpredb3, "tpr", "fpr")
plot(ROCCurveb3)
plot(ROCCurveb3, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(ROCRpredb2, "auc")@y.values)  # The AUC result equal to 0.6320749
table1 = table(ClickValidation$click, PredictValidationIM > 0.2)
table1
#   FALSE   TRUE
#0  15689   5781
#1  2468    1888
OutofSampleAccIM = (table1[1,1] + table1[2,2])/sum(table1)
BaselineAccIM  = (table1[1,1] + table1[1,2])/sum(table1)
OutofSampleAccIM # The accuracy is 0.6805932
BaselineAccIM   # The baseline is 0.8313328
CTRLogRinValIM = (table1[1,2] + table1[2,2])/sum(table1)
CTRLogRinValIM  # The click-through rate is 0.2969488




# Therefore, comparing the three accuracy of the models, pick up the highest accuracy,
#the Random Forest is the most recommended one, the accuracy is 0.8348563.


#####Testing on testing set

PredictClickRFTest = predict(ClickRF , newdata = ClickTest)
tableRFTest = table(ClickTest$click, PredictClickRFTest)
#PredictClickRFTest
#     0     1
#0 21327   142
#1  4145   212
AccTestRF = (tableRFTest[1,1] + tableRFTest[2,2]) / sum(tableRFTest)
AccTestRF  # The accuracy is 0.8340819
CTRRFTest = (tableRFTest[1,2] + tableRFTest[2,2]) / sum(tableRFTest)
CTRRFTest  # The click-through is 0.01370712

# That is great!







#Therefore, I decide to pick up rows randomly to compress the dataset.
#str(ClickTrain)       # 103304 objects.
#CTsmall = ClickTrain[sample(nrow(ClickTrain), 10000), ]
#ClickTrain1 = ClickTrain[c(-50000), ] 
#str(CTsmall)
#ClickModel = glm(click ~ C1 + banner_pos + site_id + site_domain + site_category + app_id + app_domain + app_category + device_id + device_ip + device_model + device_type + device_conn_type + C14 + C15 + C16 + C17 + C18 + C19 + C20 + C21, data = CTsmall, family=binomial)
#summary(ClickModel)
# Coefficients: (221 not defined because of singularities)
#CS =CTsmall(as.is = TRUE)
#mean(as.numeric(levels(factorname)[factorname]))
#test = glm (click ~ site_id, data = CTsmall, family = binomial)
#summary(test)






