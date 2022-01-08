# Loading data & Data cleaning

library(tidyverse)
library(caret)
library(randomForest)
library(DMwR)
library("xgboost")
library("caTools") 
churner <- read.csv("BankChurners.csv", stringsAsFactors = FALSE)

churners<-churner%>%
  select(-c(1,22,23))

# Deleting rows containing "Unknown"
#churners <- churners[- grep("Unknown", churners$Education_Level),]
#churners <- churners[- grep("Unknown", churners$Marital_Status),]
#churners <- churners[- grep("Unknown", churners$Income_Category),]

churners<-churners%>%
  mutate(Attrition_Flag=ifelse(Attrition_Flag=="Existing Customer",0,1),Attrition_Flag=as.factor(Attrition_Flag))%>%
  mutate_if(is.character,as.factor)

#Replacing INCOME unknown data with mode income

#summary(churners$Income_Category) #mode:$40K - $60
churners$Income_Category[ grep("Unknown", churners$Income_Category)] <- '$40K - $60K'

# Creating dummy variables
#vars_dummy <- churners[, c("Gender", "Marital_Status")]
vars_dummy <- churners[, c("Gender", "Marital_Status","Education_Level", "Income_Category", "Card_Category")]
dummy      <- dummyVars("~.", data = vars_dummy)
dummy_     <- predict(dummy, vars_dummy) 

# Concatenating the new dummy variables in the dataset

#head(dummy_)

#churners <- dplyr::select(cbind(churners, dummy_), -c("Gender", "Gender.F", "Marital_Status"))
churners <- dplyr::select(cbind(churners, dummy_), -c("Gender", "Gender.F", "Marital_Status","Education_Level", "Income_Category", "Card_Category"))

colnames(churners)[which(names(churners) == "Education_Level.High School")] <- "Education_Level.High_School"
colnames(churners)[which(names(churners) == "Education_Level.Post-Graduate")] <- "Education_Level.Post_Graduate"
colnames(churners)[which(names(churners) == "Income_Category.$120K +")] <- "Income_Category.120K"
colnames(churners)[which(names(churners) == "Income_Category.Less than $40K")] <- "Income_Category.Less_than_40K"
colnames(churners)[which(names(churners) == "Income_Category.$40K - $60K")] <- "Income_Category.40K_60K"
colnames(churners)[which(names(churners) == "Income_Category.$60K - $80K")] <- "Income_Category.60K_80K"
colnames(churners)[which(names(churners) == "Income_Category.$80K - $120K")] <- "Income_Category.80K_120K"

# Removing Low Variance variables
#zero_nearzero_variance<-nearZeroVar(churners)
#zero_nearzero_variance
#churners<-churners%>%
#  select(-20)

# Multi-correlated variables
#correlated<-findCorrelation(cor(churners[,-c(1,3,5,6,7,8)]),names=T) # not to include fct.
#correlated
# Removing the variables that is also highly correlated with one of the other variables
#churners<-churners%>%
#  select(-correlated)
#head(churners)

#Attrition_Flag <- as.factor(churners[,1])
#predictor_variable <- churners[,c(2,9,13:18)]
#predictor_variable2 <- churners[,-c(2,9,13:18)]

#preprovalues <- preProcess(predictor_variable,method = c("center"))
#predictor_variable <-  predict(preprovalues,predictor_variable)

#churners <-  cbind(Attrition_Flag,predictor_variable,predictor_variable2)

idx2 <- createDataPartition(churners$Attrition_Flag, p = 0.75, list = FALSE)
train <- churners[idx2,]
test <- churners[-idx2,]

#head(train)

# Finding important variable

#rf<-randomForest(Attrition_Flag~.,data=train)
#print(rf$importance)
#varImpPlot(rf)

# train[,11:32] <- sapply(train[, 11:32], as.integer)

# Training model

# Grid # SMOTE
set.seed(1000)

smote_train <- SMOTE(Attrition_Flag ~ ., data  = train)
table(train$Attrition_Flag)
table(smote_train$Attrition_Flag)

my_control1<-trainControl(method="cv",number=10,summaryFunction = twoClassSummary,classProbs = TRUE,verboseIter = TRUE,savePredictions = TRUE, search = "grid")
tunegrid <- expand.grid(.mtry = (1:11)) 
rf_model<-caret::train(Attrition_Flag~.,data=smote_train,method="rf",traincontrol=my_control1,ntree = 80, tuneGrid = tunegrid)
#plot(rf_model)

print(rf_model)

# pred_accuracy

pred_test <- predict(rf_model, newdata=test)

pred_testing <- ifelse(pred_test == test$Attrition_Flag , 1,0)
testing_accuracy <- round(length(pred_testing[pred_testing==1])/length(pred_testing),5)

print(testing_accuracy)

# Confusion Matrix

cft <- table(pred_test, test$Attrition_Flag)
print(cft)

(tp <- cft[2, 2])
(tn <- cft[1, 1])
(fp <- cft[2, 1])
(fn <- cft[1, 2])

print(accuracy <- (tp + tn)/(tp + tn + fp + fn))
print(sensitivity <- tp/(tp + fn)) # = recall
print(specificity <- tn/(tn + fp))
print(Precision <- tp/(tp + fp))



