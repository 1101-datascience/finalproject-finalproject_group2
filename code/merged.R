#start input parsing
# read parameters

library(e1071)
library(caTools)
library(caret)
library(DMwR)
library(randomForest)
library(tidyverse)
library(xgboost)
set.seed(120)

svm_ml <- function (data_train,data_test){
   ##### model #####
   train_data <- data_train
   test_data <- data_test
cat("\n----model----\n")
model <- svm(formula=Attrition_Flag ~ ., 
             data = train_data)
cat("----summary----\n")
summary(model)

train.pred <- predict(model, train_data)
test.pred <- predict(model, test_data)

#table(real=train_data$Attrition_Flag, predict=train.pred)
confus1 <- table(real=train_data$Attrition_Flag, predict=train.pred)
sum(diag(confus1))/sum(confus1)
#confus.matirx

#table(real=test_data$Attrition_Flag, predict=test.pred)
confus2 <- table(real=test_data$Attrition_Flag, predict=test.pred)
sum(diag(confus2))/sum(confus2)
#confus

result <- confusionMatrix(confus2)


   result$overall['Accuracy']
}
xgboost_ml <- function (data_train,data_test){
   dummy <- dummyVars(" ~.", data=input_f[,-1])
   train.x <- data.frame(predict(dummy, newdata = data_train[,-1]))
   test.x <- data.frame(predict(dummy, newdata = data_test[,-1]))


   train.y <- ifelse(data_train$Attrition_Flag=="Attrited Customer", 1, 0)
   test.y <- ifelse(data_test$Attrition_Flag=="Attrited Customer", 1, 0)

   ## Train Model
   model <- xgboost(data = as.matrix(train.x), label=as.matrix(train.y), 
                  max.depth = 6, eta = 1, nthread = 2, eval_metric = "logloss",
                  nrounds = 9, objective = "binary:logistic")
   predict_prob <- predict(model, as.matrix(test.x))
   prediction <- as.numeric(predict_prob > 0.5)
   truth <- test.y
   result <- data.frame(truth=ifelse(truth==1, "Attrited", "Existing"),
                       prediction=ifelse(prediction==1, "Attrited", "Existing"))
   confusion_matrix <- table(truth=result$truth,
                              prediction=result$prediction)
   print(confusion_matrix)
   
   sample_num <- nrow(result)
   pos_num <- nrow(result[result$prediction==result$truth, ])
   pos <- "Attrited"
   neg <- "Existing"
   cm_list <- list("tp"=confusion_matrix[pos, pos], "fn"=confusion_matrix[pos, neg],
                     "fp"=confusion_matrix[neg, pos], "tn"=confusion_matrix[neg, neg])
   
   accuracy <- round(pos_num/nrow(result), digits = 4)
   return (accuracy)
}
NaiveBayes_ml <- function(data_train,data_test){
   classifier_cl <- naiveBayes(Attrition_Flag ~ ., data = data_train)
   #classifier_cl
   
   # Predicting on test data'
   y_pred <- predict(classifier_cl, newdata = data_test)
   
   # Confusion Matrix
   cm <- table(data_test$Attrition_Flag, y_pred)
   
   # Model Evaluation
   result <- confusionMatrix(cm)

   result$overall['Accuracy']
}


RF_ml <- function(data_train,data_test){
   my_control1<-trainControl(method="cv",number=10,summaryFunction = twoClassSummary,classProbs = TRUE,verboseIter = TRUE,savePredictions = TRUE, search = "grid")
   tunegrid <- expand.grid(.mtry = (1:11)) 
   rf_model<-caret::train(Attrition_Flag~.,data=train_cl,method="rf",traincontrol=my_control1,ntree = 80, tuneGrid = tunegrid)
   #plot(rf_model)

   print(rf_model)

   # pred_accuracy

   pred_test <- predict(rf_model, newdata=test_cl)

   pred_testing <- ifelse(pred_test == test_cl$Attrition_Flag , 1,0)
   testing_accuracy <- round(length(pred_testing[pred_testing==1])/length(pred_testing),5)

   return(testing_accuracy)

}
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript merged.R  --input file... --output out.csv", call.=FALSE)
}

# parse parameters
i<-1 
while(i < length(args))
{
    if(args[i] == "--input"){
    input_f<-args[i+1]
    i<-i+1
  }else if(args[i] == "--output"){
    output_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

print("PROCESS COMPLETE")
#precrocessing start 
#----------
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

input_f<- read.csv(file = input_f)
input_f$Attrition_Flag <- as.factor(input_f$Attrition_Flag)
input_f$Attrition_Flag <- as.factor(input_f$Attrition_Flag)
input_f$Gender <- as.factor(input_f$Gender)
input_f$Education_Level <- as.factor(input_f$Education_Level)
input_f$Marital_Status <- as.factor(input_f$Marital_Status)

income_mode <- getmode(input_f$Income_Category)
print(income_mode)

input_f$Income_Category[input_f$Income_Category == "Unknown"] <- income_mode
input_f$Income_Category<- as.factor(input_f$Income_Category)



input_f$Card_Category<- as.factor(input_f$Card_Category)



input_f$CLIENTNUM <- NULL
input_f$Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1 <- NULL
input_f$Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2 <- NULL

#table(input_f$Attrition_Flag)

#Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2
split <- sample.split(input_f, SplitRatio = 0.7)
train_cl <- subset(input_f, split == "TRUE")
test_cl <- subset(input_f, split == "FALSE")
 

table(train_cl$Attrition_Flag)
train_cl <- SMOTE(Attrition_Flag~., train_cl)
table(train_cl$Attrition_Flag)


#print(xgboost_ml(train_cl,train_cl))
#print(svm_ml(train_cl,train_cl))
#print(RF_ml(train_cl,train_cl))


output_df <- data.frame(matrix(ncol = 6, nrow = 0))
x <- c("type","training","test")
colnames(output_df) <- x
names(output_df) <- x

train_result <- NaiveBayes_ml(train_cl,train_cl)
test_result <- NaiveBayes_ml(train_cl,test_cl)
current_df <- data.frame("NaiveBayes",round(train_result,2),round(test_result,2))
names(current_df)<- c("type","training","test")
output_df <- rbind(output_df,current_df)

train_result <- svm_ml(train_cl,train_cl)
test_result <- svm_ml(train_cl,test_cl)
current_df <- data.frame("SVM",round(train_result,2),round(test_result,2))
names(current_df)<- c("type","training","test")
output_df <- rbind(output_df,current_df)

train_result <- xgboost_ml(train_cl,train_cl)
test_result <- xgboost_ml(train_cl,test_cl)
current_df <- data.frame("Xgboost",round(train_result,2),round(test_result,2))
names(current_df)<- c("type","training","test")
output_df <- rbind(output_df,current_df)


train_result <- RF_ml(train_cl,train_cl)
test_result <- RF_ml(train_cl,test_cl)
current_df <- data.frame("RandomForest",round(train_result,2),round(test_result,2))
names(current_df)<- c("type","training","test")
output_df <- rbind(output_df,current_df)

rownames(output_df) <- NULL
print(output_df)

write.csv(output_df,file=output_f, row.names = FALSE, quote=F)


