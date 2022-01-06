
# Installing Packages
#install.packages("e1071")
#install.packages("caTools")
#install.packages("caret")
 
# Loading package
library(e1071)
library(caTools)
library(caret)
library(DMwR)
# Splitting data into train
# and test data

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

input_f<- read.csv(file = 'BankChurners.csv')
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
 
#print(head(train_cl))
# Feature Scaling
train_scale <- train_cl[, 2:12]
test_scale <- test_cl[, 2:12]
 
# Fitting Naive Bayes Model
# to training dataset
set.seed(120)  # Setting Seed

table(train_cl$Attrition_Flag)
train_cl <- SMOTE(Attrition_Flag~., train_cl)
table(train_cl$Attrition_Flag)


classifier_cl <- naiveBayes(Attrition_Flag ~ ., data = train_cl)
#classifier_cl
 
# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_cl)
 
# Confusion Matrix
cm <- table(test_cl$Attrition_Flag, y_pred)
cm
 
# Model Evaluation
confusionMatrix(cm)