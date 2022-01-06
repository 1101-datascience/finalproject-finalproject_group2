library(e1071)
library(caTools)
library(caret)
library(DMwR)
#library(smotefamily)


##### read train data #####
input_data <- read.csv("BankChurners.csv", header = T)
cat("input row number:", nrow(input_data), "\n")

##### fill in income #####
zerok <- 0
fourtyk <- 0
sixtyk <- 0
eightyk <- 0
onetwentyk <- 0
max <- ''
for (i in 1:nrow(input_data)){
  if(input_data[i,8] == 'Less than $40K'){
    zerok <- zerok + 1
    if(zerok > max){
      max <- 'Less than $40K'
    }
  }
  else if (input_data[i, 8] == '$40K - $60K'){
    fourtyk <- fourtyk + 1
    if(fourtyk > max){
      max <- '$40K - $60K'
    }
  }
  else if (input_data[i, 8] == '$60K - $80K'){
    sixtyk <- sixtyk + 1
    if(sixtyk > max){
      max <- '$60K - $80K'
    }
  }
  else if (input_data[i, 8] == '$80K - $120K'){
    eightyk <- eightyk + 1
    if(eightyk > max){
      max <- '$80K - $120K'
    }
  }
  else if (input_data[i, 8] == '$120K +'){
    onetwentyk <- onetwentyk + 1
    if(onetwentyk > max){
      max <- '$120K +'
    }
  }
}
cat("max", max, "\n")

for(i in 1:nrow(input_data)){
  if(input_data[i, 8] == 'Unknown'){
    input_data[i, 8] <- max
  }
}

set.seed(120)
split <- sample.split(input_data, SplitRatio = 0.7)
train_data <- subset(input_data, split == "TRUE")
test_data <- subset(input_data, split == "FALSE")

##### smote #####
cat("\n---table---\n")
print(table(train_data$Attrition_Flag))

train_data <- SMOTE(Attrition_Flag ~., train_data)
table(train_data$Attrition_Flag)
cat("---table---\n\n")



##### model #####
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

confusionMatrix(confus2)
