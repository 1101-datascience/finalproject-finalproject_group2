### Read Data
path <- "card_data/BankChurners.csv"
d <- read.table(path, header=T, sep=",", encoding = "utf-8")
d <- subset(d, select = -c(1, 22, 23))
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
income_mode <- getmode(d$Income_Category)
d[which(d$Income_Category=="Unknown"), "Income_Category"] <- income_mode
# print(dim(d[which(d$Income_Category=="Unknown"), ]))
# summary(d)
# str(d)


# print(dim(d[which(d$Attrition_Flag=="Attrited Customer"), ]))
# print(dim(d[which(d$Attrition_Flag!="Attrited Customer"), ]))

# install.packages("xgboost", repos="https://cran.rstudio.com/")
library("xgboost")
library("caret")

# set.seed(120)
# install.packages("caTools", repos="https://cran.rstudio.com/")
library("caTools") 
index.train <- createDataPartition(d[ ,1], p=0.7, list=F, times=1) # caret

train <- d[index.train,]
test <- d[-index.train,]

# install.packages(c("zoo","xts","quantmod"))
# install.packages( "~/Downloads/DMwR_0.4.1.tar.gz", repos=NULL, type="source" )
library("DMwR")
train.smote <- SMOTE(Attrition_Flag~., train)

# print(dim(train.smote[which(train.smote$Attrition_Flag=="Attrited Customer"), ]))
# print(dim(train.smote[which(train.smote$Attrition_Flag!="Attrited Customer"), ]))

# print(dim(test[which(test$Attrition_Flag=="Attrited Customer"), ]))
# print(dim(test[which(test$Attrition_Flag!="Attrited Customer"), ]))

## One-hot encoding
dummy <- dummyVars(" ~.", data=d[,-1])
train.x <- data.frame(predict(dummy, newdata = train.smote[,-1]))
test.x <- data.frame(predict(dummy, newdata = test[,-1]))

train.y <- ifelse(train.smote$Attrition_Flag=="Attrited Customer", 1, 0)
test.y <- ifelse(test$Attrition_Flag=="Attrited Customer", 1, 0)

## Train Model
model <- xgboost(data = as.matrix(train.x), label=as.matrix(train.y), 
                 max.depth = 6, eta = 1, nthread = 2, eval_metric = "logloss",
                 nrounds = 9, objective = "binary:logistic")
predict_prob <- predict(model, as.matrix(test.x))
prediction <- as.numeric(predict_prob > 0.5)

## Performance function
calc_performance <- function(truth, prediction){
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
  sensitivity <- round(cm_list$tp / (cm_list$tp + cm_list$fn), digits = 4) ## also recall
  specificity <- round(cm_list$tn / (cm_list$tn + cm_list$fp), digits = 4)
  precision <- round(cm_list$tp / (cm_list$tp + cm_list$fp), digits = 4) ## also PPV
  
  performance <- list("accuracy" = accuracy, "sensitivity" = sensitivity,
                      "specificity" = specificity, "precision" = precision)
  
  return(performance)
}

performance.test <- calc_performance(test.y, prediction)
print(performance.test)
