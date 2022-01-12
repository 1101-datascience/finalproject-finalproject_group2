# [Group 2] Predict Churning Customers

### Groups
* 朱宇方, 107703035
* 林子恩, 107703046 
* 郭家瑜, 107308016
* 穆永綸, 109971003

### Goal
Predict if a customer is going to stop using the credit card services.

### Demo 
You should provide an example commend to reproduce your result
```R
Rscript  code/merged.R --input data/BankChurners.csv --output results/performance.csv

```
* any on-line visualization
  * Url：https://wutever0017.shinyapps.io/bankchurner_v1/
## Folder organization and its related information

### docs
* Your presentation, docs/1101DS_Group2.pptx

### data

* Source : Credit Card customers from Kaggle
* Url : https://www.kaggle.com/sakshigoyal7/credit-card-customers
* Input format: CSV file
* Any preprocessing?
  * Handle missing data: Income_Category
  * SMOTE

### code

* Which method do you use?
  * Models for Classification Task
  * SVM
  * NaiveBayes
  * XGBoost
  * Random Forest
  
* What is a null model for comparison?
  * Accuracy of Null Model : 0.8393

### results

* Which metric do you use 
  * precision, recall, R-square
* Is your improvement significant?

| type | training | testing |
| --------- | -------- | -------- |
| NaiveBayes | 0.81 | 0.81 
| SVM | 0.94 | 0.92 
| XGBoost | 0.99 | 0.95
| Random Forest | 0.95 | 0.95


## References
* Code/implementation which you include/reference 
[SMOTE] (https://www.rdocumentation.org/packages/DMwR/versions/0.4.1/topics/SMOTE)

[Mode] (https://www.tutorialspoint.com/r/r_mean_median_mode.htm)
[Naive Bayes] (https://www.edureka.co/blog/naive-bayes-in-r/)
[SVM] (https://www.rdocumentation.org/packages/e1071/versions/1.7-9/topics/svm)
[One-hot Encoding] (https://www.rdocumentation.org/packages/caret/versions/6.0-90/topics/dummyVars)
[Xgboost] (https://www.rdocumentation.org/packages/xgboost/versions/1.5.0.2/topics/xgb.train)
[ggplot2] (https://ggplot2.tidyverse.org/index.html)
[Random Forest] https://rpubs.com/phamdinhkhanh/389752)

* Packages you use
library(e1071)
library(caTools)
library(caret)
library(DMwR)
library(randomForest)
library(tidyverse)
library(xgboost)
library(party)

library(shiny)
library(ggbiplot)
library(MASS)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(cowplot)

* Related publications
* Chawla, N. V., Bowyer, K. W., Hall, L. O., & Kegelmeyer, W. P. (2002). SMOTE: synthetic minority over-sampling technique. Journal of artificial intelligence research, 16, 321-357.
