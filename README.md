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
* What is the challenge part of your project?

## References
* Code/implementation which you include/reference (__You should indicate in your presentation if you use code for others. Otherwise, cheating will result in 0 score for final project.__)
* Packages you use
* Related publications
