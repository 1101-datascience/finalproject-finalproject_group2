library(shiny)
library(ggbiplot)
library(MASS)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(cowplot)

library(e1071)
library(caTools)
library(caret)
library(DMwR)
library(randomForest)
library(xgboost)
library(party)
set.seed(120)

features <- c("Attrition_Flag", "Gender", "Age", "Education_Level", 
              "Income_Category", "Marital_Status", "Card_Category", 
              "Total_Trans_Amt", "Avg_Utilization_Ratio", "Credit_Limit")
pc <- c("Customer_Age", "Dependent_count", 
        "Months_on_book", "Total_Relationship_Count", 
        "Months_Inactive_12_mon", "Contacts_Count_12_mon", "Credit_Limit",
        "Total_Revolving_Bal", "Avg_Open_To_Buy", "Total_Amt_Chng_Q4_Q1", 
        "Total_Trans_Amt", "Total_Trans_Ct", "Total_Ct_Chng_Q4_Q1", "Avg_Utilization_Ratio")

input_data <- read.csv("BankChurners.csv", header = T)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

input_data$CLIENTNUM <- NULL
input_data$Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1 <- NULL
input_data$Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2 <- NULL

input_data$Attrition_Flag <- as.factor(input_data$Attrition_Flag)
input_data$Gender <- as.factor(input_data$Gender)
input_data$Education_Level <- as.factor(input_data$Education_Level)
input_data$Marital_Status <- as.factor(input_data$Marital_Status)
income_mode <- getmode(input_data$Income_Category)
input_data$Income_Category[input_data$Income_Category == "Unknown"] <- income_mode
input_data$Income_Category<- as.factor(input_data$Income_Category)
input_data$Card_Category<- as.factor(input_data$Card_Category)

split <- sample.split(input_data, SplitRatio = 0.7)
train_cl1 <- subset(input_data, split == "TRUE")
train_cl2 <- SMOTE(Attrition_Flag ~ ., train_cl1)
test_cl <- subset(input_data, split == "FALSE")




# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("BankChurners"),
  
  # tabs
  tabsetPanel(type = "tabs",
              tabPanel("raw data", sidebarLayout(
                                    sidebarPanel(
                                      selectInput("feature", "select feature to view graph", features, selected = features[1])
                                    ),
                                    mainPanel(plotOutput("featurePlot"))
                                    )
                      ),
              tabPanel("training data", plotOutput("train")),
              tabPanel("Naive Bayes"),
              tabPanel("SVM", sidebarLayout(
                                sidebarPanel(
                                  selectInput("x_axis", "select x-axis for PCA", pc, selected = pc[1]),
                                  selectInput("y_axis", "select y-axis for PCA", pc, selected = pc[1])
                                ),
                                mainPanel(plotOutput("svm_plot")),
                              )
                       ),
              tabPanel("Random Forest", plotOutput("rf_tree")),
              tabPanel("Xgboost")
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$feature, {
    print(paste0("You have chosen: ", input$feature))
  })
  
  output$featurePlot <- renderPlot({
    
    if(input$feature == 'Attrition_Flag'){
      Attrition_Flag_bar <- ggplot(data = input_data, aes(x = Attrition_Flag,  fill = Attrition_Flag)) +
                            geom_bar(position = position_dodge(), alpha = 0.75) + 
                            theme(text = element_text(size=20))
      print(Attrition_Flag_bar)
    }
    
    if(input$feature == 'Gender'){

      gender_bar <- ggplot(data = input_data, aes(x = Attrition_Flag,  fill = Gender)) +
                          geom_bar(position = position_dodge(), alpha = 0.75) + 
                          theme(text = element_text(size=20))

      df <- data.frame(
        Gender = c("Male", "Female"),
        value = c(4769, 5358)
      )

      gender_pie <- ggplot(df, aes(x = "", y = value, fill = Gender)) +
                          geom_col(color = "white", alpha = 0.75) +
                          geom_text(aes(label = value), size=8,
                                    position = position_stack(vjust = 0.5)) +
                          coord_polar(theta = "y")+ 
                          theme_void()

      print(plot_grid(gender_bar, gender_pie, align='v', nrow=2, rel_heights = c(3/6, 2/6)))
    }
    
    if(input$feature == 'Age'){

      age_box <- qplot(Customer_Age,data = input_data ,geom="boxplot",fill=Customer_Age) + 
                      theme(text = element_text(size=15))
      
      age_bar2 <- ggplot(data = input_data, aes(x = Customer_Age,  fill = Attrition_Flag)) +
                          geom_bar(position = position_dodge(), alpha = 0.75) + 
                          theme(text = element_text(size=15))

      #grid.arrange(age_bar, age_box, ncol=1, nrow=2)
      print(plot_grid(age_box, age_bar2, align='v', nrow=2, rel_heights = c(1, 2)))
    }
    
    if(input$feature == 'Education_Level'){
      edu_bar <- ggplot(data = input_data, aes(x = Education_Level,  fill = Attrition_Flag)) +
                        geom_bar(position = position_dodge(), alpha = 0.75) + 
                        theme(text = element_text(size=15))

      print(edu_bar)
    }
    
    if(input$feature == 'Income_Category'){
      income_bar <- ggplot(data = input_data, aes(x = Income_Category,  fill = Attrition_Flag)) +
                        geom_bar(position = position_dodge(), alpha = 0.75) + 
                        theme(text = element_text(size=15))
      print(income_bar)
    }
    
    if(input$feature == 'Marital_Status'){
      Marital_Status_bar <- ggplot(data = input_data, aes(x = Marital_Status,  fill = Attrition_Flag)) +
                                  geom_bar(position = position_dodge(), alpha = 0.75) + 
                                  theme(text = element_text(size=15))
      print(Marital_Status_bar)
    }
    
    if(input$feature == 'Card_Category'){
      Card_Category_bar <- ggplot(data = input_data, aes(x = Card_Category, fill = Attrition_Flag)) +
                                geom_bar(position = position_dodge(), alpha = 0.75) + 
                                theme(text = element_text(size=15))
      print(Card_Category_bar)
    }
    
    if(input$feature == 'Total_Trans_Amt'){
      p <- qplot(Total_Trans_Amt,data = input_data, geom="density",
                 fill=Attrition_Flag,alpha = I(.5)) + theme(text = element_text(size=20))
      print(p)
    }
    
    if(input$feature == 'Avg_Utilization_Ratio'){
      p <- qplot(Avg_Utilization_Ratio, data = input_data, geom="density",
                 fill=Attrition_Flag,alpha = I(.5)) + theme(text = element_text(size=20))
      print(p)
    }
    
    if(input$feature == 'Credit_Limit'){
      p <- qplot(Credit_Limit,data = input_data, geom="density", colour=Attrition_Flag) + 
                  theme(text = element_text(size=20))
      print(p)
    }
  })
  
  output$train <- renderPlot({
    print("---train data---")
    data <- data.frame(table(train_cl1$Attrition_Flag))
    data2 <- data.frame(
      group = c("Attrited Customer", "Existing Customer"),
      value = data$Freq
    )
    print(data2)
    
    smote_data <- data.frame(table(train_cl2$Attrition_Flag))
    smote_data2 <- data.frame(
      group = c("Attrited Customer", "Existing Customer"),
      value = smote_data$Freq
    )
    print(smote_data2)
    
    b4smote <- ggplot(data2, aes(x = "", y = value, fill = group)) +
                        geom_col(color = "white", alpha = 0.75) +
                        geom_text(aes(label = value), size=8,
                                  position = position_stack(vjust = 0.5)) +
                        coord_polar(theta = "y") + ggtitle('training data before smote') + 
                        theme_void()
    aftersmote <- ggplot(smote_data2, aes(x = "", y = value, fill = group)) +
                        geom_col(color = "white", alpha = 0.75) +
                        geom_text(aes(label = value), size=8,
                                  position = position_stack(vjust = 0.5)) +
                        coord_polar(theta = "y") + ggtitle('training data after smote') +
                        theme_void()
    print(plot_grid(b4smote, aftersmote, align='v', nrow=2, rel_heights = c(1, 1)))
  })
  
  ###############
  ### svm/pca ###
  print("--- train cl2---")
  print(head(train_cl2))
  print("---numeric data---")
  # find numeric columns
  num_cols <- unlist(lapply(train_cl2, is.numeric))

  numeric_data <- train_cl2[, num_cols]
  print(head(numeric_data))
  #log transform
  log.bc <- log(numeric_data[,])
  log.bc <- replace(log.bc, log.bc == -Inf, 0)
  print("---log---")
  print(head(log.bc))
  bc.attr <- train_cl2[, 1]
  print("--- bc.attr ---")
  print(bc.attr)
  # apply PCA - scale. = TRUE is highly advisable, but default is FALSE.
  bc.pca <- prcomp(log.bc, center = TRUE, scale. = TRUE)
  
  output$svm_plot <- renderPlot({
    
    x <- match(input$x_axis, pc)
    y <- match(input$y_axis, pc)
    axis <- c(x, y)
    print(axis)
    
    g <- ggbiplot(bc.pca, choices = axis, obs.scale = 1, var.scale = 1, groups = bc.attr)
    g <- g + scale_color_discrete(name = '')
    g <- g + ggtitle("PCA of bankchurners")
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top',
                   plot.title = element_text(face="bold", size=20, hjust = 0.5))
    plot(g)
  })
  
  ### random forest tree
  rf <- ctree(Attrition_Flag ~ ., data=train_cl2, control=ctree_control(maxdepth = 3) )
  output$rf_tree <- renderPlot({
    print("random forest")
    plot(rf, type="simple")
    #cforest(Attrition_Flag ~ ., data=input_data, controls=cforest_control(mtry=2, mincriterion=0))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
