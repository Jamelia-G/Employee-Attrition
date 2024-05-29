##################################################################################################################
### Data Preparation and Understanding
install.packages("tidyverse")
install.packages("randomForest")
install.packages("glmnet")
install.packages ("pROC")
install.packages("rpart.plot")
library(rpart.plot)
library(tidyverse)

# read data into R
hr_data <- read.csv("C:/Users/jamel/Downloads/WA_Fn-UseC_-HR-Employee-Attrition.csv")

# Next use the function summary to inspect the data
summary(hr_data)

##############################################
# Cleaning up the data
#Dropped Over18, EmployeeCount and StandardHours columns
#they have the same value for all records in the data set

hr_data <- hr_data %>%
  select(-Over18) %>%
  select(-EmployeeCount) %>%
  select (-StandardHours)

# Convert 'Gender' to a dummy variables (1 for 'Male' and 0 for 'Female')
hr_data <- hr_data %>%
  mutate(Gender = ifelse(Gender == "Male", 1, 0))

# Convert 'OverTime' to a dummy variables (1 for 'Yes' and 0 for 'No')
hr_data <- hr_data %>%
  mutate(OverTime = ifelse(OverTime == "Yes", 1, 0))

#"MaritalStatus" into 1 for "Single, 2 for "Married" and 3 for "Divorced" 
hr_data <- hr_data %>%
  mutate(MaritalStatus = ifelse(MaritalStatus == "Single", 1, ifelse(MaritalStatus == "Married", 2,3)))

#'BusinessTravel' into 1 for "Travel_Rarely", 2 for "Travel_Frequently" and 3 for "Non-Travel" 
unique(hr_data$BusinessTravel)
hr_data <- hr_data %>%
  mutate(BusinessTravel = ifelse(BusinessTravel == "Travel_Rarely", 1, ifelse(BusinessTravel == "Travel_Frequently", 2,3)))         

# Convert Target Variable 'Attrition' to a dummy variables (1 for 'Yes' and 0 for 'No')
hr_data <- hr_data %>%
  mutate(Attrition = ifelse(Attrition == "Yes", 1, 0))

# Keys for reference
department_key <- unique(hr_data$Department)
educationField_key <- unique(hr_data$EducationField)
job_role_key <- unique(hr_data$JobRole)

# Convert Department to a categorical variable
hr_data$Department <- as.numeric(factor(hr_data$Department))

# Convert Education to a categorical variable
hr_data$EducationField <- as.numeric(factor(hr_data$EducationField))

# Convert JobRole to a categorical variable
hr_data$JobRole <- as.numeric(factor(hr_data$JobRole))

##################################################################################################################
###Exploratory Data Analysis

##########   Attrition Likelihood   ##########
library(ggplot2)
hr_data %>% 
  group_by(Attrition) %>%
  summarize(counts = n()) %>%
  mutate(prop = (counts / sum(counts)) * 100) %>%
  arrange(desc(prop)) %>% 
  ggplot(aes("", counts)) +
  geom_col(
    position = "fill",
    color = "black",
    width = 1,
    aes(fill = factor(Attrition))
  ) +
  geom_text(
    aes(label = str_c(round(prop), "%"), group = factor(Attrition)),
    position = position_fill(vjust = 0.5),
    color = "white",
    size = 6,
    show.legend = FALSE,
    fontface = "bold"
  ) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual (values = c("#0066FF", "#CCCCCC")) +
  theme_void() +
  labs(title = "Are the Attrition Variable Balanced? ",
       fill = ""
  )
##########   Attrition by Monthly Income and Age   ##########
library(scales)
hr_data %>% select(Age, MonthlyIncome, Attrition) %>%
  ggplot(aes(Age, MonthlyIncome, color = MonthlyIncome)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(vars(Attrition)) +
  scale_y_continuous(labels = label_dollar()) +
  scale_color_viridis_c(labels = label_dollar()) +
  theme(strip.background = element_rect(fill = "#0066FF"),
        strip.text = element_text(color = "white", face = "bold", size = 8)) +
  labs(title = "Relationship between Monthly Income and Age Separated by Attrition",
       x = NULL,
       y = NULL,
       color = NULL)

##################################################################################################################
###Modelling
# Set a random seed for reproducibility
set.seed(123)

# Split the data into training and testing sets (70-30 split)
ind <- sample(1:nrow(hr_data), 0.7 * nrow(hr_data))
hr_data.train <- hr_data[ind, ]
hr_data.test <- hr_data[-ind, ]

##########   Model#1 - Logistic Regression   ##########

# The logistic regression model
logistic_model <- glm(Attrition ~ ., data = hr_data.train, family = binomial)

# Predictions on the training data
train_probabilities <- predict(logistic_model, hr_data.train, type = "response")
train_predictions <- ifelse(train_probabilities > 0.5, 1, 0)

# Predictions on the testing data
test_probabilities <- predict(logistic_model, hr_data.test, type = "response")
test_predictions <- ifelse(test_probabilities > 0.5, 1, 0)

# In-sample and Out-of-sample Accuracy
logistic_in_sample_accuracy <- mean(train_predictions == hr_data.train$Attrition)
logistic_out_of_sample_accuracy <- mean(test_predictions == hr_data.test$Attrition)

cat("In-Sample Accuracy:", logistic_in_sample_accuracy, "\n")
cat("Out-of-Sample Accuracy:", logistic_out_of_sample_accuracy, "\n")

##########   Model#2 - Classification Tree   ##########

# Load the required library for decision trees
library(rpart)

# Create a classification tree model
tree_model <- rpart(Attrition ~ ., data = hr_data.train, method = "class")

# Predictions on the training data
train_predictions <- predict(tree_model, hr_data.train, type = "class")

# Predictions on the testing data
test_predictions <- predict(tree_model, hr_data.test, type = "class")

# In-sample and Out-of-sample Accuracy
tree_IS_accuracy <- mean(train_predictions == hr_data.train$Attrition)
tree_OOS_accuracy <- mean(test_predictions == hr_data.test$Attrition)

cat("In-Sample Accuracy:", tree_IS_accuracy, "\n")
cat("Out-of-Sample Accuracy:", tree_OOS_accuracy, "\n")

##########   Model#3 - Random Forest   ##########

# Load the required library for Random Forest
library(randomForest)

# Create a Random Forest model
rf_model <- randomForest(Attrition ~ ., data = hr_data.train)

# Predictions on the training data
train_probabilities <- predict(rf_model, hr_data.train, type = "response")
train_predictions <- ifelse(train_probabilities > 0.5, 1, 0)

# Predictions on the testing data
test_probabilities <- predict(rf_model, hr_data.test, type = "response")
test_predictions <- ifelse(test_probabilities > 0.5, 1, 0)

# In-sample and Out-of-sample Accuracy
rf_IS_accuracy <- mean(train_predictions == hr_data.train$Attrition)
rf_OOS_accuracy <- mean(test_predictions == hr_data.test$Attrition)

cat("Random Forest - In-Sample Accuracy:", rf_IS_accuracy, "\n")
cat("Random Forest - Out-of-Sample Accuracy:", rf_OOS_accuracy, "\n")


##########   Model#4 - Logistic Regression with L1 (Lasso) Regularization   ##########

library(glmnet)

# Extract predictors and response variable
x_train <- hr_data.train[, !(names(hr_data.train) %in% "Attrition")]

y_train <- as.factor(hr_data.train$Attrition)

# Create a matrix of predictors
x_train_matrix <- as.matrix(x_train)

# The logistic regression model with L1 (Lasso) regularization
lasso_model <- glmnet::cv.glmnet(
  x = x_train_matrix,
  y = y_train,
  alpha = 1,
  family = "binomial",
  type.measure = "class",
  nfolds = 10
)

# Select the best lambda (regularization parameter)
best_lambda <- lasso_model$lambda.min

# Build the final model with the selected lambda
final_model <- glmnet::glmnet(
  x = x_train_matrix,
  y = y_train,
  alpha = 1,
  family = "binomial",
  lambda = best_lambda
)

# Predictions on the training data
train_probabilities <- predict(final_model, s = best_lambda, newx = x_train_matrix)
train_predictions <- ifelse(train_probabilities > 0.5, 1, 0)

# Predictions on the testing data
x_test_matrix <- as.matrix(hr_data.test[, !(names(hr_data.test) %in% "Attrition")])
test_probabilities_lasso <- predict(final_model, s = best_lambda, newx = x_test_matrix)
test_probabilities_lasso <- as.numeric(test_probabilities_lasso[, 1])
test_predictions_lasso <- ifelse(test_probabilities_lasso > 0.5, 1, 0)

# In-sample and Out-of-sample Accuracy
lasso_in_sample_accuracy <- mean(train_predictions == hr_data.train$Attrition)
lasso_out_of_sample_accuracy <- mean(test_predictions_lasso == hr_data.test$Attrition)

cat("In-Sample Accuracy (Logistic Regression with L1 (Lasso) Regularization):", lasso_in_sample_accuracy, "\n")
cat("Out-of-Sample Accuracy (Logistic Regression with L1 (Lasso) Regularization):", lasso_out_of_sample_accuracy, "\n")

##################################################################################################################
###Evaluation

##########   Accuracy of In and Out of sample   ##########

# Load the required libraries
library(ggplot2)

# Create a data frame with model names and accuracy values
model_names <- c("Logistic Regression", "Decision Tree", "Random Forest", "Lasso Regression")
OOS_accuracy_values <- c(logistic_out_of_sample_accuracy, tree_OOS_accuracy, rf_OOS_accuracy, lasso_out_of_sample_accuracy)
IS_accuracy_values <- c(logistic_in_sample_accuracy, tree_IS_accuracy, rf_IS_accuracy, lasso_in_sample_accuracy)

# Create data frames for in-sample and out-of-sample accuracy
accuracy_data <- data.frame(Model = c(model_names,model_names), Type = c("Out-of-Sample", "Out-of-Sample", "Out-of-Sample", "Out-of-Sample","In-Sample", "In-Sample", "In-Sample", "In-Sample"), Accuracy = c(OOS_accuracy_values, IS_accuracy_values))


# Create a bar plot
accuracy_plot <- ggplot(accuracy_data, aes(x = Model, y = Accuracy, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model Accuracy Comparison",
       x = "Model",
       y = "Accuracy") +
  theme_minimal() +
  scale_fill_manual(values = c("Out-of-Sample" = "#0066FF", "In-Sample" = "black"))

# Display the bar plot
print(accuracy_plot)

##########   Confusion Matrix   ##########

# Confusion matrix for Logistic Regression
test_probabilities_logistic <- predict(logistic_model, hr_data.test, type = "response")
test_predictions_logistic <- ifelse(test_probabilities_logistic > 0.5, 1, 0)
logistic_confusion <- table(Actual = hr_data.test$Attrition, Predicted = test_predictions_logistic)

# Confusion matrix for Decision Tree
test_probabilities_tree <- as.numeric(predict(tree_model, newdata = hr_data.test, type = "prob")[,1])
test_predictions_tree <- predict(tree_model, hr_data.test, type = "class")
tree_confusion <- table(Actual = hr_data.test$Attrition, Predicted = test_predictions_tree)

# Confusion matrix for Random Forest
test_probabilities_rf <- predict(rf_model, hr_data.test, type = "response")
test_predictions_rf <- ifelse(test_probabilities_rf > 0.5, 1, 0)
rf_confusion <- table(Actual = hr_data.test$Attrition, Predicted = test_predictions_rf)

# Confusion matrix for Lasso Regression
lasso_confusion <- table(Actual = hr_data.test$Attrition, Predicted = test_predictions_lasso)

# Display the confusion matrices
print("Confusion Matrix for Logistic Regression:")
print(logistic_confusion)

print("Confusion Matrix for Decision Tree:")
print(tree_confusion)

print("Confusion Matrix for Random Forest:")
print(rf_confusion)

print("Confusion Matrix for Lasso Regression:")
print(lasso_confusion)

##########   False Negative Rate   ##########

# Calculate FNR for each model
FNR_logistic <- logistic_confusion[1, 2] / (logistic_confusion[1, 1] + logistic_confusion[1, 2])
FNR_tree <- tree_confusion[1, 2] / (tree_confusion[1, 1] + tree_confusion[1, 2])
FNR_rf <- rf_confusion[1, 2] / (rf_confusion[1, 1] + rf_confusion[1, 2])
FNR_lasso <- lasso_confusion[1, 2] / (lasso_confusion[1, 1] + lasso_confusion[1, 2])

# Print TNR for each model
cat("Logistic Regression FNR (False Negative Rate):", FNR_logistic, "\n")
cat("Decision Tree FNR (False Negative Rate):", FNR_tree, "\n")
cat("Random Forest FNR (False Negative Rate):", FNR_rf, "\n")
cat("Lasso Regression FNR (False Negative Rate):", FNR_lasso, "\n")

# Create a data frame with model names and accuracy values
model_names <- c("Logistic Regression", "Decision Tree", "Random Forest", "Lasso Regression")
FNR_values <- c(FNR_logistic, FNR_tree, FNR_rf, FNR_lasso)

# Create data frames for in-sample and out-of-sample accuracy
FNR_data <- data.frame(Model = model_names, Type = "False Negative Rate", FNR = FNR_values)

# Create a bar plot
FNR_plot <- ggplot(FNR_data, aes(x = Model, y = FNR, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model FNR Comparison",
       x = "Model",
       y = "FNR") +
  theme_minimal() +
  scale_fill_manual(values = "#0066FF")

# Display the bar plot
print(FNR_plot)

##########   ROC Curve   ##########

# Load the pROC library
library(pROC)

# Define ROC curve for Logistic Regression model
roc_logistic <- roc(response = hr_data.test$Attrition, predictor = test_probabilities_logistic)

# Define ROC curve for Decision Tree model
roc_tree <- roc(response = hr_data.test$Attrition, predictor = test_probabilities_tree)

# Define ROC curve for Random Forest model
roc_rf <- roc(response = hr_data.test$Attrition, predictor = test_probabilities_rf)

# Define ROC curve for Lasso Regression model
roc_lasso <- roc(response = hr_data.test$Attrition, predictor = test_probabilities_lasso)

roc_plot <- plot.roc(roc_logistic, print.auc = TRUE, print.auc.y = 0.2, col = "red", print.thres = 0.2, print.thres.pattern = "Optimal Threshold: {threshold}")
roc_plot <- plot.roc(roc_tree, add = TRUE, col = "green")
roc_plot <- plot.roc(roc_rf, add = TRUE, col = "blue")
roc_plot <- plot.roc(roc_lasso, add = TRUE, col = "purple")

# Add labels and legend
legend("bottomright", legend = c("Logistic Regression", "Decision Tree", "Random Forest", "Lasso Regression"), col = c("red", "green", "blue", "purple"), lty = 1)

# Print the plot
roc_plot

# Print the AUC for each model
cat("AUC for Logistic Regression:", auc(roc_logistic), "\n")
cat("AUC for Decision Tree:", auc(roc_tree), "\n")
cat("AUC for Random Forest:", auc(roc_rf), "\n")
cat("AUC for Lasso Regression:", auc(roc_lasso), "\n")

# Create a data frame with model names and AUC values
model_names <- c("Logistic Regression", "Decision Tree", "Random Forest", "Lasso Regression")
AUC_values <- c(auc(roc_logistic), auc(roc_tree), auc(roc_rf), auc(roc_lasso))

# Create data frames for in-sample and out-of-sample AUC
AUC_data <- data.frame(Model = model_names, Type = "AUC", AUC = AUC_values)

# Create a bar plot for AUC
AUC_plot <- ggplot(AUC_data, aes(x = Model, y = AUC, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model AUC Comparison",
       x = "Model",
       y = "AUC") +
  theme_minimal() +
  scale_fill_manual(values = "Black")

# Display the bar plot
print(AUC_plot)

##################################################################################################################
### Implications for Profitability

# Define your cost-benefit matrix
cost.benefit.matrix <- c(0, -0.05, -0.2, 0.25)

pred_lasso <- test_predictions_lasso  # Replace with your Lasso model's predictions
true_values <- hr_data$Attrition  # Replace with the true values

# Calculate the expected profit based on the cost-benefit matrix and confusion matrix
expected_profit <- sum(cost.benefit.matrix * lasso_confusion)


##################################################################################################################

profictcurveOrder <- function(score, y, cost.benefit.m, K=100,...)
{
  
  threshold <-seq(from=min(score), to=max(score), by= (max(score)-min(score))/K)  
  profit <- rep(0,length(threshold))
  prop <- rep(0,length(threshold))
  for( i in 1:length(threshold) ){
    thr <- threshold[1+length(threshold)-i]
    confusion.matrix <- c( sum( (score>=thr) * My ),  sum( (score>=thr) * !My ) , sum( (score<thr) * My ),  sum( (score<thr) * !My))
    
    ### Expected profit
    profit[i] <- t(cost.benefit.m) %*% confusion.matrix
    prop[i] <- sum( (score<thr) ) / length(score)
  }
  plot(prop,profit, type="l")
  plot(prop,profit, type="l", xlab="Proportion of population", ylab="Profit", main="Profit Curve for Migration")
  plot(prop,1-threshold, type="l", xlab="Proportion of population", ylab="Churn Probability", main="Ranking of Customers")
  
}



profitcurveAll <- function(p,y, cost.benefit.m,...){
  y <- factor(y)
  n <- length(p)
  p <- as.vector(p)
  pp <- p[order(p, decreasing =TRUE, na.last = NA)]
  yy <- y[order(p, decreasing =TRUE, na.last = NA)]
  Q <- pp > matrix(rep(seq(0,1,length=100),n),ncol=100,byrow=TRUE)
  TN <- colSums(!Q[yy==levels(yy)[1],])
  FN <- colSums(Q[yy==levels(yy)[1],])
  TP <- colSums(Q[yy==levels(yy)[2],])
  FP <- colSums(!Q[yy==levels(yy)[2],])
  profit <- cost.benefit.m %*% rbind( TP, FP, FN, TN ) 
  plot(seq(0,1,length=100), profit, type="l", xlab="Proportion of data", ...)
  
}

profitcurve <- function(p,y, cost.benefit.m ,...){
  y <- factor(y)
  n <- length(p)
  p <- as.vector(p)
  pp <- p[order(p, decreasing =TRUE, na.last = NA)]
  yy <- y[order(p, decreasing =TRUE, na.last = NA)]
  profit <- rep.int(0,n)
  VV1<-(yy==levels(yy)[1])
  VV2<-(yy==levels(yy)[2])
  for ( kk in 1:n ){
    TN <- 0
    FN <- 0
    TP <- sum(VV2[1:kk])
    FP <- sum(!VV2[1:kk])
    profit[kk] <- cost.benefit.m %*% rbind( TP, FP, FN, TN ) 
  }
  plot(seq(from=1,to=n,by=1)/n, profit, type="l", xlab="Proportion of data", ylab="Profit", main="Profit Curve", ...)
  return (profit)
}
##################################################################################################################
library(ggplot2)
library(reshape2)

# Create a data frame with model names and metric values
model_names <- c("Logistic Regression", "Decision Tree", "Random Forest", "Lasso Regression")
OOS_accuracy <- c(logistic_out_of_sample_accuracy, tree_OOS_accuracy, rf_OOS_accuracy, lasso_out_of_sample_accuracy)
IS_accuracy <- c(logistic_in_sample_accuracy, tree_IS_accuracy, rf_IS_accuracy, lasso_in_sample_accuracy)
Sensitivity <- c(1-FNR_logistic, 1-FNR_tree, 1-FNR_rf, 1-FNR_lasso)
AUC <- c(auc(roc_logistic), auc(roc_tree), auc(roc_rf), auc(roc_lasso))

metrics_data <- data.frame(Model = model_names, 
                           OOS_Accuracy = OOS_accuracy, 
                           IS_Accuracy = IS_accuracy, 
                           Sensitivity = Sensitivity, 
                           AUC = AUC)

# Reshape the data for plotting
metrics_data <- melt(metrics_data, id.vars = "Model", variable.name = "Metric", value.name = "Value")

# Create a grouped bar plot
metrics_plot <- ggplot(metrics_data, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model Performance Comparison",
       x = "Model",
       y = "Value") +
  theme_minimal() +
  scale_fill_manual(values = c("OOS_Accuracy" = "#0066FF", "IS_Accuracy" = "black", "Sensitivity" = "grey", "AUC" = "red"))

# Display the grouped bar plot
print(metrics_plot)