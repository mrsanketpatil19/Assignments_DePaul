
# Problem 1:

# Reading the data

df = read.csv("churn_train.csv", header = T)
dim(df)
head(df)

library(ggplot2)

# Boxplot for Age
ggplot(df, aes(x = as.factor(CHURN), y = AGE)) +
  geom_boxplot() +
  labs(title = "Boxplot for Age by Churn", x = "Churn", y = "Age")
# By looking at the boxplot, we can see if the age value is in between 31 to 52, loan will not be approved. Apposite to that, if, age is in between 22 to 27, there is probability of loan getting approved. There are some outliers as well for age group 32 to 52 as their loan approved.

# Boxplot for PCT_CHNG_BILL_AMT
ggplot(df, aes(x = as.factor(CHURN), y = PCT_CHNG_BILL_AMT)) +
  geom_boxplot() +
  labs(title = "Boxplot for PCT_CHNG_BILL_AMT by Churn", x = "Churn", y = "PCT_CHNG_BILL_AMT")
# changes in bill will not affect the Churn value as we cannot see a separation between the two boxplots shown above.


# Question 2: 

# Fitting logistic regression model
model <- glm(CHURN ~ ., data = df, family = "binomial")
summary(model)

# Pulling out non significant variables
coefficients_table <- summary(model)$coefficients

# Filter variables with p-values greater than 0.05
non_significant_vars <- coefficients_table[coefficients_table[, 4] > 0.05, ]

# Arrange in descending order based on p-values
non_significant_vars <- non_significant_vars[order(-non_significant_vars[, 4]), ]

# Display the result
non_significant_vars

# As there are many insignificant variables, we will use backward elimination method to select significant variables.

# Apply backward selection
backward_model <- step(model, direction = "backward", trace = 0)
summary(backward_model)

final_model <- glm(formula = CHURN ~ TOT_ACTV_SRV_CNT + AGE + PCT_CHNG_IB_SMS_CNT + 
                     PCT_CHNG_BILL_AMT + COMPLAINT, family = "binomial", data = df)
summary(final_model)

# Expression for the fitted model is as follows
#logit(p)=7.11401−0.54892×TOT_ACTV_SRV_CNT−0.17781×AGE−0.41230×PCT_CHNG_IB_SMS_CNT−0.39914×PCT_CHNG_BILL_AMT+0.50489×COMPLAINT

# Question 3:

Intercept = 7.11401
Odds Ratio = e^7.11401 = 1230.83
# The odds of churn when all other variables are zero are approximately 1231 times higher than the odds of not churning.

# TOT_ACTV_SRV_CNT:
Coefficient: -0.54892
Odds Ratio = e^-0.54892 = 0.578
# For each unit increase in the total number of active services, the odds of churn decrease by approximately 42.2%.

# AGE:
Coefficient: -0.17781
Odds Ratio = e^-0.17781 = 0.837
# For each one-year increase in age, the odds of churn decrease by approximately 16.3%.

# PCT_CHNG_IB_SMS_CNT:
Coefficient: -0.41230
Odds Ratio = e^−0.41230 = 0.662
# A one-unit increase in the percent change of incoming SMS over the previous months is associated with a 33.8% decrease in the odds of churn.

PCT_CHNG_BILL_AMT:
Coefficient: -0.39914
Odds Ratio = e^−0.39914 = 0.670
# A one-unit increase in the percent change of bill amount over the previous months is associated with a 33.0% decrease in the odds of churn.

COMPLAINT:
  Coefficient: 0.50489
Odds Ratio = 0.50489 = 1.656
# Customers with a complaint in the last two months have approximately 1.656 times higher odds of churning compared to those without a complaint.

# Question 4 : 

# Creating a new data frame with provided values
new_customer <- data.frame(GENDER = "M", EDUCATION = 3, LAST_PRICE_PLAN_CHNG_DAY_CNT = 0, TOT_ACTV_SRV_CNT = 4, 
                           AGE = 43, PCT_CHNG_IB_SMS_CNT = 1.04, PCT_CHNG_BILL_AMT = 1.19, COMPLAINT = 1)

# Predicted Probability
predicted_prob <- predict(final_model, newdata = new_customer, type = "response")

# Prediction Interval
predict_interval <- predict(final_model, newdata = new_customer, interval = "prediction")

# Display the results
predicted_prob
predict_interval

# Question 5:

source("Classify_functions.R")

df_test = read.csv("churn_test.csv", header = T)
dim(df_test)
head(df_test)

# Get predicted probabilities for the test data
predicted_probs_test <- predict(final_model, newdata = df_test, type = "response")

classify <- function(predicted_probs, threshold) {
  # Classify as 1 if predicted probability is greater than or equal to the threshold, else 0
  predictions <- ifelse(predicted_probs >= threshold, 1, 0)
  return(predictions)
}

# Threshold = 0.5

threshold <- 0.5
class_predictions <- classify(predicted_probs_test, threshold)

# Create the classification matrix
classification_matrix <- table(df_test$CHURN, class_predictions)

# Confusion Matrix
conf_matrix <- matrix(c(46, 13, 3, 36), nrow = 2, byrow = TRUE)
colnames(conf_matrix) <- c("Predicted 0", "Predicted 1")
rownames(conf_matrix) <- c("Actual 0", "Actual 1")

# Function to calculate metrics
calculate_metrics <- function(conf_matrix) {
  TP <- conf_matrix[1, 1]
  TN <- conf_matrix[2, 2]
  FP <- conf_matrix[1, 2]
  FN <- conf_matrix[2, 1]
  
  # Accuracy
  accuracy <- (TP + TN) / sum(conf_matrix)
  
  # Sensitivity (Recall)
  sensitivity <- TP / (TP + FN)
  
  # Specificity
  specificity <- TN / (TN + FP)
  
  # Precision
  precision <- TP / (TP + FP)
  
  return(c(Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, Precision = precision))
}

# Calculate metrics
metrics_0.5 <- data.frame(calculate_metrics(conf_matrix))
metrics_0.5

# Threshold = 0.6

threshold <- 0.6
class_predictions <- classify(predicted_probs_test, threshold)

# Create the classification matrix
classification_matrix <- table(df_test$CHURN, class_predictions)

# Confusion Matrix
conf_matrix <- matrix(c(46, 13, 5, 34), nrow = 2, byrow = TRUE)
colnames(conf_matrix) <- c("Predicted 0", "Predicted 1")
rownames(conf_matrix) <- c("Actual 0", "Actual 1")

metrics_0.6 <- data.frame(calculate_metrics(conf_matrix))
metrics_0.6

# Threshold = 0.7

threshold <- 0.7
class_predictions <- classify(predicted_probs_test, threshold)

# Create the classification matrix
classification_matrix <- table(df_test$CHURN, class_predictions)

# Confusion Matrix
conf_matrix <- matrix(c(53, 6, 8, 31), nrow = 2, byrow = TRUE)
colnames(conf_matrix) <- c("Predicted 0", "Predicted 1")
rownames(conf_matrix) <- c("Actual 0", "Actual 1")

metrics_0.7 <- data.frame(calculate_metrics(conf_matrix))
metrics_0.7

# Threshold = 4

threshold <- 0.4
class_predictions <- classify(predicted_probs_test, threshold)

# Create the classification matrix
classification_matrix <- table(df_test$CHURN, class_predictions)

# Confusion Matrix
conf_matrix <- matrix(c(42, 17, 3, 36), nrow = 2, byrow = TRUE)
colnames(conf_matrix) <- c("Predicted 0", "Predicted 1")
rownames(conf_matrix) <- c("Actual 0", "Actual 1")

metrics_0.4 <- data.frame(calculate_metrics(conf_matrix))
metrics_0.4

results <- metrics_0.4
results <- cbind(metrics_0.4,metrics_0.5,metrics_0.6,metrics_0.7)
colnames(results) <- c("metrics_0.4", "metrics_0.5", "metrics_0.6", "metrics_0.7")

print(results)

# by looking at the above table, we will choose threshold as 0.7 as it shown a constant score for all 4 modules.

# Problem 2:

# Importing data into R

data <- read.table("energytemp.txt", header = TRUE)
dim(data)
head(data)

# Question 1:

# Scatterplot
plot(data$energy , data$temp, main = "Scatterplot of ENERGY vs TEMP", 
     xlab = "TEMPD (Temperature Difference)", ylab = "ENERGY (Energy Consumption)")

# We can see positive linear relationship between Tempd and Energy explaining that increase in temperature difference will cause increase in the Energy consumption.


# Question 2:

# Create new variables
data$tempd2 <- data$temp^2
data$tempd3 <- data$temp^3

# Fit a cubic model
model <- lm(energy ~ temp + tempd2 + tempd3, data = data)
summary(model)

# Question 3:

# By looking at the summary of the model, we can say that all the 3 variables are significant as P values for respective variables are < 0.05.

# Question 4:

# Residuals vs Predicted
predicted_values <- predict(model)
residuals <- rstudent(model)

# Scatter plot residuals vs predicted_values
plot(predicted_values, residuals, main="Studentized Residuals vs. Predicted Values",
     xlab="Predicted Values", ylab="Studentized Residuals", col="blue", pch=16)
abline(h=0, col="red")

# By seeing at the plot, we can see that residuals are randomly distributed and no pattern detected. However we cannot conclude anyting with this as we have very less amount of data. There are no outliers present.

# Residuals vs Temp 
plot(data$temp, residuals, main="Studentized Residuals vs. Temp",
     xlab="Temp", ylab="Studentized Residuals", col="blue", pch=16)
abline(h=0, col="red")

# Residuals vs Temp2
plot(data$tempd2, residuals, main="Studentized Residuals vs. Temp2",
     xlab="Temp2", ylab="Studentized Residuals", col="blue", pch=16)
abline(h=0, col="red")

# Residuals vs Temp3
plot(data$tempd3, residuals, main="Studentized Residuals vs. Temp3",
     xlab="Temp3", ylab="Studentized Residuals", col="blue", pch=16)
abline(h=0, col="red")

# Create a normal probability plot
qqnorm(rstandard(model), main="Normal Q-Q Plot")
qqline(rstandard(model), col="red")

# As we see in the QQ plot, residulas follow the line hence there should not be skewness in the distribution of the outliers. We can say that it is a good model.

# Question 5:

# y=−17.036232+24.523999⋅temp−1.490029⋅tempd2+0.029278⋅tempd3

# qUESTION 6:

# Create a new dataframe with the values for prediction
new_data <- data.frame(temp = 10, tempd2 = 10^2, tempd3 = 10^3)

# Predict using the fitted model
predicted_energy <- predict(model, newdata = new_data)
predicted_energy



