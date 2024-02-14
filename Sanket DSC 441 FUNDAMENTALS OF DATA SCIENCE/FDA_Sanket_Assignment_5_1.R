
library(dplyr)
library(pROC)

# Importing data in R

df = read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv", header = T)


# --------------------------- Data Understanding ----------------------------
dim(df)
head(df)

str(df)
summary(df)

# Checking the class of the columns
column_types <- sapply(df, class)
print(column_types)

# Count the number of categorical and numerical variables
num_categorical <- sum(column_types == "factor" | column_types == "character")
num_numerical <- sum(column_types == "numeric" | column_types == "integer")

# Print the results
cat("Number of Categorical Variables:", num_categorical, "\n")
cat("Number of Numerical Variables:", num_numerical, "\n")

# --------------------------- Data Cleaning -------------------------------

# Checking if data has unique value columns
unique_counts <- sapply(df, function(x) length(unique(x)))

# Display the count of unique values for each column
print(unique_counts)

df <- df[, !(colnames(df) %in% c("customerID"))]

head(df)
sapply(df, function(x) length(unique(x)))

# Checking if data has any NA values column wise
na_percentages <- colMeans(is.na(df)) * 100
na_percentages

# Checking if data has any NA values row wise
percentage_na_rows <- mean(apply(df, 1, function(row) any(is.na(row)))) * 100
print(percentage_na_rows)


# Creating a function to impute NA values
imputeNA <- function(data) {
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      # Impute NA with mean for numeric variables
      data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
    } else if (is.factor(data[[col]]) | is.character(data[[col]])) {
      # Impute NA with mode for categorical or factor variables
      mode_val <- as.character(sort(table(data[[col]]), decreasing = TRUE)[1])
      data[[col]][is.na(data[[col]])] <- mode_val
    }
    # If neither numeric nor categorical, do nothing
  }
  return(data)
}

df<-imputeNA(df)

# Checking NA values after imputation
percentage_na_rows_1 <- mean(apply(df, 1, function(row) any(is.na(row)))) * 100
print(percentage_na_rows_1)

# Checking outliers in numerical variables by Z-score method

# Function to detect outliers using Z-score
detect_outliers <- function(x, threshold = 3) {
  z_scores <- scale(x)
  abs_z_scores <- abs(z_scores)
  outliers <- abs_z_scores > threshold
  return(outliers)
}

# Apply the function to each numerical variable in the dataframe
numerical_vars <- sapply(df, is.numeric)
outliers_df <- lapply(df[, numerical_vars], detect_outliers)

# Print the results
for (i in seq_along(outliers_df)) {
  var_name <- names(outliers_df)[i]
  cat("Outliers in variable", var_name, ":", any(outliers_df[[i]]), "\n")
}
# No outliers found


# Checking if class of any of the variables needs to be changed

sapply(df, class)

# Variable SeniorCitizen should be factor.
df$SeniorCitizen <- as.factor(df$SeniorCitizen)

# Converting categorical variables to factors
df <- df %>%
  mutate_if(is.character,as.factor)

class(df$SeniorCitizen)

# ---------------------------- Target Variable Analysis -----------------------

# Create a table of counts for each level of 'Churn'
churn_counts <- table(df$Churn)

barplot(churn_counts, main="Churn Distribution", xlab="Churn", ylab="Count", col=c("skyblue", "lightgreen"))

# Calculate the percentage of 'Yes' and 'No' values
churn_percentage <- prop.table(table(df$Churn)) * 100

cat("Percentage of 'Yes' in Churn:", churn_percentage["Yes"], "%\n")
cat("Percentage of 'No' in Churn:", churn_percentage["No"], "%\n")

# There is no issue of class imbalance.

# ------------------------- Exploratory Data Analysis -----------------------

# Boxplot for MonthlyCharges
boxplot(MonthlyCharges ~ Churn, data = df, main = "MonthlyCharges by Churn", 
        xlab = "Churn", ylab = "MonthlyCharges", col = c("lightblue", "lightgreen"))

# Boxplot for TotalCharges
boxplot(TotalCharges ~ Churn, data = df, main = "TotalCharges by Churn", 
        xlab = "Churn", ylab = "TotalCharges", col = c("lightblue", "lightgreen"))

# Boxplot for TotalCharges
boxplot(tenure ~ Churn, data = df, main = "Tenure by Churn", 
        xlab = "Churn", ylab = "Tenure", col = c("lightblue", "lightgreen"))

# There is no clear separation between Yes and No for both MonthlyCharges and TotalCharges. Hence we cannot say those
# There is slight separation between Yes and No by tenure.

ggplot(df, aes(x = InternetService, fill = Churn)) +
  geom_bar(position = "dodge") +
  labs(title = "Churn by Internet Service", x = "Internet Service", y = "Count") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  theme_minimal()

ggplot(df, aes(x = OnlineSecurity, fill = Churn)) +
  geom_bar(position = "dodge") +
  labs(title = "Churn by OnlineSecurity", x = "OnlineSecurity", y = "Count") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  theme_minimal()

ggplot(df, aes(x = PaperlessBilling, fill = Churn)) +
  geom_bar(position = "dodge") +
  labs(title = "Churn by PaperlessBilling", x = "PaperlessBilling", y = "Count") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  theme_minimal()

# We are not able to see any non linear relations with the help of above bar graphs with churn.

# ---------------------------- K Means Clustering ----------------------------

# Converting data to dummies
df_combined_dummies <- df %>% model.matrix(~ . - 1, data = .) %>%  as.data.frame()
dim(df_combined_dummies)

df_combined_dummies_kmeans <- df_combined_dummies[, !(colnames(df_combined_dummies) %in% c("ChurnYes"))]

# Fit the data with k-means
kmeans <- kmeans(df_combined_dummies_kmeans, centers = 2)
kmeans

# display the cluster plot
fviz_cluster(kmeans, data = df_combined_dummies_kmeans)

# Pulling out classifiers
kmeans_classifications = kmeans$cluster
kmeans_classifications <- ifelse(kmeans_classifications == 1, 0, 1)

# Create a dataframe
result <- data.frame(Churn =  df_combined_dummies$ChurnYes,kmeans_classifications = kmeans_classifications)

# Crosstab for K Means
result %>% group_by(kmeans_classifications) %>% select(kmeans_classifications, Churn) %>% table()

# Assign values from the confusion matrix
TP_KMeans <- 1548  # True Positives
TN_KMeans <- 1768  # True Negatives
FP_KMeans <- 321   # False Positives
FN_KMeans <- 3406  # False Negatives

# Calculate metrics
accuracy_KMeans <- (TP_KMeans + TN_KMeans) / (TP_KMeans + TN_KMeans + FP_KMeans + FN_KMeans)
precision_KMeans <- TP_KMeans / (TP_KMeans + FP_KMeans)
sensitivity_KMeans <- TP_KMeans / (TP_KMeans + FN_KMeans)
specificity_KMeans <- TN_KMeans / (TN_KMeans + FP_KMeans)

# Print the results with the "KMeans" suffix
cat("Accuracy_KMeans:", accuracy_KMeans, "\n")
cat("Precision_KMeans:", precision_KMeans, "\n")
cat("Sensitivity_KMeans:", sensitivity_KMeans, "\n")
cat("Specificity_KMeans:", specificity_KMeans, "\n")

# ROC plot 
roc_curve_kmeans <- roc(result$Churn, result$kmeans_classifications)

# Plot the ROC curve
plot(roc_curve_kmeans, main = "ROC Curve", col = "red", lwd = 2)

# Add a legend
legend("bottomright", legend = c("ROC Curve"), col = "red", lty = 1, cex = 0.8)

# Calculate and print the AUC (Area Under the Curve)
cat("AUC:", auc(roc_curve_kmeans), "\n")

# --------------------------- HAC Clustering ----------------------------

library(cluster)
library(dendextend)
library(cluster)
library(factoextra)


# finding distances with the help of gower metric as we have categorical and numeric data
gower_dist <- daisy(df, metric = "gower")

# Finding the optimum number of clusters with the help of knee plot
fviz_nbclust(df, FUN = hcut, method = "wss")

# We will choose optimum number of clusters as 2.

# fit the data using average linkage method as we have more number of clusters
hfit <- hclust(gower_dist, method = 'average')

# Build the new model
hac_gower_average <- cutree(hfit, k=2)
hac_gower_average <- ifelse(hac_gower_average == 1, 0, 1)

result$HAC_predictions <- hac_gower_average

# Crosstab for Decision Tree
result %>% group_by(HAC_predictions) %>% select(HAC_predictions, Churn) %>% table()

# Assign values from the confusion matrix
TP_HAC <- 113    # True Positives
TN_HAC <- 3761   # True Negatives
FP_HAC <- 1756   # False Positives
FN_HAC <- 1413   # False Negatives

# Calculate metrics
accuracy_HAC <- (TP_HAC + TN_HAC) / (TP_HAC + TN_HAC + FP_HAC + FN_HAC)
precision_HAC <- TP_HAC / (TP_HAC + FP_HAC)
sensitivity_HAC <- TP_HAC / (TP_HAC + FN_HAC)
specificity_HAC <- TN_HAC / (TN_HAC + FP_HAC)

# Print the results with the "HAC" suffix
cat("Accuracy_HAC:", accuracy_HAC, "\n")
cat("Precision_HAC:", precision_HAC, "\n")
cat("Sensitivity_HAC:", sensitivity_HAC, "\n")
cat("Specificity_HAC:", specificity_HAC, "\n")

# ROC curve
result$HAC_predictions <- as.numeric(as.character(result$HAC_predictions))

roc_curve_HAC <- roc(result$Churn, result$HAC_predictions)

# Plot the ROC curve
plot(roc_curve_HAC, main = "ROC Curve", col = "blue", lwd = 2)

# Add a legend
legend("bottomright", legend = c("ROC Curve"), col = "blue", lty = 1, cex = 0.8)

# Calculate and print the AUC (Area Under the Curve)
cat("AUC:", auc(roc_curve_HAC), "\n")

# --------------------------- Decision Tree Classifier ------------------------

library(class)
library(e1071)
library(rpart)
library(caret)

set.seed(123)
train_indices <- createDataPartition(df_combined_dummies$ChurnYes, p = 0.8, list = FALSE)
df_train <- df_combined_dummies[train_indices, ]
df_test <- df_combined_dummies[-train_indices, ]


# Decision Trees
colnames(df) <- make.names(colnames(df))
tree_model <- rpart(ChurnYes ~ ., data = df_train, method = "class")
tree_predictions <- predict(tree_model, df_test, type = "class")
tree_accuracy_test <- sum(tree_predictions == df_test$ChurnYes) / length(df_test$ChurnYes)
print(tree_accuracy_test)

# predicting class for all data
tree_predictions_all <- predict(tree_model, df_combined_dummies, type = "class")

result$tree_predictions <- tree_predictions_all

# Crosstab for Decision Tree
result %>% group_by(tree_predictions) %>% select(tree_predictions, Churn) %>% table()

# Assign values from the confusion matrix
TP_tree <- 761  # True Positives
TN_tree <- 4807  # True Negatives
FP_tree <- 1108  # False Positives
FN_tree <- 367   # False Negatives

# Calculate metrics
accuracy_tree <- (TP_tree + TN_tree) / (TP_tree + TN_tree + FP_tree + FN_tree)
precision_tree <- TP_tree / (TP_tree + FP_tree)
sensitivity_tree <- TP_tree / (TP_tree + FN_tree)
specificity_tree <- TN_tree / (TN_tree + FP_tree)

# Print the results
cat("Accuracy_tree:", accuracy_tree, "\n")
cat("Precision_tree:", precision_tree, "\n")
cat("Sensitivity_tree:", sensitivity_tree, "\n")
cat("Specificity_tree:", specificity_tree, "\n")

# ROC curve
result$tree_predictions <- as.numeric(as.character(result$tree_predictions))

roc_curve_tree <- roc(result$Churn, result$tree_predictions)

# Plot the ROC curve
plot(roc_curve_tree, main = "ROC Curve", col = "blue", lwd = 2)

# Add a legend
legend("bottomright", legend = c("ROC Curve"), col = "blue", lty = 1, cex = 0.8)

# Calculate and print the AUC (Area Under the Curve)
cat("AUC:", auc(roc_curve_tree), "\n")

# -------------------------- SVM Classifier -----------------------

# Support Vector Machine (SVM)
library(e1071)

df_train$ChurnYes <- as.factor(df_train$ChurnYes)
df_test$ChurnYes <- as.factor(df_test$ChurnYes)

# Train the SVM model
svm_model <- svm(ChurnYes ~ ., data = df_train, kernel = "linear")

# Make predictions on the test set
svm_predictions <- predict(svm_model, df_test)

# Evaluate accuracy
svm_accuracy_test <- sum(svm_predictions == df_test$ChurnYes) / length(df_test$ChurnYes)
print(svm_accuracy_test)

# predicting class for all data
svm_predictions_all <- predict(svm_model, df_combined_dummies)

result$svm_predictions <- svm_predictions_all

# Crosstab for Decision Tree
result %>% group_by(svm_predictions) %>% select(svm_predictions, Churn) %>% table()

# Assign values from the confusion matrix
TP_svm <- 996   # True Positives
TN_svm <- 4630  # True Negatives
FP_svm <- 873   # False Positives
FN_svm <- 544   # False Negatives

# Calculate metrics
accuracy_svm <- (TP_svm + TN_svm) / (TP_svm + TN_svm + FP_svm + FN_svm)
precision_svm <- TP_svm / (TP_svm + FP_svm)
sensitivity_svm <- TP_svm / (TP_svm + FN_svm)
specificity_svm <- TN_svm / (TN_svm + FP_svm)

# Print the results with the "svm" suffix
cat("Accuracy_svm:", accuracy_svm, "\n")
cat("Precision_svm:", precision_svm, "\n")
cat("Sensitivity_svm:", sensitivity_svm, "\n")
cat("Specificity_svm:", specificity_svm, "\n")

# ROC curve
result$svm_predictions <- as.numeric(as.character(result$svm_predictions))

roc_curve_svm <- roc(result$Churn, result$svm_predictions)

# Plot the ROC curve
plot(roc_curve_svm, main = "ROC Curve", col = "blue", lwd = 2)

# Add a legend
legend("bottomright", legend = c("ROC Curve"), col = "blue", lty = 1, cex = 0.8)

# Calculate and print the AUC (Area Under the Curve)
cat("AUC:", auc(roc_curve_svm), "\n")

# ------------------------- Comparing Final Results ----------------------

Final_Results <- data.frame(
  Classifier = c("KMeans", "HAC", "Decision Tree", "SVM"),
  Accuracy = c(accuracy_KMeans, accuracy_HAC, accuracy_tree, accuracy_svm),
  Precision = c(precision_KMeans, precision_HAC, precision_tree, precision_svm),
  Sensitivity = c(sensitivity_KMeans, sensitivity_HAC, sensitivity_tree, sensitivity_svm),
  Specificity = c(specificity_KMeans, specificity_HAC, specificity_tree, specificity_svm),
  AUC = c(auc(roc_curve_kmeans), auc(roc_curve_HAC), auc(roc_curve_tree), auc(roc_curve_svm))
)

print(Final_Results)

# ---------------------------------- No Need ---------------------------------

df$SeniorCitizen <- as.factor(df$SeniorCitizen)

# Identify numeric and categorical columns
numeric_cols <- sapply(df, is.numeric)
categorical_cols <- sapply(df, function(x) is.factor(x) | is.character(x))

# Create df_numeric and df_categorical
df_numeric <- df[, numeric_cols]
df_categorical <- df[, categorical_cols]

# Scale the data
df_numeric <- data.frame(scale(df_numeric))  
class(df_numeric)

New_df <- cbind(df_numeric, df_categorical)

# Converting data to dummies
df_combined_dummies_1 <- New_df %>% model.matrix(~ . - 1, data = .) %>%  as.data.frame()
dim(df_combined_dummies_1)

# Fit the data with k-means
kmeans <- kmeans(df_combined_dummies_1, centers = 2)
kmeans

# display the cluster plot
fviz_cluster(kmeans, data = df_combined_dummies_1)

# Pulling out classifiers
kmeans_classifications = kmeans$cluster

# Create a dataframe
result <- data.frame(Churn =  df$Churn,kmeans_classifications = kmeans_classifications)

# Crosstab for K Means
result %>% group_by(kmeans_classifications) %>% select(kmeans_classifications, Churn) %>% table()

result$kmeans_classifications <- as.numeric(as.character(result$kmeans_classifications))


