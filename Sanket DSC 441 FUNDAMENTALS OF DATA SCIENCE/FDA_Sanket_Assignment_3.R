
# PROBLEM 1

# Loading required libraries
library(rpart)
library(rpart.plot)
library(tree)
library(caret)

# Importing data in R

df = read.csv("breast_cancer_updated.csv", header = T)
dim(df)
head(df)

df <- df[, !names(df) %in% "IDNumber"]
head(df)

# Question 1:

# Removing NA values from data
colMeans(is.na(df)) * 100
df <- na.omit(df)

# Fit decision tree model
cancer_model <- rpart(Class ~ ., data = df)

# Applying decision tree learning using 10-fold cross-validation
set.seed(123)  
ctrl <- trainControl(method = "cv", number = 10, savePredictions = TRUE)
model <- train(Class ~ ., data = df, method = "rpart", trControl = ctrl)

# Report accuracy
model$results$Accuracy

# Question 2 :

# Generating a visualization of the decision tree
rpart.plot(cancer_model, shadow.col = "gray", nn = TRUE)


# Question3 :

rules <- rpart.rules(cancer_model)
print(rules)

# Rule 1 - if UniformCellSize >= 3 and UniformCellShape < 3 and BlandChromatin < 4 then Class = 0.00
# Rule 2 -if UniformCellSize < 3 and BareNuclei < 6 then Class = 0.01
# Rule 3 - if UniformCellSize is between 3 and 5 and UniformCellShape >= 3 and BareNuclei < 3 then Class = 0.29
# Rule 4 - if UniformCellSize >= 3 and UniformCellShape < 3 and BlandChromatin >= 4 then Class = 0.71
# Rule 5 - if UniformCellSize is between 3 and 5 and UniformCellShape >= 3 and BareNuclei >= 3 then Class = 0.87
# Rule 6 - if UniformCellSize < 3 and BareNuclei >= 6 then Class = 0.88
# Rule 7 - if UniformCellSize >= 5 and UniformCellShape >= 3 then Class = 0.98


# PROBLEM 2 :

# Load libraries
library(dplyr)
library(rpart)
library(caret)

# Load the storms data
data(storms, package = "dplyr")

# View the data
str(storms)
dim(storms)

# Convert the target variable (category) to a factor
storms$category <- as.factor(storms$category)

# Removing NA values from data
colMeans(is.na(storms)) * 100
storms <- na.omit(storms)

sapply(storms, function(x) length(unique(x)))
sapply(storms, function(x) class(x))

# Removing name variable from data as it will take too much time to train decision tree
storms <- storms[, !names(storms) %in% "name"]
head(storms)

# Question 1:
# Build a decision tree
# Training the decision tree model with specified hyperparameters
set.seed(123)  
cv_model <- train(category ~ ., data = storms,
                    method = "rpart",
                    trControl = trainControl(method = "cv", number = 5),
                    control = rpart.control(maxdepth = 2, minsplit = 5, minbucket = 3))

# Printing the Accuracy of the model
print(cv_model$results$Accuracy)

# Question 2 : 

# Creating a train/test partition
set.seed(789) 
splitIndex <- createDataPartition(storms$category, p = 0.8, list = FALSE)
train_storms <- storms[splitIndex, ]
test_storms <- storms[-splitIndex, ]


tree_model_train <- rpart(category ~ ., data = train_storms, method = "class", minsplit = 5, maxdepth = 2, minbucket = 3)


# Predicting the category of both train and test data
predictions_storms_train <- predict(tree_model_train, newdata = train_storms,type = "class")
predictions_storms_test <- predict(tree_model_train, newdata = test_storms, type = "class")

# Confusion matrix to evaluate accuracy
conf_matrix_storms_train <- confusionMatrix(predictions_storms_train, train_storms$category)
conf_matrix_storms_test <- confusionMatrix(predictions_storms_test, test_storms$category)

# Confusion matrix table
conf_matrix_train<-table(predictions_storms_train, train_storms$category)
conf_matrix_test<-table(predictions_storms_test, test_storms$category)
print(conf_matrix_train)
print(conf_matrix_test)

accuracy_storms_train <- conf_matrix_storms_train$overall["Accuracy"]
accuracy_storms_test <- conf_matrix_storms_test$overall["Accuracy"]

# Print the accuracy
print(paste("Accuracy_train:", round(accuracy_storms_train,4)))
print(paste("Accuracy_test:", round(accuracy_storms_test,4)))

# The model performs well to predict class 1 and 2 in both training and testing data which can be proved by iths high diagonal count.
# Whereas, to classify class 3, the model fails. It might tells us that there is lack of representative samples for class 3.
# In both training and testing data, classes 4 and 5 show some misclassifications.

# Also model is working similar on both the data sets i.e. train data and test data.
# As model is performing similar on test and train data, it means that the model has generalized well to new, unseen data.
# Model is maintaining similar performance on training and testing data.
# In conclusion, model is not overfitting the training data.



# PROBLEM 3 :

library(rpart)
library(ggplot2)

# Splitting data into 80% 20% split

set.seed(678) 
splitIndex_3 <- createDataPartition(storms$category, p = 0.8, list = FALSE)
train_data_3 <- storms[splitIndex, ]
test_data_3 <- storms[-splitIndex, ]


# Tree 1
storms_tree_1 <- rpart(category ~ ., data = train_data_3, method = "class", minsplit = 5, maxdepth = 2, minbucket = 3)

predictions_train_tree_1 <- predict(storms_tree_1, newdata = train_data_3, type = "class")
predictions_test_tree_1 <- predict(storms_tree_1, newdata = test_data_3, type = "class")

conf_matrix_train_tree_1 <- confusionMatrix(predictions_train_tree_1, train_data_3$category)
conf_matrix_test_tree_1 <- confusionMatrix(predictions_test_tree_1, test_data_3$category)

accuracy_train_tree_1 <- conf_matrix_train_tree_1$overall["Accuracy"]
accuracy_test_tree_1 <- conf_matrix_test_tree_1$overall["Accuracy"]

# Checking the nodes of the tree 
nodes_1<-sum(storms_tree_1$frame$var == "<leaf>")

comp_tbl <- data.frame("Nodes" = nodes_1, "TrainAccuracy" = accuracy_train_tree_1, "TestAccuracy" = accuracy_test_tree_1,
                       "Minsplit" = 5, "Maxdepth" = 2, "Minbucket" = 3)


# Tree 2
storms_tree_2 <- rpart(category ~ ., data = train_data_3, method = "class", minsplit = 10, maxdepth = 2, minbucket = 6)

predictions_train_tree_2 <- predict(storms_tree_2, newdata = train_data_3, type = "class")
predictions_test_tree_2 <- predict(storms_tree_2, newdata = test_data_3, type = "class")

conf_matrix_train_tree_2 <- confusionMatrix(predictions_train_tree_2, train_data_3$category)
conf_matrix_test_tree_2 <- confusionMatrix(predictions_test_tree_2, test_data_3$category)

accuracy_train_tree_2 <- conf_matrix_train_tree_2$overall["Accuracy"]
accuracy_test_tree_2 <- conf_matrix_test_tree_2$overall["Accuracy"]

# Checking the nodes of the tree 
nodes_2<-sum(storms_tree_2$frame$var == "<leaf>")

comp_tbl <- comp_tbl %>% rbind(list(nodes_2, accuracy_train_tree_2, accuracy_test_tree_2, 10, 2, 6))


# Tree 3
storms_tree_3 <- rpart(category ~ ., data = train_data_3, method = "class", minsplit = 15, maxdepth = 2, minbucket = 9)

predictions_train_tree_3 <- predict(storms_tree_3, newdata = train_data_3, type = "class")
predictions_test_tree_3 <- predict(storms_tree_3, newdata = test_data_3, type = "class")

conf_matrix_train_tree_3 <- confusionMatrix(predictions_train_tree_3, train_data_3$category)
conf_matrix_test_tree_3 <- confusionMatrix(predictions_test_tree_3, test_data_3$category)

accuracy_train_tree_3 <- conf_matrix_train_tree_3$overall["Accuracy"]
accuracy_test_tree_3 <- conf_matrix_test_tree_3$overall["Accuracy"]

# Checking the nodes of the tree 
nodes_3<-sum(storms_tree_3$frame$var == "<leaf>")

comp_tbl <- comp_tbl %>% rbind(list(nodes_3, accuracy_train_tree_3, accuracy_test_tree_3, 15, 2, 9))


# Tree 4
storms_tree_4 <- rpart(category ~ ., data = train_data_3, method = "class", minsplit = 5, maxdepth = 3, minbucket = 3)

predictions_train_tree_4 <- predict(storms_tree_4, newdata = train_data_3, type = "class")
predictions_test_tree_4 <- predict(storms_tree_4, newdata = test_data_3, type = "class")

conf_matrix_train_tree_4 <- confusionMatrix(predictions_train_tree_4, train_data_3$category)
conf_matrix_test_tree_4 <- confusionMatrix(predictions_test_tree_4, test_data_3$category)

accuracy_train_tree_4 <- conf_matrix_train_tree_4$overall["Accuracy"]
accuracy_test_tree_4 <- conf_matrix_test_tree_4$overall["Accuracy"]

# Checking the nodes of the tree 
nodes_4<-sum(storms_tree_4$frame$var == "<leaf>")

comp_tbl <- comp_tbl %>% rbind(list(nodes_4, accuracy_train_tree_4, accuracy_test_tree_4, 5, 3, 3))


# Tree 5
storms_tree_5 <- rpart(category ~ ., data = train_data_3, method = "class", minsplit = 10, maxdepth = 3, minbucket = 6)

predictions_train_tree_5 <- predict(storms_tree_5, newdata = train_data_3, type = "class")
predictions_test_tree_5 <- predict(storms_tree_5, newdata = test_data_3, type = "class")

conf_matrix_train_tree_5 <- confusionMatrix(predictions_train_tree_5, train_data_3$category)
conf_matrix_test_tree_5 <- confusionMatrix(predictions_test_tree_5, test_data_3$category)

accuracy_train_tree_5 <- conf_matrix_train_tree_5$overall["Accuracy"]
accuracy_test_tree_5 <- conf_matrix_test_tree_5$overall["Accuracy"]

# Checking the nodes of the tree 
nodes_5<-sum(storms_tree_5$frame$var == "<leaf>")

comp_tbl <- comp_tbl %>% rbind(list(nodes_5, accuracy_train_tree_5, accuracy_test_tree_5, 10, 3, 6))

# Tree 6
storms_tree_6 <- rpart(category ~ ., data = train_data_3, method = "class", minsplit = 15, maxdepth = 3, minbucket = 9)

predictions_train_tree_6 <- predict(storms_tree_6, newdata = train_data_3, type = "class")
predictions_test_tree_6 <- predict(storms_tree_6, newdata = test_data_3, type = "class")

conf_matrix_train_tree_6 <- confusionMatrix(predictions_train_tree_6, train_data_3$category)
conf_matrix_test_tree_6 <- confusionMatrix(predictions_test_tree_6, test_data_3$category)

accuracy_train_tree_6 <- conf_matrix_train_tree_6$overall["Accuracy"]
accuracy_test_tree_6 <- conf_matrix_test_tree_6$overall["Accuracy"]

# Checking the nodes of the tree 
nodes_6<-sum(storms_tree_6$frame$var == "<leaf>")

comp_tbl <- comp_tbl %>% rbind(list(nodes_6, accuracy_train_tree_6, accuracy_test_tree_6, 15, 3, 9))


# Tree 7
storms_tree_7 <- rpart(category ~ ., data = train_data_3, method = "class", minsplit = 30, maxdepth = 3, minbucket = 20)

predictions_train_tree_7 <- predict(storms_tree_7, newdata = train_data_3, type = "class")
predictions_test_tree_7 <- predict(storms_tree_7, newdata = test_data_3, type = "class")

conf_matrix_train_tree_7 <- confusionMatrix(predictions_train_tree_7, train_data_3$category)
conf_matrix_test_tree_7 <- confusionMatrix(predictions_test_tree_7, test_data_3$category)

accuracy_train_tree_7 <- conf_matrix_train_tree_7$overall["Accuracy"]
accuracy_test_tree_7 <- conf_matrix_test_tree_7$overall["Accuracy"]

# Checking the nodes of the tree 
nodes_7<-sum(storms_tree_7$frame$var == "<leaf>")

comp_tbl <- comp_tbl %>% rbind(list(nodes_7, accuracy_train_tree_7, accuracy_test_tree_7, 30, 3, 20))

# Tree 8
storms_tree_8 <- rpart(category ~ ., data = train_data_3, method = "class", minsplit = 40, maxdepth = 10, minbucket = 30)

predictions_train_tree_8 <- predict(storms_tree_8, newdata = train_data_3, type = "class")
predictions_test_tree_8 <- predict(storms_tree_8, newdata = test_data_3, type = "class")

conf_matrix_train_tree_8 <- confusionMatrix(predictions_train_tree_8, train_data_3$category)
conf_matrix_test_tree_8 <- confusionMatrix(predictions_test_tree_8, test_data_3$category)

accuracy_train_tree_8 <- conf_matrix_train_tree_8$overall["Accuracy"]
accuracy_test_tree_8 <- conf_matrix_test_tree_8$overall["Accuracy"]

# Checking the nodes of the tree 
nodes_8<-sum(storms_tree_8$frame$var == "<leaf>")

comp_tbl <- comp_tbl %>% rbind(list(nodes_8, accuracy_train_tree_8, accuracy_test_tree_8, 40, 10, 30))


# Tree 9
storms_tree_9 <- rpart(category ~ ., data = train_data_3, method = "class", minsplit = 60, maxdepth = 20, minbucket = 40)

predictions_train_tree_9 <- predict(storms_tree_9, newdata = train_data_3, type = "class")
predictions_test_tree_9 <- predict(storms_tree_9, newdata = test_data_3, type = "class")

conf_matrix_train_tree_9 <- confusionMatrix(predictions_train_tree_9, train_data_3$category)
conf_matrix_test_tree_9 <- confusionMatrix(predictions_test_tree_9, test_data_3$category)

accuracy_train_tree_9 <- conf_matrix_train_tree_9$overall["Accuracy"]
accuracy_test_tree_9 <- conf_matrix_test_tree_9$overall["Accuracy"]

# Checking the nodes of the tree 
nodes_9<-sum(storms_tree_9$frame$var == "<leaf>")

comp_tbl <- comp_tbl %>% rbind(list(nodes_9, accuracy_train_tree_9, accuracy_test_tree_9, 60, 20, 40))

# Tree 10
storms_tree_10 <- rpart(category ~ ., data = train_data_3, method = "class", minsplit = 200, maxdepth = 25, minbucket = 100)

predictions_train_tree_10 <- predict(storms_tree_10, newdata = train_data_3, type = "class")
predictions_test_tree_10 <- predict(storms_tree_10, newdata = test_data_3, type = "class")

conf_matrix_train_tree_10 <- confusionMatrix(predictions_train_tree_10, train_data_3$category)
conf_matrix_test_tree_10 <- confusionMatrix(predictions_test_tree_10, test_data_3$category)

accuracy_train_tree_10 <- conf_matrix_train_tree_10$overall["Accuracy"]
accuracy_test_tree_10 <- conf_matrix_test_tree_10$overall["Accuracy"]

# Checking the nodes of the tree 
nodes_10<-sum(storms_tree_10$frame$var == "<leaf>")

comp_tbl <- comp_tbl %>% rbind(list(nodes_10, accuracy_train_tree_10, accuracy_test_tree_10, 200, 25, 100))


# Tree 11
storms_tree_11 <- rpart(category ~ ., data = train_data_3, method = "class", minsplit = 300, maxdepth = 25, minbucket = 200)

predictions_train_tree_11 <- predict(storms_tree_11, newdata = train_data_3, type = "class")
predictions_test_tree_11 <- predict(storms_tree_11, newdata = test_data_3, type = "class")

conf_matrix_train_tree_11 <- confusionMatrix(predictions_train_tree_11, train_data_3$category)
conf_matrix_test_tree_11 <- confusionMatrix(predictions_test_tree_11, test_data_3$category)

accuracy_train_tree_11 <- conf_matrix_train_tree_11$overall["Accuracy"]
accuracy_test_tree_11 <- conf_matrix_test_tree_11$overall["Accuracy"]

# Checking the nodes of the tree 
nodes_11<-sum(storms_tree_11$frame$var == "<leaf>")

comp_tbl <- comp_tbl %>% rbind(list(nodes_11, accuracy_train_tree_11, accuracy_test_tree_11, 300, 25, 200))

# Tree 12
storms_tree_12 <- rpart(category ~ ., data = train_data_3, method = "class", 
                        minsplit = 500, maxdepth = 25, minbucket = 500)

predictions_train_tree_12 <- predict(storms_tree_12, newdata = train_data_3, type = "class")
predictions_test_tree_12 <- predict(storms_tree_12, newdata = test_data_3, type = "class")

conf_matrix_train_tree_12 <- confusionMatrix(predictions_train_tree_12, train_data_3$category)
conf_matrix_test_tree_12 <- confusionMatrix(predictions_test_tree_12, test_data_3$category)

accuracy_train_tree_12 <- conf_matrix_train_tree_12$overall["Accuracy"]
accuracy_test_tree_12 <- conf_matrix_test_tree_12$overall["Accuracy"]

# Checking the nodes of the tree 
nodes_12<-sum(storms_tree_12$frame$var == "<leaf>")

comp_tbl <- comp_tbl %>% rbind(list(nodes_12, accuracy_train_tree_12, accuracy_test_tree_12, 500, 25, 500))

# Final table
print(comp_tbl)

# Observing the accuracies with line graph

ggplot(comp_tbl, aes(x=Nodes)) + 
  geom_line(aes(y = TrainAccuracy), color = "red") + 
  geom_line(aes(y = TestAccuracy), color="blue") +
  ylab("Accuracy")


# Question 3 : 

# We will choose the tree number 8 as our final tree. 
# Parameters - Nodes = 5, Minsplit = 40, Maxdepth = 10 and Minbucket = 30

# Confusion matrix
conf_matrix_train_tree_8_table <- table(predictions_train_tree_8, train_data_3$category)
conf_matrix_test_tree_8_table <- table(predictions_test_tree_8, test_data_3$category)

# Printing the final results of matrix
print(conf_matrix_train_tree_8_table)
print(conf_matrix_test_tree_8_table)

# With the help of above results, we can clearly conclude that our model is performing well enough to predict all the classes with 0 miss classifications.

# Using same parameters to train a model with 10 fold cross validation.
train_control = trainControl(method = "cv", number = 10)

hypers = rpart.control(minsplit =  40, maxdepth = 10, minbucket = 30)
tree8_cv <- train(category ~ ., data = train_data_3, control = hypers, trControl = train_control, method = "rpart1SE")

# Report accuracy
tree8_cv$results$Accuracy

# With cross validation also, we can see our model is providing accuracy = 1.


# PROBLEM 4 :

# Load necessary libraries
library(rpart)

# Loading data
BankData <- read.csv("Bank_Modified.csv")

# Removing the ID column
BankData <- BankData[, !names(BankData) %in% "X"]
head(BankData)

# Converting the target variable 'approval' to a factor
BankData$approval <- as.factor(BankData$approval)

# Question 1 :

# Building the initial decision tree model with the help of hyperparameters

set.seed(567)  
split_index <- createDataPartition(BankData$approval, p = 0.8, list = FALSE)
train_data_1 <- BankData[split_index, ]
test_data_1 <- BankData[-split_index, ]

BankData_model <- rpart(approval ~ ., data = train_data_1, method = "class", minsplit = 10, maxdepth = 20)

predictions_1 <- predict(BankData_model, newdata = test_data_1, type = "class")

# Confusion matrix to evaluate accuracy
conf_matrix <- confusionMatrix(predictions_1, test_data_1$approval)
accuracy_1 <- conf_matrix$overall["Accuracy"]

# Print the accuracy
print(paste("Accuracy:", round(accuracy_1,4)))

# Number of leaf nodes
sum(BankData_model$frame$var == "<leaf>")

# Question 2 : 

# Running the variable importance analysis on the model
var_importance <- varImp(BankData_model)
print(var_importance)

# Question 3 :

# Plotting variable importance
barplot(var_importance$Overall, main = "Variable Importance Plot",
        col = "lightblue", cex.names = 0.7, las = 2, names.arg = rownames(var_importance))

# Question 4 :

# By looking at the graph, we can see top 6 variables with high importance are bool1, cont4, bool2, ages, cont3 and cont6.

# Rebuild the model with top six variables

BankData_model_new <- rpart(approval ~ bool1 + cont4 + bool2 + ages + cont5 + cont6, data = train_data_1, method = "class", minsplit = 10, maxdepth = 20)

predictions <- predict(BankData_model_new, newdata = test_data_1, type = "class")

# Confusion matrix to evaluate accuracy
conf_matrix_1 <- confusionMatrix(predictions, test_data_1$approval)
accuracy <- conf_matrix_1$overall["Accuracy"]

# Print the accuracy
print(paste("Accuracy initial:", round(accuracy_1,4)))
print(paste("Accuracy new:", round(accuracy,4)))


# Question 5:

# Visualize the initial tree
library(rpart.plot)
rpart.plot(BankData_model, shadow.col = "gray", nn = TRUE)

# Visualize the tree with top six variables
rpart.plot(BankData_model_new, shadow.col = "gray", nn = TRUE)

# By reducing the number of variabes, size of the tree increased.








