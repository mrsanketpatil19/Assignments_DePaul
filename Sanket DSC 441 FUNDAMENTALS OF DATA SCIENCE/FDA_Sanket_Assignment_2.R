# Problem 1

# Importing data in R

Data = read.csv("BankData.csv", header = T)

dim(Data)

Data <- na.omit(Data)
dim(Data)

# Question 1 :

# Applying Visualization techniques to create graphs..

# Checking class of all variables 
lapply(Data,class)

ls(Data)


# Separating all numeric variables into another dataframe
num_data <- select_if(Data, is.numeric)
ls(num_data)
# Verifying class of all numeric variables
lapply(num_data,class)

# Creating Scatter plot matrix for all numeric variables
pairs(~cont1+cont2+cont3+cont4+cont5+cont6+credit.score+ages,data = Data,main = "Scatterplot for all variable")

# By looking at the scatterplot matrix, we can say there is no linear relationship between any of the numeric variable.

library(ggplot2)
library(tidyverse)

# Creating Histogram..

hist(Data$credit.score,xlab = "Credit Score",col = "brown",border = "black")

Data %>% ggplot(aes(credit.score)) +
  geom_histogram(fill = "blue", bins = 15, color = "black", alpha = 0.5) +
  labs( x = "Credit Score", title = "Histogram of Credit Score") +
  theme_minimal()

# Used two methods to create histogram. By looking at the histogram, we can see that distribution is normal. We can see a perfect bell curve 

# Creating a Bar plot

ggplot(Data, aes(x = bool1, y = credit.score, fill = bool1)) +
  geom_bar(stat = "identity") +
  labs(x = "Bool1", y = "Credit Score", title = "Bar Plot: Bool1 vs Credit Score") +
  theme_minimal()

ggplot(Data, aes(x = bool1)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(x = "Bool1", y = "Count", title = "Bar Plot: Categorical Variable Bool1") +
  theme_minimal()
  
# By looking at the bar plot, we can see that the the category distribution is almost similar. There is no biased category.
# Also there are more number of records for True as compared to False.

# Creating Density plot 

ggplot(Data, aes(x = credit.score, fill = ages)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Credit score by ages", x = "credit score", fill = "Credit Score")

Data %>% ggplot(aes(x=credit.score, fill = approval)) + geom_histogram()

Data %>% ggplot(aes(x = ages, fill = approval)) + geom_density(alpha=0.5)


# Question 2 : 


# Z-score normalization
Normalized_cont1 <- scale(Data$cont1)   

# Z-score normalization
Normalized_cont1 = (Data$cont1 - mean(Data$cont1)) / sd(Data$cont1)

# Old Min max before Z-score normalization
max(Data$cont1)
min(Data$cont1)

# New min max values after Z-score normalization
max(Normalized_cont1)
min(Normalized_cont1)



# Min-max normalization

# Min-max values before normalization
max(Data$cont2)
min(Data$cont2)

Normalized_cont2 = (Data$cont2 - min(Data$cont2)) / (max(Data$cont2) - min(Data$cont2))

# Min-max values after normalization
max(Normalized_cont2)
min(Normalized_cont2)    

# Decimal scaling normalization

max(Data$cont3)
decimal_places<-2 

Normalized_cont3 <- Data$cont3 / 10^decimal_places

# Min-max values before normalization
min(Data$cont3)
max(Data$cont3)

# Min-max values after normalization
min(Normalized_cont3)
max(Normalized_cont3)

# Question 3:

# Z-score Normalization : 

hist(Data$cont1,xlab = "Credit Score",col = "brown",border = "black")

hist(Normalized_cont1,xlab = "Credit Score",col = "brown",border = "black")  

# After Z-score normalization for the variable count1, there is change in min and max value but there is no change in normality of the data as we can see histogram looks similar as it was previously.


# Decimal Normalization : 
# Histogram before normalization 
hist(Data$cont2,xlab = "Credit Score",col = "brown",border = "black")

# Histogram after normalization 
hist(Normalized_cont2,xlab = "Credit Score",col = "brown",border = "black")

# After min-max normalization for the variable count2, there is change in min and max value but there is no change in normality of the data as we can see histogram looks similar as it was previously.


# Decimal Scaling : 

# Histogram before normalization 
hist(Data$cont3,xlab = "Credit Score",col = "brown",border = "black")

# Histogram after normalization 
hist(Normalized_cont3,xlab = "Credit Score",col = "brown",border = "black")

# After Decimal scaling for the variable count3, there is change in min and max value but there is no change in normality of the data as we can see histogram looks similar as it was previously.


# Question 4 :

# Equal depth (quantile) binning

Data$credit.score_bins <- cut(Data$credit.score, breaks=quantile(Data$credit.score, probs=0:3/3), labels=c("Low", "Medium", "High"), include.lowest=TRUE)

head(Data)

# We have chosen variable credit.score. By looking at the distribution of the data, we decided to choose equal depth binning technique as data is normally distributed. 


# Question 5 :

# To calculate mean of each bin
mean_bins <- tapply(Data$credit.score, Data$credit.score_bins, mean)

# Replace original values with bin mean
Data$Smoothed_credit.score <- as.numeric(mean_bins[Data$credit.score_bins])

head(Data)

# Problem 2 : 

# Reading data in R
df = read.csv("BankData.csv", header = T)
head(df)

dim(df)

df <- na.omit(df)
dim(df)

#Load the required library
library(e1071)

unique(df$approval) 

# Replacing the values of target to 1 and 0.
df$approval <- ifelse(df$approval == "+", 1, 0)
head(df)


y <- as.factor(df$approval)
X <- within(df, rm(approval))

# Train the SVM model
svm_model <- svm(approval ~ ., data = df, kernel = "linear", cost = 1, cross = 10)

# Print the model details
print(svm_model)

# Access cross-validation results
summary <- summary(svm_model)
print(summary)

# As this is a regression model, accuracy might not be a most appropriate approach. Hence we evaluated Correlation coefficient and MSE.
# The squared correlation coefficient suggests a moderate amount of relationship between predicted and actual values, and the model seems to have a reasonably low mean squared error.


# Question b. 

#Use grid search to optimize the C parameter
# Define a grid of C values to search
c_values <- 10^(-2:2)

# Perform grid search with cross-validation
svm_tune <- tune(svm, approval ~ ., data = df, kernel = "linear", ranges = list(cost = c_values), tunecontrol = tune.control(sampling = "cross", cross = 10))

# Print the best parameter and its corresponding accuracy
C_parameter <- svm_tune$best.parameters$cost
accuracy <- svm_tune$best.performance

# Chosen C parameter for the model is 0.01.
# Best accuracy for the model is 0.1147595.

# Question c:

# The results are different even if the value of C=1. This is because of below mentioned reasons.
The folds can be different between the two runs, leading to slightly different training and validation sets.
For every run, SVM may change its local minima hence SVM might converge differently.
Also other hyperparameters can change differently for every iteration.
To resolve this issue, we can use set.seed function. This function will reproduce any operation which was performed with randomness.


# Problem 3 : 

# Install and load necessary libraries
library(dplyr)
library(e1071)
library(caret)

# Loading starwars dataset 

starwars <- dplyr::starwars
colnames(starwars)

starwars <- starwars %>% select(-films, -vehicles, -starships, -name)
colnames(starwars)

# Dimensions before removing rows with NA
dim(starwars)

# Removing rows with NA values
starwars <- na.omit(starwars)
# Dimensions after removing rows with NA
dim(starwars)

# Question a:

#Creating dummy variables except variable "gender":
starwars_dummies <- model.matrix(~. - gender, data = starwars)[, -1]

gender<-starwars$gender

# Merging dummy variables with gender variable
starwars_new <- cbind.data.frame(starwars_dummies, gender=gender)

colnames(starwars_new)
dim(starwars_new)

summary(starwars_new)
# Note that after merging gender with dummies data, all variables converted to class character. We need to convert those variables into numeric data.

# Head of new data after creating dummy variables except gender
head(starwars_new)

# Question b: 

# Replacing values from gender variable to 1 and
starwars_new$gender <- as.factor(starwars_new$gender)

# Split the data into training and testing 
set.seed(123)
train_indices <- sample(seq_len(nrow(starwars_new)), size = 0.80 * nrow(starwars_new))

# Create training and testing datasets
train_data <- starwars_new[train_indices, ]
test_data <- starwars_new[-train_indices, ]

# Applying svm model 

svm_model <- svm(gender ~ ., data = train_data)
svm_predictions <- predict(svm_model, test_data)

# Accuracy
accuracy <- sum(round(svm_predictions) == test_data$gender) / length(svm_predictions)
print(accuracy)

confusionMatrix(svm_predictions, test_data$gender)


# SVM has predicted all the values correctly hence we can say the accuracy is 100%.

# Question c: 

starwars_no_gender <- starwars_new %>% select(-gender)

starwars_no_gender <- starwars_no_gender %>% mutate_all(as.numeric)

# Perform PCA
pca_model <- prcomp(starwars_no_gender, center = TRUE, scale. = TRUE)


# Determine the number of principal components
# Plotting the PCA components on graph
plot(pca_model)
plot(pca_model$sdev^2/sum(pca_model$sdev^2), type="b", main="Scree Plot", xlab="Principal Component", ylab="Proportion of Variance")

# From the scree plot, you'd typically choose the number of components before the "elbow", or where the curve starts leveling off. Let's assume it's 'm' components.


variance_explained<-cumsum(pca_model$sdev^2)/sum(pca_model$sdev^2)

# Choose a threshold (e.g., 95% variance explained)
n_components <- sum(variance_explained >= 0.95)
cat("Number of components to retain:", n_components, "\n")

# Reduce data to the selected number of components
reduced_starwars_pca <- as.data.frame(pca_model$x[, 1:n_components])

reduced_starwars_pca$gender <- starwars_new$gender

summary(reduced_starwars_pca)

# -------------------------

index <- sample(1:nrow(reduced_starwars_pca), nrow(reduced_starwars_pca)*0.8)

# Train-test split on PCA data
set.seed(456)
train_data_pca <- reduced_starwars_pca[index, ]
test_data_pca <- reduced_starwars_pca[-index, ]

# Grid search on the C parameter
tune_result <- tune.svm(gender ~ ., data = train_data_pca, cost = 10^(-1:2))

# Best model
best_svm_model <- svm(gender ~ ., data = train_data_pca, cost = tune_result$best.parameters$cost)

# Predict
predictions_pca <- predict(best_svm_model, test_data_pca)

# confusion matrix
confusionMatrix(predictions_pca,test_data_pca$gender)

# Accuracy
accuracy <- mean(predictions_pca == test_data_pca$gender)
print(accuracy)

# Question e:

PCA has reduced the dimensionality of the dataset. Hence we are working on reduced number of principal components instead of working on all variables.
This will increase the computational efficiency of the model and model will work faster.
It will avoid overfitting.


# Question 4 :

# Install and load necessary libraries
install.packages("caret")
library(caret)

# Load Sacramento housing dataset
data(Sacramento)

# Removing zip and city variables as given
sacramento_data <- Sacramento[, !(names(Sacramento) %in% c("zip", "city"))]
colnames(sacramento_data)

# Question 1: 

# Part a: Explore variable distributions
summary(sacramento_data)
# Check for class imbalance in the 'type' variable
table(sacramento_data$type)

hist(sacramento_data$price,xlab = "Price",col = "maroon",border = "black")
hist(sacramento_data$sqft,xlab = "sqft",col = "maroon",border = "black")
hist(sacramento_data$latitude,xlab = "latitude",col = "maroon",border = "black")

# By looking at the histogram and summary, we can see some of the variables are skewed i.e. price and sqft but some have slightly normal destribution i.e. latitude.
# Also for variable type, we can see there is class imbalance as type=Residential as far more records than other two levels.

# We can check this class imbalance with the help of below bar graph.
ggplot(sacramento_data, aes(x = type)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(x = "Type", y = "Count", title = "Bar Plot: Categorical Variable Type") +
  theme_minimal()

# Question 2 :

# We will choose normalization to improve data
sacramento_data[, names(sacramento_data) %in% c("beds", "baths", "sqft", "price")] <- 
scale(sacramento_data[, names(sacramento_data) %in% c("beds", "baths", "sqft", "price")])

# Question 3 :

#Use SVM to predict 'type'

set.seed(123)
train_index_4 <- sample(seq_len(nrow(sacramento_data)), size = 0.80 * nrow(sacramento_data))
train_data_4 <- sacramento_data[train_index_4, ]
test_data_4 <- sacramento_data[-train_index_4, ]

# SVM with grid search
svm_model_4 <- train(type ~ ., data = train_data_4, method = "svmRadial", trControl = trainControl(method = "cv"))
svm_predictions_4 <- predict(svm_model_4, test_data_4)

# Accuracy and confusion matrix
conf_matrix <- confusionMatrix(svm_predictions_4, test_data_4$type)
accuracy <- conf_matrix$overall["Accuracy"]
kappa <- conf_matrix$overall["Kappa"]
print(paste("Accuracy:", accuracy))
print(paste("Kappa:", kappa))
print(conf_matrix)

# Question 4 : 
# Assuming outlier_threshold is a value defining the z-score threshold for outliers
outlier_threshold <- 3

# Function to remove outliers based on z-scores
remove_outliers <- function(x) {
  z_scores <- scale(x)
  abs_z_scores <- abs(z_scores)
  x[abs_z_scores < outlier_threshold, ]
}

# Apply the function to all numeric columns except "type"
sacramento_data <- sacramento_data[, -which(names(sacramento_data) == "type"), drop = FALSE]
sacramento_data <- lapply(sacramento_data, function(x) if(is.numeric(x)) remove_outliers(x) else x)
sacramento_data <- as.data.frame(sacramento_data
                                 

                                
                                 
                                 
                                 
                                 
                                 
# Problem 5 :

# Make a copy of mtcars
mtcars <- mtcars

# Initialize a new variable to hold fold indices
mtcars$folds = 0

# Create 5 folds, get a list of lists of indices
folds <- createFolds(1:nrow(mtcars), k = 5, list = TRUE)
print(folds)

# Set all the rows in a given fold to have that fold’s index in the folds variable
for (i in 1:5) {
  mtcars$folds[folds[[i]]] = i
}

# Load required libraries 
library(ggplot2)
library(dplyr)

# Visualize the distribution of the gears variable across folds
ggplot(mtcars, aes(x = factor(folds), fill = factor(gear))) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of Gear Variable Across Folds",
       x = "Fold Index",
       y = "Proportion") +
  scale_fill_discrete(name = "Gear")                                 
