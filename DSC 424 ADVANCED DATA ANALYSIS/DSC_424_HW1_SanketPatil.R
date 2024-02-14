
# Question 2 

# Define the matrices and vectors
Z <- matrix(c(1, 1, 1, 1,
              -1, 1, 0, 3), nrow=4, byrow=FALSE)

Y <- matrix(c(0, 8, 0, 6), nrow=4, byrow=TRUE)

M <- matrix(c(2, 11, 0,
              1, 3, 40,
              4, 28, 73), nrow=3, byrow=FALSE)

N <- matrix(c(-4, 7, 9,
              -3, 2, 7,
              0, 1, -8), nrow=3, byrow=FALSE)

v <- matrix(c(-3, 39, 15), nrow=3)

w <- matrix(c(0, 10, 29), nrow=3)

# a. v · w (dot product)
Question_a <- sum(v * w)
Question_a

# b. -3 * w
Question_b <- -3 * w
Question_b

# c. M * v 
Question_c <- M %*% v
Question_c

# d. M + N 
Question_d <- M + N
Question_d

# e. M - N 
Question_e <- M - N
Question_e

# f. Z'Z 
Question_f <- t(Z) %*% Z
Question_f

# g. (Z'Z)^-1 (inverse of Z'Z)
Question_g <- solve(Question_f)
Question_g

# h. Z'Y (matrix multiplication of Z transpose and Y)
Question_h <- t(Z) %*% Y
Question_h

# i. β = (Z'Z)^-1 Z'Y
Question_i <- Question_g %*% Question_h
Question_i

# j. det(Z'Z) (determinant of Z'Z)
Question_j <- det(Question_f)
Question_j




# Question 5 - Indian housing data

# Load necessary libraries
library(tidyverse)
library(dplyr)
library(modeest)  # for the mfv function to find the mode
library(caret)    # For correlation
library(fastDummies)

# Read the CSV file
df <- read.csv("D:/Assignments_Depaul/DSC_424_Advance_Data_Analysis/HW1/indian_housing_data.csv", header = TRUE)
View(df)

# Display basic information about the dataset
str(df) 

# Display summary statistics
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

# Checking the columns with unique counts in the dataframe
unique_counts <- sapply(df, function(x) length(unique(x)))

# Display the number of unique values for each column
print(unique_counts)

# Delete column with Unique values
df <- df[, !colnames(df) %in% "URLs"]

# Delete column with dates
df <- df[, !colnames(df) %in% "postedOn"]

# Remove locality as it will not help and will cause issue in creating dummy variables
df <- df[, !colnames(df) %in% "locality"]

# Checking the % of rows with value = 9. We are treating them as NA as given in the problem statement
percentage_rows_exact_Value_9 <- colMeans(df == 9, na.rm = TRUE) * 100
print(percentage_rows_exact_Value_9)


# Find columns where percentage is greater than 60%
columns_greater_than_70_percent <- names(percentage_rows_exact_Value_9[percentage_rows_exact_Value_9 > 70])

# Print or use the column names as needed
cat("Columns where more than 70% of values are 9:\n")
print(columns_greater_than_70_percent)

# Remove columns from df
df <- df[, !(names(df) %in% columns_greater_than_70_percent)]

dim(df)

# Check for NA values column wise
na_percentages <- colMeans(is.na(df)) * 100
na_percentages

# Calculate the percentage of rows affected by NA
percentage_na_rows <- mean(apply(df, 1, function(row) any(is.na(row)))) * 100
print(percentage_na_rows)

summary(df)

sapply(df, function(x) length(unique(x)))

# -------------------  Handeling carpet area problem as it has different units -------------

unique(df$carpetAreaUnit)

convert_to_sqft_and_replace <- function(df) {
  convert <- tibble(
    unit = c("Sq_ft", "Kanal", "Marla", "Sq_yrd", "Biswa1", "Sq_m", "Rood", "Biswa2", "Acre"),
    factor = c(1, 5445, 272.25, 9, 1350, 10.764, 10890, 2700, 43560)
  )
  
  df$carpetArea <- mapply(function(area, unit) {
    if (unit %in% names(convert) && unit != "Sq-ft" && unit != "9") {
      return(area * convert[unit])
    } else {
      return(area)
    }
  }, df$carpetArea, df$carpetAreaUnit)
  
  # Return the modified data frame
  return(df)
}

# Converting the other units to sqft
df <- convert_to_sqft_and_replace(df)

# Now we don't need carpetAreaUnit column as we have converted all values to sq-ft. 
# Delete column carpetAreaUnit
df <- df[, !colnames(df) %in% "carpetAreaUnit"]

# We have a column called flrNum which has some categorical values. We will convert them to numeric
unique(df$flrNum)

# Assigning Ground as 0, Lower basement as -2 and Upper Basement as -1
df <- df %>% 
  mutate(flrNum = case_when(
    flrNum == "Ground" ~ 0,
    flrNum == "Upper Basement" ~ -1,
    flrNum == "Lower Basement" ~ -2,
    TRUE ~ as.numeric(flrNum) 
  ))

# Converting whole column to numeric
df$flrNum <- as.numeric(df$flrNum)

# replacing flrNum=9 by 4 as 4 is mean of flrNum column
df <- df %>% 
  mutate(flrNum = case_when(
    flrNum == 9 ~ 4, TRUE ~ as.numeric(flrNum)))

sapply(df, function(x) length(unique(x)))

# Converting some of the columns as factors as they will not provide any useful information with numeric as they have less number if unique values.
# Hence we can treat those columns as factors to find non linear relations between them
df$bedrooms <- as.factor(df$bedrooms)
df$bathrooms <- as.factor(df$bathrooms)
df$balconies <- as.factor(df$balconies)

#  -------------------- Treating value = 9 as NA which is provided in the problem statement and replacing them -------------

# Checking % of value=9 column wise
colMeans(df == 9, na.rm = TRUE) * 100

replace_9_with_mean_or_mode <- function(df) {
  for (col in names(df)) {

    is_numeric <- is.numeric(df[[col]])
    is_character <- is.character(df[[col]])
    is_factor <- is.factor(df[[col]])
    
    # Replace 9 with mean for numeric columns
    if (is_numeric) {
      df[[col]][df[[col]] == 9] <- mean(df[[col]][df[[col]] != 9], na.rm = TRUE)
    }
    # Replace 9 with mode for character and factor columns
    else if (is_character || is_factor) {
      mode_val <- as.character(names(sort(table(df[[col]][df[[col]] != 9]), decreasing = TRUE)[1]))
      df[[col]][df[[col]] == 9] <- mode_val
    }
  }
  
  return(df)
}

# Replace the values now
df <- replace_9_with_mean_or_mode(df)

# Checking if the values are replaced or not
colMeans(df == 9, na.rm = TRUE) * 100

# Check for NA values generated during preprocessing
na_percentages <- colMeans(is.na(df)) * 100
na_percentages

# Calculate the percentage of rows affected by NA
percentage_na_rows <- mean(apply(df, 1, function(row) any(is.na(row)))) * 100
print(percentage_na_rows)


# ---------------------------------- Target variable Analysis ---------------------------------

# Plotting histogram of target variable

hist(df$exactPrice, main = "Histogram of exactPrice", xlab = "exactPrice", col = "skyblue", border = "black")

exactPrice_skewness <- skewness(df$exactPrice)

cat("Skewness of exactPrice:", exactPrice_skewness, "\n")

# Target variable is highly skewed hence we need handle this.

# Finding outliers

# Calculate Z-scores
z_scores <- scale(df$exactPrice)

# Set a threshold (e.g., 3 or -3)
threshold <- 3

# Identify outliers
outliers <- which(abs(z_scores) > threshold)

# Print the indices of outliers
cat("Indices of outliers in exactPrice:", outliers, "\n")

# Print the values of outliers
cat("Values of outliers in exactPrice:", df$exactPrice[outliers], "\n")

# Remove rows with outliers
df <- df[-outliers, ]

# Print information about removed rows
cat("Number of rows removed:", length(outliers), "\n")

hist(df$exactPrice, main = "Histogram of exactPrice", xlab = "exactPrice", col = "skyblue", border = "black")

skewness(df$exactPrice)
# Still data is highly skewed after removing outliers.


# We will use log scaling to scale the data in target variable
df$exactPrice <- log(df$exactPrice)

skewness(df$exactPrice)
# Now the skewness is under range of -1 to 1. 

# Plotting the histogram of the data
hist(df$exactPrice, main = "Histogram of exactPrice", xlab = "exactPrice", col = "skyblue", border = "black")

# -----------------------------------  Checking Correlation ----------------------------------

# Find numeric columns in predictors
numeric_columns <- sapply(df, is.numeric)

# Create a data frame with only numeric columns
numeric_predictors <- df[, numeric_columns]

# Check for correlations between predictors
cor_matrix <- cor(numeric_predictors)
highly_correlated_vars <- findCorrelation(cor_matrix, cutoff = 0.8)  
highly_correlated_vars

# Creating a heatmap
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", tl.cex = 0.7, 
         addCoef.col = "black", number.cex = 0.7, number.digits = 2)

# By checking correlation values, we can see there is not a single variable which is highly correlated with target variale.

# We will fit a temporary model only on numeric data to check if there is multicollinearity
model_temp <- lm(exactPrice ~ ., data = numeric_predictors)
summary(model_temp)
# We can see we got very low adjusted R-Squared with this numeric data. Now we will check vif values for these numeric columns.

# Calculate VIF
vif_values <- car::vif(model_temp)

# Display VIF values
print(model_temp)

# We can see all the variables have vif values under 10. Hence we can say there is no multicollinearity in the data.

# ---------------------------------------- EDA ---------------------------------------------

colnames(df)

library(ggplot2)

# Here in the below mentioned plot, we can clearly see that as number of bedrooms increases, the price increases. We were able to say this on the basis of median of the boxplots as median increases as we increase the number of bedrooms. 
ggplot(df, aes(x = as.factor(bedrooms), y = exactPrice)) +
  geom_boxplot() +
  labs(title = "Boxplot of exactPrice by Bedrooms",
       x = "Number of Bedrooms",
       y = "exactPrice") +
  theme_minimal()

# Same analysis goes for number of bathrooms, as we increase number of bathrooms, price of the house increases.
ggplot(df, aes(x = as.factor(bathrooms), y = exactPrice)) +
  geom_boxplot() +
  labs(title = "Boxplot of exactPrice by bathrooms",
       x = "Number of bathrooms",
       y = "exactPrice") +
  theme_minimal()

# We can see here that prices range is high with number of balconies between 3 to 8. Price is low for number of balconies = 10 which is strange.
ggplot(df, aes(x = as.factor(balconies), y = exactPrice)) +
  geom_boxplot() +
  labs(title = "Boxplot of exactPrice by balconies",
       x = "Number of balconies",
       y = "exactPrice") +
  theme_minimal()

# If a house is on sale, price is large as compared to house on rent which is obvious
ggplot(df, aes(x = as.factor(RentOrSale), y = exactPrice)) +
  geom_boxplot() +
  labs(title = "Boxplot of exactPrice by RentOrSale",
       x = "Number of RentOrSale",
       y = "exactPrice") +
  theme_minimal()

# If a house is Multistory, Penthouse and Villa, the price is high so these variables must be significant in predicting the price.
ggplot(df, aes(x = as.factor(propertyType), y = exactPrice)) +
  geom_boxplot() +
  labs(title = "Boxplot of exactPrice by propertyType",
       x = "Number of propertyType",
       y = "exactPrice") +
  theme_minimal()

# There are outliers present in the carpet are variable hence we are not able to capture any relation 
ggplot(df, aes(x = carpetArea, y = exactPrice)) +
  geom_point() +
  labs(title = "Scatterplot of exactPrice by Carpet Area",
       x = "Carpet Area",
       y = "exactPrice") +
  theme_minimal()

# We can see there is slight positive correlation between these two variables
ggplot(df, aes(x = securityDeposit, y = exactPrice)) +
  geom_point() +
  labs(title = "Scatterplot of exactPrice by securityDeposit",
       x = "securityDeposit",
       y = "exactPrice") +
  theme_minimal()

# We can see positive correlation between Sqft price and exact price
ggplot(df, aes(x = sqftPrice, y = exactPrice)) +  geom_point() +
  labs(title = "Scatterplot of exactPrice by sqftPrice",
       x = "sqftPrice",
       y = "exactPriceś") +
  theme_minimal()



# ------------------------------------ Combining Data ---------------------------------------

# Creating dummy variables

df_combined_dummies <- df %>% model.matrix(~ . - 1, data = .) %>%  as.data.frame()
dim(df_combined_dummies)

colnames(df)
dim(df)

sapply(df, function(x) length(unique(x)))

# ----------------------------------- Splitting Data -------------------------------------------------

# Creating a train/test partition
set.seed(123) 
splitIndex <- createDataPartition(df_combined_dummies$exactPrice, p = 0.8, list = FALSE)
df_train <- df_combined_dummies[splitIndex, ]
df_test <- df_combined_dummies[-splitIndex, ]

dim(df_train)
dim(df_test)

# Apply linear regression
Initial_model <- lm(exactPrice ~ ., data=df_train)
summary(Initial_model)
# We can see the Adjusted R-squared value is 0.9521. Also there are many insignificant variables which we can remove further with step function.

# Making predictions on test data
predictions <- predict(Initial_model, newdata = df_test)

length(predictions)

# Calculate Mean Squared Error (MSE)
mse_initial <- mean((df_test$exactPrice - predictions)^2)
cat("Mean Squared Error (MSE):", mse_initial, "\n")

# Calculate Mean Absolute Error (MAE)
mae_initial <- mean(abs(df_test$exactPrice - predictions))
cat("Mean Absolute Error (MAE):", mae_initial, "\n")

# We can see we got vary low MSE and MAE values. 

# Calculate residuals
residuals <- df_test$exactPrice - predictions

# Residual Plot
plot(predictions, residuals, 
     xlab = "Predicted Values", ylab = "Residuals",
     main = "Residual Plot for Test Data Predictions",
     pch = 16, col = "blue")
abline(h = 0, col = "red", lty = 2)

# We will perform backward elimination method to select significant variables. Commented this line of code as it takes time to run the code.
# backward_elimination <- step(Initial_model, direction = "backward")

# Display the summary of the backward model
summary(backward_elimination)

# Creating the model based on the variables selected by backward elimination variable selection method.
backward_model <- lm(exactPrice ~ sqftPrice + securityDeposit + `propertyTypeBuilder Floor Apartment` + 
                       `propertyTypeMultistorey Apartment` + `propertyTypeResidential House` + 
                       `propertyTypeStudio Apartment` + `furnishingSemi-Furnished` + 
                       furnishingUnfurnished + flrNum + facingNorth + `facingNorth - West` + 
                       facingSouth + `facingSouth - East` + `facingSouth -West` + 
                       facingWest + totalFlrNum + cityAgartala + cityBangalore + 
                       cityBhopal + cityChandigarh + cityChennai + cityDehradun + 
                       cityGandhinagar + cityGangtok + cityGoa + cityHyderabad + 
                       cityJaipur + cityKolkata + cityLucknow + cityMumbai + `cityNew-Delhi` + 
                       `cityNew Delhi` + cityPatna + cityRaipur + carpetArea + bedrooms2 + 
                       bedrooms3 + bedrooms4 + bedrooms5 + bedrooms6 + bedrooms7 + 
                       bedrooms8 + bedrooms10 + bathrooms2 + bathrooms3 + bathrooms4 + 
                       bathrooms5 + bathrooms6 + bathrooms7 + bathrooms8 + bathrooms10 + 
                       balconies2 + balconies3 + balconies4 + balconies6 + balconies7 + 
                       balconies8 + balconies10 + RentOrSaleSale + Long, data = df_train)

summary(backward_model)

# Making predictions on test data
predictions_test <- predict(backward_model, newdata = df_test)

# Calculate MSE
mse_backward <- mean((df_test$exactPrice - predictions_test)^2)
cat("Mean Squared Error (MSE):", mse_backward, "\n")

# Calculate MAE
mae_backward <- mean(abs(df_test$exactPrice - predictions_test))
cat("Mean Absolute Error (MAE):", mae_backward, "\n")

# Calculate residuals
residuals_backward <- df_test$exactPrice - predictions_test

# Residual Plot
plot(predictions_test, residuals_backward, 
     xlab = "Predicted Values", ylab = "Residuals",
     main = "Residual Plot for Test Data Predictions (Backward Selection)",
     pch = 16, col = "blue")
abline(h = 0, col = "red", lty = 2)

# QQ Plot
qqnorm(residuals_backward, main = "QQ Plot for Test Data Predictions (Backward Selection)", col = "blue")
qqline(residuals_backward, col = "red")

# We can see that variation of residuals with respect to predicted values is constant. Hence we can say the model is good. Also many points are following the line in the QQ plot.

# We can see all the significant variables are selected by backward elimination method. All the variables have p value less than 0.05 except some of the balconies variables.

# the large F-statistic and the very small p-value indicate that the regression model as a whole is highly significant, suggesting that the set of independent variables jointly have a significant effect on the dependent variable.

# The method chosen for variable selection is backward elimination. This method iteratively removes insignificant variables from the model until all remaining variables are statistically significant. Backward elimination starts with a full model including all potential predictors and progressively removes variables based on their p-values until all remaining variables have p-values below a chosen threshold.

# We have selected backward elimination was employed to refine the initial model obtained through linear regression. By systematically removing variables with high p-values, the resulting model aims to improve interpretability, reduce overfitting, and enhance predictive accuracy by focusing on the most relevant predictors.

# The overall significance of the regression fit can be assessed based on several metrics:
  
# Adjusted R-squared: The adjusted R-squared value indicates the proportion of variance in the response variable that is explained by the model, adjusted for the number of predictors. In this case, the adjusted R-squared value is 0.9521, indicating that approximately 95.21% of the variance in the exactPrice variable is explained by the selected predictors.

# Significance of coefficients: The coefficients associated with each predictor variable provide insight into their impact on the response variable. In the summary output provided, most coefficients have extremely low p-values (indicated by '***'), suggesting that the corresponding predictors are statistically significant in predicting the exactPrice.



