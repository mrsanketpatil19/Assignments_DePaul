# Load necessary libraries
library(tidyverse)
library(dplyr)
library(modeest)  # for the mfv function to find the mode
library(caret)    # For correlation
library(fastDummies)

# Importing data in R

df = read.csv("Property_Price_Train.csv", header = T)
dim(df)
head(df)

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

'''
# Finding NA% columnwise
na_percentages <- colMeans(is.na(df)) * 100

columns_with_high_na <- names(na_percentages[na_percentages > 60])

# Calculate the percentage of rows with NA
percentage_na_rows <- mean(apply(df, 1, function(row) any(is.na(row)))) * 100

# Removing columns which have high NA %.
df <- df[, !(names(df) %in% columns_with_high_na)]

# Calculate the percentage of rows with NA
percentage_na_rows <- mean(apply(df, 1, function(row) any(is.na(row)))) * 100

na_percentages <- colMeans(is.na(df)) * 100

columns_with_na <- names(na_percentages[na_percentages > 0])
columns_with_na

# Create a new dataframe Data_NA with selected columns
Data_NA <- df[, columns_with_na, drop = FALSE]

# Pulling out names of all numeric and categorical variables
numerical_columns <- sapply(Data_NA, is.numeric)
categorical_columns <- sapply(Data_NA, function(x) is.factor(x) | is.character(x))

# Create new dataframes for numerical and categorical columns
Numerical_Data_NA <- Data_NA[, numerical_columns, drop = FALSE]
Categorical_Data_NA <- Data_NA[, categorical_columns, drop = FALSE]


# Replace NA values with mean for all columns
Numerical_Data_NA <- Numerical_Data_NA %>% mutate_all(~ ifelse(is.na(.), mean(., na.rm = TRUE), .))

# Checking NA% now
colMeans(is.na(Numerical_Data_NA)) * 100


# Function to find the mode
find_mode <- function(x) {
  mode_result <- mfv(x,na_rm = TRUE)
  if (length(mode_result) > 0) {
    return(mode_result)
  } else {
    # If there is no mode, return NA
    return(NA)
  }
}

# Checking what mfv function does
mfv(Categorical_Data_NA$FireplaceQu, na_rm = TRUE)

# Checking if find_mode function working properly
sapply(Categorical_Data_NA, find_mode)

# Replace NA values with mode for all columns
Categorical_Data_NA <- Categorical_Data_NA %>% mutate_all(~ ifelse(is.na(.), find_mode(.), .))

# Checking NA% now
colMeans(is.na(Categorical_Data_NA)) * 100
'''

#--------------------------------------------START Preprocessing------------------------------------------

df <- df[, !colnames(df) %in% "Id"]


# Dealing wih columns with dates
#1. To get the house life, we will substract the construction year column from current year.
#2. Also we will remove other variables with dates as it does not any value in ML models.

df$House_Life <- 2023 - df$Construction_Year

# Removing other date variables

df <- subset(df, select = -c(Construction_Year, Remodel_Year, Garage_Built_Year, Month_Sold, Year_Sold))
dim(df)


# In the data, there are lot of columns which has NA values but which are not actual NaN values. These values shold be something else.
#Lane_Type = No_Allay_Access
#Basement_Height = No_Basement
#Basement_Condition = No_Basement
#Exposure_Level = No_Basement
#BsmtFinType1 = No_Basement
#BsmtFinType2 = No_Basement
#Fireplace_Quality = No_Fireplace
#Garage = No_Garage
#Garage_Finish_Year = No_Garage
#Garage_Qualit = No_Garage
#Garage_Condition = No_Garage
#Pool_Quality = No_Pool
#Fence_Quality = No_Fence
#Fence_Quality = None

df$Lane_Type <- ifelse(is.na(df$Lane_Type), "No_Allay_Access", df$Lane_Type)
df$Basement_Height <- ifelse(is.na(df$Basement_Height), "No_Basement", df$Basement_Height)
df$Basement_Condition <- ifelse(is.na(df$Basement_Condition), "No_Basement", df$Basement_Condition)
df$Exposure_Level <- ifelse(is.na(df$Exposure_Level), "No_Basement", df$Exposure_Level)
df$BsmtFinType1 <- ifelse(is.na(df$BsmtFinType1), "No_Basement", df$BsmtFinType1)
df$BsmtFinType2 <- ifelse(is.na(df$BsmtFinType2), "No_Basement", df$BsmtFinType2)
df$Fireplace_Quality <- ifelse(is.na(df$Fireplace_Quality), "No_Fireplace", df$Fireplace_Quality)
df$Garage <- ifelse(is.na(df$Garage), "No_Garage", df$Garage)
df$Garage_Finish_Year <- ifelse(is.na(df$Garage_Finish_Year), "No_Garage", df$Garage_Finish_Year)
df$Garage_Qualit <- ifelse(is.na(df$Garage_Qualit), "No_Garage", df$Garage_Qualit)
df$Garage_Condition <- ifelse(is.na(df$Garage_Condition), "No_Garage", df$Garage_Condition)
df$Pool_Quality <- ifelse(is.na(df$Pool_Quality), "No_Pool", df$Pool_Quality)
df$Fence_Quality <- ifelse(is.na(df$Fence_Quality), "No_Fence", df$Fence_Quality)

na_percentages <- colMeans(is.na(df)) * 100
na_percentages

# Removing column Miscellaneous_Feature as it has NA values greater than 75%
df <- df[, !names(df) %in% "Miscellaneous_Feature"]
head(df)
dim(df)

# Calculate the percentage of rows with NA
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

# Calculate the percentage of rows with NA after imputaion
percentage_na_rows_1 <- mean(apply(df, 1, function(row) any(is.na(row)))) * 100
print(percentage_na_rows_1)


class(df$Garage_Size)


# Converting variables to factors

plot(df$House_Condition, df$Sale_Price, 
     xlab = "House Condition", ylab = "Sale Price",
     main = "Scatter Plot: House Condition vs Sale Price")

plot(df$Building_Class, df$Sale_Price, 
     main = "Scatter Plot between Building_Class and Sale_Price",
     xlab = "Building_Class",
     ylab = "Sale_Price",
)

# Ordinal variables have a meaningful order but the intervals between values are not necessarily uniform.
# Converting variables to factors allows statistical models to capture potential nonlinear relationships. For instance, the effect of going from 1 to 2 bedrooms may not be the same as going from 3 to 4 bedrooms.
# If the values in House_Condition represent categories such as "Excellent," "Good," "Average," etc., and these categories have no inherent numeric meaning (e.g., an "Excellent" condition doesn't mean it's twice as good as "Good"), then treating it as a factor is appropriate.
# If the categories in House_Condition are not inherently ordered numerically (e.g., "Good" is not necessarily twice as good as "Average"), treating them as numeric could lead to incorrect interpretations.

df$Garage_Size <- as.factor(df$Garage_Size)
df$Fireplaces <- as.factor(df$Fireplaces)
df$Rooms_Above_Grade <- as.factor(df$Rooms_Above_Grade)
df$Kitchen_Above_Grade <- as.factor(df$Kitchen_Above_Grade)
df$Bedroom_Above_Grade <- as.factor(df$Bedroom_Above_Grade)
df$Half_Bathroom_Above_Grade <- as.factor(df$Half_Bathroom_Above_Grade)
df$Full_Bathroom_Above_Grade <- as.factor(df$Full_Bathroom_Above_Grade)
df$Underground_Half_Bathroom <- as.factor(df$Underground_Half_Bathroom)
df$Underground_Full_Bathroom <- as.factor(df$Underground_Full_Bathroom)
df$House_Condition <- as.factor(df$House_Condition)
df$Overall_Material <- as.factor(df$Overall_Material)
df$Building_Class <- as.factor(df$Building_Class)


df$Garage_Area[df$Garage_Area<0]

# Checking if any of the data have negative values
numeric_cols <- sapply(df, is.numeric)
neg_values <- sapply(df[, sapply(df, is.numeric)], function(x) any(x < 0))

# Need to replace these negative values for Area columns with 0 as area cannot be negative

cols_to_replace <- c("Garage_Area", "W_Deck_Area", "Open_Lobby_Area", "Enclosed_Lobby_Area")

df[, cols_to_replace] <- lapply(df[, cols_to_replace], function(x) ifelse(x < 0, 0, x))

# Checking if data has Biased columns

checkDominantClass <- function(data) {
  # Get column names with class factor or character type
  class_columns <- sapply(data, function(x) is.factor(x) | is.character(x))
  
  for (col in names(data)[class_columns]) {
    # Check if the dominant class contributes more than 95%
    class_counts <- table(data[[col]])
    dominant_class <- names(class_counts)[which.max(class_counts)]
    dominant_class_percentage <- max(class_counts) / sum(class_counts)
    
    if (dominant_class_percentage > 0.95) {
      cat(sprintf("Variable '%s' has a dominant class: %s, which contributes %.2f%% of the total.\n", col, dominant_class, dominant_class_percentage * 100))
    }
  }
}
checkDominantClass(df)


# Removing variables which has dominant class > 95%

df <- subset(df, select = -c(Road_Type, Utility_Type, Condition2, Roof_Quality, Heating_Type,Pool_Quality))
dim(df)

# --------------------------- Target Variable Analysis

# Plotting histogram of target variable

hist(df$Sale_Price, main = "Histogram of Sale_Price", xlab = "Sale_Price", col = "skyblue", border = "black")

sale_price_skewness <- skewness(df$Sale_Price)

cat("Skewness of Sale_Price:", sale_price_skewness, "\n")

# Finding outliers

# Calculate Z-scores
z_scores <- scale(df$Sale_Price)

# Set a threshold (e.g., 3 or -3)
threshold <- 3

# Identify outliers
outliers <- which(abs(z_scores) > threshold)

# Print the indices of outliers
cat("Indices of outliers in Sale_Price:", outliers, "\n")

# Print the values of outliers
cat("Values of outliers in Sale_Price:", df$Sale_Price[outliers], "\n")


# Remove rows with outliers
df <- df[-outliers, ]

# Print information about removed rows
cat("Number of rows removed:", length(outliers), "\n")

hist(df$Sale_Price, main = "Histogram of Sale_Price", xlab = "Sale_Price", col = "skyblue", border = "black")

skewness(df$Sale_Price)

# Identify numeric and categorical columns
numeric_cols <- sapply(df, is.numeric)
categorical_cols <- sapply(df, function(x) is.factor(x) | is.character(x))

# Create df_numeric and df_categorical
df_numeric <- df[, numeric_cols]
df_categorical <- df[, categorical_cols]

# Checking skewness of all numeric variables

apply(df_numeric, 2, skewness)[apply(df_numeric, 2, skewness)>2]
cols_with_high_skewness <- names(apply(df_numeric, 2, skewness)[apply(df_numeric, 2, skewness)>2])

# Perform log transformation for selected columns
df_numeric[, cols_with_high_skewness] <- log1p(df_numeric[, cols_with_high_skewness])

# Print the transformed dataframe
apply(df_numeric, 2, skewness)


##------------------------------------------ EDA ---------------------------------------------------

dim(df_categorical)
dim(df_numeric)

colnames(df_categorical)

plot(df$Sale_Price, df$House_Life, 
     xlab = "Sale Price", ylab = "House Life",
     main = "Scatter Plot: Sale Price vs House Life")
# Slight negative linear relation 

plot(df$Sale_Price, df$Total_Basement_Area, 
     xlab = "Sale Price", ylab = "Total_Basement_Area",
     main = "Scatter Plot: Sale Price vs Total_Basement_Area")
# Positive linear relation

plot(df$Sale_Price, df$First_Floor_Area, 
     xlab = "Sale Price", ylab = "First_Floor_Area",
     main = "Scatter Plot: Sale Price vs First_Floor_Area")
# Positive linear relation

plot(df$Sale_Price, df$Second_Floor_Area, 
     xlab = "Sale Price", ylab = "Second_Floor_Area",
     main = "Scatter Plot: Sale Price vs Second_Floor_Area")

boxplot(df_numeric$Sale_Price ~ df_categorical$Overall_Material,
        main = "Sale Price vs. Overall Material",
        xlab = "Overall Material",
        ylab = "Sale Price",
        col = "lightblue",  # Boxplot fill color
        border = "black"    # Boxplot border color
)

boxplot(df_numeric$Sale_Price ~ df$House_Condition,
        main = "Sale Price vs. House_Condition",
        xlab = "House_Condition",
        ylab = "Sale Price",
        col = "lightblue",  # Boxplot fill color
        border = "black"    # Boxplot border color
)

boxplot(df_numeric$Sale_Price ~ df$Neighborhood,
        main = "Sale Price vs. Neighborhood",
        xlab = "Neighborhood",
        ylab = "Sale Price",
        col = "lightblue",  # Boxplot fill color
        border = "black"    # Boxplot border color
)


boxplot(df_numeric$Sale_Price ~ df$Condition1,
        main = "Sale Price vs. Condition1",
        xlab = "Condition1",
        ylab = "Sale Price",
        col = "lightblue",  # Boxplot fill color
        border = "black"    # Boxplot border color
)

boxplot(df_numeric$Sale_Price ~ df$Sale_Condition,
        main = "Sale Price vs. Sale_Condition",
        xlab = "Sale_Condition",
        ylab = "Sale Price",
        col = "lightblue",  # Boxplot fill color
        border = "black"    # Boxplot border color
)


## --------------------------------- Correlation  ---------------------------------------------

# Checking correlation 
correlation_matrix <- cor(df_numeric)
view(correlation_matrix)

# Remove highly correlated columns
df_numeric <- df_numeric[, -highly_correlated_cols]

# Find columns with correlation greater than 0.8 with Sale_Price
high_correlation_columns <- names(which(correlation_matrix["Sale_Price", ] > 0.8))

# Print or use high_correlation_columns as needed
print(high_correlation_columns)


# ----------------------------------- Combining Data ----------------------------------

# Creating dummy variables

df_categorical <- as.data.frame(lapply(df_categorical, as.factor))

New_df <- cbind(df_numeric, df_categorical)

df_combined_dummies <- New_df %>% model.matrix(~ . - 1, data = .) %>%  as.data.frame()
dim(df_combined_dummies)

# ----------------------------------- Splitting Data -------------------------------------------------

# Creating a train/test partition
set.seed(123) 
splitIndex <- createDataPartition(df_combined_dummies$Sale_Price, p = 0.8, list = FALSE)
df_train <- df_combined_dummies[splitIndex, ]
df_test <- df_combined_dummies[-splitIndex, ]

dim(df_train)
dim(df_test)

 
# Apply linear regression
Initial_model <- lm(Sale_Price ~ ., data=df_train)


# Making predictions on test data
predictions <- predict(Initial_model, newdata = df_test)
dim(df_test)

summary(Initial_model)

# Calculate Mean Squared Error (MSE)
mse_initial <- mean((df_test$Sale_Price - predictions)^2)
cat("Mean Squared Error (MSE):", mse_initial, "\n")

# Calculate Mean Absolute Error (MAE)
mae_initial <- mean(abs(df_test$Sale_Price - predictions))
cat("Mean Absolute Error (MAE):", mae_initial, "\n")

# Residual Analysis

residuals <- rstudent(Initial_model)
predicted_values <- predict(Initial_model)

plot(predicted_values, residuals, main="Studentized Residuals vs. Predicted Values",
     xlab="Predicted Values", ylab="Studentized Residuals", col="blue", pch=16)
abline(h=0, col="red")

# Create a normal probability plot

qqnorm(rstandard(Initial_model), main="Normal Q-Q Plot")
qqline(rstandard(Initial_model), col="red")

cooksd <- cooks.distance(Initial_model)
# Find indices of influential points with Cook's distance > 1
influential_indices <- which(cooksd > 1)

library(car)
influenceIndexPlot(Initial_model)

residuals_df <- data.frame(
  Actual = df_test$Sale_Price,
  Predicted = predictions,
  Residuals = df_test$Sale_Price - predictions
)

# Plot histogram or density plot of residuals
ggplot(residuals_df, aes(x = Residuals)) +
  geom_histogram(binwidth = 10000, fill = "blue", color = "white", alpha = 0.7) +
  labs(title = "Distribution of Residuals", x = "Residuals", y = "Frequency")

skewness(residuals_df$Residuals)

# --------------------------------  Variable Selection --------------------------

# Perform backward elimination
#backward_elimination <- step(model_m1, direction = "backward")

# Display the summary of the backward model
summary(backward_elimination)

backward_model <- lm(formula = Sale_Price ~ Lot_Size + BsmtUnfSF + Total_Basement_Area + 
                         Second_Floor_Area + Grade_Living_Area + Screen_Lobby_Area + 
                         House_Life + Building_Class20 + Building_Class30 + Building_Class45 + 
                         Building_Class160 + Zoning_ClassFVR + Zoning_ClassRHD + Zoning_ClassRLD + 
                         Zoning_ClassRMD + Property_ShapeIR3 + Property_ShapeReg + 
                         Land_OutlineLvl + Lot_ConfigurationCulDSac + Property_SlopeMS + 
                         NeighborhoodCollgCr + NeighborhoodCrawfor + NeighborhoodEdwards + 
                         NeighborhoodGilbert + NeighborhoodIDOTRR + NeighborhoodMeadowV + 
                         NeighborhoodMitchel + NeighborhoodNAmes + NeighborhoodNoRidge + 
                         NeighborhoodNridgHt + NeighborhoodNWAmes + NeighborhoodOldTown + 
                         NeighborhoodSawyer + NeighborhoodStoneBr + NeighborhoodSWISU + 
                         NeighborhoodTimber + Condition1Norm + Condition1PosN + Condition1RRAe + 
                         Condition1RRAn + House_Design2.5Fin + House_Design2Story + 
                         Overall_Material2 + Overall_Material3 + Overall_Material4 + 
                         Overall_Material5 + Overall_Material6 + Overall_Material7 + 
                         Overall_Material8 + Overall_Material9 + House_Condition3 + 
                         House_Condition4 + House_Condition5 + House_Condition6 + 
                         House_Condition7 + House_Condition8 + Roof_DesignShed + Exterior1stBrkComm + 
                         Exterior1stBrkFace + Exterior1stCemntBd + Exterior1stMetalSd + 
                         Exterior1stPlywood + Exterior1stVinylSd + Exterior2ndCmentBd + 
                         `Exterior2ndWd Sdng` + Brick_Veneer_TypeBrkFace + Exterior_MaterialFa + 
                         Exterior_MaterialGd + Exterior_MaterialTA + Exterior_ConditionFa + 
                         Exterior_ConditionGd + Exterior_ConditionPo + Exterior_ConditionTA + 
                         Foundation_TypeCB + Foundation_TypePC + Foundation_TypeS + 
                         Basement_HeightFa + Basement_HeightGd + Basement_HeightNo_Basement + 
                         Basement_HeightTA + Basement_ConditionTA + Exposure_LevelGd + 
                         Exposure_LevelMn + Exposure_LevelNo + BsmtFinType1GLQ + Underground_Full_Bathroom1 + 
                         Underground_Full_Bathroom3 + Underground_Half_Bathroom2 + 
                         Full_Bathroom_Above_Grade1 + Full_Bathroom_Above_Grade2 + 
                         Half_Bathroom_Above_Grade1 + Bedroom_Above_Grade1 + Bedroom_Above_Grade3 + 
                         Bedroom_Above_Grade6 + Bedroom_Above_Grade8 + Kitchen_Above_Grade2 + 
                         Kitchen_QualityFa + Kitchen_QualityGd + Kitchen_QualityTA + 
                         Rooms_Above_Grade3 + Rooms_Above_Grade4 + Rooms_Above_Grade5 + 
                         Rooms_Above_Grade6 + Rooms_Above_Grade7 + Rooms_Above_Grade8 + 
                         Rooms_Above_Grade9 + Rooms_Above_Grade10 + Rooms_Above_Grade11 + 
                         Functional_RateSD + Functional_RateTF + Fireplaces1 + Fireplaces2 + 
                         Fireplaces3 + GarageAttchd + GarageBasment + GarageBuiltIn + 
                         GarageCarPort + GarageDetchd + GarageNo_Garage + Garage_Finish_YearUnf + 
                         Garage_Size1 + Garage_Size2 + Garage_Size3 + Garage_QualityFa + 
                         Garage_QualityGd + Sale_TypeCon + Sale_TypeConLD + Sale_TypeCWD + 
                         Sale_TypeNew + Sale_TypeOth + Sale_ConditionAdjLand + Sale_ConditionFamily + 
                         Sale_ConditionNormal + Sale_ConditionPartial, data = df_train)

summary(backward_model)

# Making predictions on test data
predictions_backward <- predict(backward_model, newdata = df_test)

# Calculate Mean Squared Error (MSE)
mse_backward <- mean((df_test$Sale_Price - predictions_backward)^2)
cat("Mean Squared Error (MSE):", mse_backward, "\n")

# Calculate Mean Absolute Error (MAE)
mae_backward <- mean(abs(df_test$Sale_Price - predictions_backward))
cat("Mean Absolute Error (MAE):", mae_backward, "\n")

# Residual Analysis

residuals_backward <- rstudent(backward_model)
predicted_values_backward <- predict(backward_model)

plot(predicted_values_backward, residuals_backward, main="Studentized Residuals vs. Predicted Values for backward model",
     xlab="Predicted Values", ylab="Studentized Residuals", col="blue", pch=16)
abline(h=0, col="red")

# Create a normal probability plot

qqnorm(rstandard(backward_model), main="Normal Q-Q Plot for backward model")
qqline(rstandard(backward_model), col="red")

cooksd_backward <- cooks.distance(backward_model)
# Find indices of influential points with Cook's distance > 1
influential_indices_backward <- which(cooksd_backward > 1)

influenceIndexPlot(backward_model)

residuals_df_backward <- data.frame(
  Actual_backward = df_test$Sale_Price,
  Predicted_backward = predictions_backward,
  Residuals_backward = df_test$Sale_Price - predictions_backward
)

# Plot histogram or density plot of residuals
ggplot(residuals_df_backward, aes(x = Residuals_backward)) +
  geom_histogram(binwidth = 10000, fill = "blue", color = "white", alpha = 0.7) +
  labs(title = "Distribution of Residuals with backward elimination", x = "Residuals", y = "Frequency")

skewness(residuals_df_backward$Residuals_backward)


# ---------------------- Extracting P values for variables for which we did EDA ----------------

# List of variables to extract p-values for
variables <- c("House_Life", "Total_Basement_Area", "Second_Floor_Area",
               "House_Condition8", "House_Condition5", "Overall_Material9", "Sale_ConditionNormal")

# Initialize an empty data frame
p_value_df <- data.frame(Variable = character(), P_Value = numeric(), stringsAsFactors = FALSE)

# Loop through variables and extract p-values
for (variable in variables) {
  p_value <- summary(backward_model)$coefficients[variable, "Pr(>|t|)"]
  p_value_df <- rbind(p_value_df, data.frame(Variable = variable, P_Value = p_value))
}

# Print the resulting data frame
print(p_value_df)
view(p_value_df)


summary(backward_model)$coefficients["House_Life", "Pr(>|t|)"]
summary(backward_model)$coefficients["Total_Basement_Area", "Pr(>|t|)"]
summary(backward_model)$coefficients["Second_Floor_Area", "Pr(>|t|)"]
summary(backward_model)$coefficients["House_Condition8", "Pr(>|t|)"]
summary(backward_model)$coefficients["House_Condition5", "Pr(>|t|)"]
summary(backward_model)$coefficients["Overall_Material9", "Pr(>|t|)"]
summary(backward_model)$coefficients["Sale_ConditionNormal", "Pr(>|t|)"]

# ----------------------------------------

# Perform forward selection
forward_selection <- step(model_m1, direction = "forward", trace = 0)

summary(forward_selection)

# Making predictions on test data
predictions_forward <- predict(forward_selection, newdata = df_test)

# Calculate Mean Squared Error (MSE)
mse_forward <- mean((df_test$Sale_Price - predictions_forward)^2)
cat("Mean Squared Error (MSE):", mse_forward , "\n")

# Calculate Mean Absolute Error (MAE)
mae_forward  <- mean(abs(df_test$Sale_Price - predictions_forward))
cat("Mean Absolute Error (MAE):", mae_forward , "\n")

# Residual Analysis

residuals_forward <- rstudent(forward_selection)
predicted_values_forward <- predict(forward_selection)

plot(predicted_values_forward, residuals_forward, main="Studentized Residuals vs. Predicted Values for forward model",
     xlab="Predicted Values", ylab="Studentized Residuals", col="blue", pch=16)
abline(h=0, col="red")

# Create a normal probability plot

qqnorm(rstandard(forward_model), main="Normal Q-Q Plot for backward model")
qqline(rstandard(forward_model), col="red")

cooksd_forward <- cooks.distance(forward_model)
# Find indices of influential points with Cook's distance > 1
influential_indices_forward <- which(cooksd_forward > 1)

influenceIndexPlot(forward_model)

residuals_df_forward <- data.frame(
  Actual_forward = df_test$Sale_Price,
  Predicted_forward = predictions_forward,
  Residuals_forward = df_test$Sale_Price - predictions_forward
)

# Plot histogram or density plot of residuals
ggplot(residuals_df_forward, aes(x = Residuals_forward)) +
  geom_histogram(binwidth = 10000, fill = "blue", color = "white", alpha = 0.7) +
  labs(title = "Distribution of Residuals with forward selection", x = "Residuals", y = "Frequency")

skewness(residuals_df_forward$Residuals_forward)

# ------------------------------- Model Comparison ---------------

Final_results <- data.frame(Initial_Model = )

Initial_model
backward_model
forward_selection

mse_initial
mae_initial
mse_backward
mae_backward
mse_forward
mae_forward

# Model names
models <- c("Initial_model", "backward_model", "forward_selection")

# R-Squared values
R_Squared_initial <- summary(Initial_model)$r.squared
R_Squared_backward <- summary(backward_model)$r.squared
R_Squared_forward <- summary(forward_selection)$r.squared
R_Squared <- c(R_Squared_initial, R_Squared_backward, R_Squared_forward)

# Adjusted R-Squared values
Adj_R_Squared_initial <- summary(Initial_model)$adj.r.squared
Adj_R_Squared_backward <- summary(backward_model)$adj.r.squared
Adj_R_Squared_forward <- summary(forward_selection)$adj.r.squared
Adj_R_Squared<- c(Adj_R_Squared_initial, Adj_R_Squared_backward, Adj_R_Squared_forward)

# MSE and MAE values
mse_values <- c(mse_initial, mse_backward, mse_forward)
mae_values <- c(mae_initial, mae_backward, mae_forward)

# Create a data frame
comparison_df <- data.frame(Model = models, MSE = mse_values, MAE = mae_values, R_Squared = R_Squared, Adjusted_R_Squared=Adj_R_Squared)

# Print the data frame
print(comparison_df)


#---------------------------------- Random Forest ----------

library(randomForest)

# Fit a Random Forest model
rf_model <- randomForest(Sale_Price ~ ., data = df_train)








  