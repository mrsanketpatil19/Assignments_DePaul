# Problem 1

# Importing data in R

df = read.table("Bankingfull.txt", header = T)
dim(df)
head(df)

# Question 1 : 

# Creating Scatterpot matrix:

pairs(~Balance+Age+Education+Income+HomeVal+Wealth,data = df,main = "Scatterplot Matrix")

# By lookng at the scatterplot matrix, we can see that Variables Income and Wealth both has strong positive linear relation with Balance. Also we are not able to see any outliers in those two variables.
# Variable HomeVal has also a strong linear relation with Balance.
# Variables Age and Education also have linear relation with Balance but not as strong as other variables. Also these two variables have outliers as well.
# All variables have strong or minimal linear relation with each other.

# Question 2 :

cor(df)

# By looking at corelation matrix, we can see that variable Wealth and Income have strong positive correlation with target variable Balance.
# Variable Age, Education and HomeVal have moderate positive correlation with dependent variable Balance.
# Variable Age and Education have the weakest correlation between them.

# Question 3 :

model_M1 <- lm(Balance~Age+Education+Income+HomeVal+Wealth, data=df)

# VIF calculation
library(car)
vif(model_M1)
# We checked the VIF statistics for the above model and found that vaiables Income and Wealth have VIF factor > 10.
# Hence we can conclude that there is a problem of multicollinearity with variables Income and Wealth.  

summary(model_M1)

# Question 4 :

#a)

# In the above model, we can see the Income and Wealth variables have vif value >10. First we will try to refit the model by removing the variable Income as it has highest VIF value.
# Also variable HomeVal have greater P value hence we will remove that variable as well.

model_M2 <- lm(Balance~Age+Education+Wealth, data=df)
vif(model_M2)

summary(model_M2)

# R-squared and adjusted R-squared
summary(model_M1)$adj.r.squared
summary(model_M2)$adj.r.squared

# By removing variables Income and HomeVal, we refit the model and checked the R2 and Adj R2 for both model_M1 and model_M2.
# We can see that model_M1 has better R2 and Adj R2 values than model_M2.

#b)

# Residual Analysis
par(mfrow=c(2,2))
# Standardized Residuals vs Predicted
plot(fitted(model_M1), rstandard(model_M1), main="Standardized Residuals vs Predicted", xlab="Fitted values", ylab="Standardized Residuals")
abline(h=0, col="red")  # Add a horizontal line at y=0 for reference

# By looking at plot, we can see there is less variation of residuals hence we can say model is good. There are also 2 to 3 outlier points.

# Standardized Residuals vs X-variables
plot(df$Age, rstandard(model_M1), main="Standardized Residuals vs AGE", xlab="AGE", ylab="Standardized Residuals")
abline(h=0, col="red")  # Add a horizontal line at y=0 for reference

plot(df$Income, rstandard(model_M1), main="Standardized Residuals vs INCOME", xlab="INCOME", ylab="Standardized Residuals")
abline(h=0, col="red")

plot(df$HomeVal, rstandard(model_M1), main="Standardized Residuals vs HOMEVAL", xlab="HOMEVAL", ylab="Standardized Residuals")
abline(h=0, col="red")

# With the help of above plots, we can see

# Normal Plot of Residuals
qqnorm(rstandard(model_M1), main="Normal Q-Q Plot")
qqline(rstandard(model_M1), col="red")

#c)

# Finding outliers
residuals_standardized <- rstandard(model_M1)
outliers_indices <- which(residuals_standardized > 3)

# Extract values with standardized residuals greater than 3
outliers_values <- residuals_standardized[outliers_indices]

# Show the indices and values of outliers
cat("Indices of outliers:", outliers_indices, "\n")
cat("Values of outliers:", outliers_values, "\n")

cooksd <- cooks.distance(model_M1)
# Find indices of influential points with Cook's distance > 1
influential_indices <- which(cooksd > 1)

influenceIndexPlot(model_M1)

# As there are outliers(Standardized Residuals>3) but the count of those outliers is less and also there is less variation in the residuals, we can conclude that it is a good model.

#d)

library(dplyr)
library(QuantPsyc) 
lm.beta(model_M1)

# By looking at the standardized coefficients, we can conclude that variable "Age" has the strongest effect on target variable variable "Balance".

# Question 5 :

# New data for prediction
new_df <- data.frame(Age = 34, Education = 13, Income = 64000, HomeVal = 140000, Wealth = 160000)

summary(df)

# Prediction and Confidence Interval
Predicted_values <- data.frame(predict(model_M1, newdata=new_df, interval="confidence"))

# Print in the desired format
cat("Predicted average bank balance =", Predicted_values[, 1], "\n")
cat("Lower 95% Confidence Interval =", Predicted_values[, 2], "\n")
cat("Upper 95% Confidence Interval =", Predicted_values[, 3], "\n")




# Problem 2 :

# Importing data in R

data = read.csv("pgatour2006_small.csv", header = T)
dim(data)
head(data)

# Remove the variable "Name" as it has unique values.
data <- data[, !names(data) %in% "Name"]

# Question 1 : 

# Creating Scatterpot matrix:

pairs(~PrizeMoney+DrivingAccuracy+GIR+PuttingAverage+BirdieConversion+PuttsPerRound,data = data,main = "Scatterplot Matrix")

# By looking at the scatterplot, we can conclude that there is no linear relationship between PrizeMoney and any of the independent variable. 
# There is linear relationsip between variables "PuttingAverage" and "BirdieConversion".

# Question 2 :

# Histogram of PrizeMoney

hist(data$PrizeMoney,xlab = "Prize Money",col = "brown",border = "black")

# By looking at the histogram, we can say that the data is highly right skewed.

# Question 3 :

# Log transformation
data$ln_Prize <- log(data$PrizeMoney)

# Histogram of ln_Prize
hist(data$ln_Prize, main="Distribution of ln_Prize", xlab = "ln_Prize",col = "brown",border = "black")

# After applying a log transformation to the variable 'PrizeMoney,' the distribution of the data appears to approximate a normal distribution.

# Question 4 :

# Remove the variable "Name" as it has unique values.
head(data) 
cor(data)

# First we will train the model with all independent variables to predict Ln_Prize
model_m1 <- lm(ln_Prize ~ DrivingAccuracy+GIR+PuttingAverage+BirdieConversion+PuttsPerRound, data=data)


# a)
summary(model_m1)
# By lokking at the summary of the model, we will remove variable "DrivingAccuracy" as it has the highest P-Value.

model_m2 <- lm(ln_Prize ~ GIR+PuttingAverage+BirdieConversion+PuttsPerRound, data=data)
summary(model_m2)

# After refitting the model, we can see adjusted R2 increased from 0.5293 to 0.5318.
# We can still see variable "PuttingAverage" have high P-value. Hence we will again refit the model without variable "PuttingAverage".

model_m3 <- lm(ln_Prize ~ GIR+BirdieConversion+PuttsPerRound, data=data)
summary(model_m3)

# After refitting the model, now we can see all variables looking significant. 
# Adjusted R-squared:  0.5274 

# b)

# Residual plots
par(mfrow=c(2,2))
plot(model_m3)

# By looking at the Residuals vs Fitted plot, we can say the model looks valid as variation is somewhat less and there are less number of outliers.
# With the help of Q-Q plot, we can see there are many points which follow the line.

# c)
# Influential points
influenceIndexPlot(model_m3)

# Finding outliers
residuals_standardized_1 <- rstandard(model_m3)
outliers_indices_1 <- which(residuals_standardized_1 > 3)

# Extract values with standardized residuals greater than 3
outliers_values_1 <- residuals_standardized_1[outliers_indices_1]

# Show the indices and values of outliers
cat("Indices of outliers:", outliers_indices_1, "\n")
cat("Values of outliers:", outliers_values_1, "\n")

# We have chosen these points as outliers as the value for standardized residuals is greater than 3. i.e these points are 3 standard deviations away from the mean. Hence we can call those points as outliers.

# Question 5 : 

coefficients <- coef(model_m1)
coefficients

# For each 1% increase in the GIR, we expect an average increase of exp(0.2687897963) times in PrizeMoney, holding other factors constant.


# Question 6 :

test_data <- data.frame(DrivingAccuracy = 64, GIR = 67, BirdieConversion = 28, PuttingAverage = 1.77, PuttsPerRound = 29.16)

# Predictions and 95% prediction interval
prediction <- predict(model_m1, newdata = test_data, interval = "prediction", level = 0.95)

# Display the results
print(prediction)

# Back-transform predictions and interval to original scale
prediction_original <- exp(prediction[, 1])
lower_bound_original <- exp(prediction[, 2])
upper_bound_original <- exp(prediction[, 3])

# Display the back-transformed results
print(data.frame(Predicted_PrizeMoney = prediction_original, Lower_Bound = lower_bound_original, Upper_Bound = upper_bound_original))






