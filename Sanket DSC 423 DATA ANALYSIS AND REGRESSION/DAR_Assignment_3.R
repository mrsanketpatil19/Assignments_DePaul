# Problem 1

library(psych)

# Importing data in R

df = read.csv("college.csv", header = T)
dim(df)
head(df)

describe(df$Grad.Rate)

# Drop column with unique values
df <- df[, !colnames(df) %in% "school"]

# NA values check
colMeans(is.na(df)) * 100

# Question 1:

# Plot the distribution
hist(df$Grad.Rate, main = "Distribution of Graduation Rates", xlab = "Graduation Rate", col = "lightblue", border = "black")

skew(df$Grad.Rate)

# By seeing at the data, data looks normally distributed but slightly negatively skewed.

# Question 2 :
# Scatter plots

plot(df$Grad.Rate, df$Accept.pct, , xlab = "Graduation Rate",  ylab = "Acceptance Percentage", col = "brown", pch = 16)
# Slight negative linear relation between Grad.Rate and Accept.pct

plot(df$Grad.Rate, df$F.Undergrad, , xlab = "Graduation Rate",  ylab = "F.Undergrad", col = "brown", pch = 16)
# No linear relation between Grad.Rate and F.Undergrad

plot(df$Grad.Rate, df$P.Undergrad, , xlab = "Graduation Rate",  ylab = "P.Undergrad", col = "brown", pch = 16)
# No linear relation between Grad.Rate and P.Undergrad

plot(df$Grad.Rate, df$Outstate, , xlab = "Graduation Rate",  ylab = "Outstate ", col = "brown", pch = 16)
# There is positive linear relation between Grad>rate and Outstate

plot(df$Grad.Rate, df$Room.Board, , xlab = "Graduation Rate",  ylab = "Room.Board ", col = "brown", pch = 16)
# There is positive linear relation between Grad>rate and Room.Board

plot(df$Grad.Rate, df$Personal, , xlab = "Graduation Rate",  ylab = "Personal ", col = "brown", pch = 16)
# There is no linear relation between Grad.Rate and Personal

plot(df$Grad.Rate, df$PhD, , xlab = "Graduation Rate",  ylab = "PhD ", col = "brown", pch = 16)
# There is slight linear relation between Grad.Rate and PhD

plot(df$Grad.Rate, df$Terminal, , xlab = "Graduation Rate",  ylab = "Terminal  ", col = "brown", pch = 16)
# There is no linear relation between Grad.Rate and Terminal

# Separating numeric columns
numeric_cols <- sapply(df, is.numeric)
df_numeric <- df[, numeric_cols]
correlation_matrix <- cor(df_numeric)

# By looking at the correlation_matrix, we can clearly see that variable Outstate and Grad.Rate have the highest correlation value of 0.5712.
# Where as variables Books and Grad.Rate have the least correlation value of 0.001060894.

# Question c:

# Boxplot for graduation rates by university type
boxplot(Grad.Rate ~ Private, data = df, main = "Graduation Rate by University Type",
        col = c("skyblue", "lightgreen"), ylab = "Graduation Rate")

# Boxplot for graduation rates by elite status
boxplot(Grad.Rate ~ Elite10, data = df, main = "Graduation Rate by Elite Status",
        col = c("skyblue", "lightgreen"), ylab = "Graduation Rate")

# By looking at the plots, we can see Graduation rate vary by status(elite vs non elite) as there is clear separation between two classes.
# By looking at the plots, we can see there is slight overlapping of Public and Private university. Hence Graduation rates vary by University Type but not as good as Status(elite vs non elite).

# Question D:

df$Private <- ifelse(df$Private == "Yes", 1, 0)

# Applying linear regression model
model <- lm(Grad.Rate ~ ., data=df)
summary(model)

# Question e:

library(car)

vif(model)

# With the help of vif values, we can see that there is no multi-collinearity between variables as all variables have value < 10.

# Question f:

# Using backward selection method and forward selection method for variable selection

backward_model <- step(model, direction="backward")
forward_model <- step(model, direction="forward")

summary(backward_model)
summary(forward_model)

# By seeing the results for both the models, we can see backward selection model gave more significant variables than forward selection. Also adjusted R-squared is slightly better in backward selection model.

# Question g:

# As explained above, we can clearly see backward selection model performed better in terms of variable significance and adjusted R-squared. Hence we will choose backward selection model.
# We will use the backward_model from now onwards for analysis. 

final_model <- lm(formula = Grad.Rate ~ Private + Accept.pct + Elite10 + F.Undergrad + 
                    P.Undergrad + Outstate + Room.Board + Personal + PhD + perc.alumni + 
                    Expend, data = df)
summary(final_model)

# Grad.Rate=Grad.Rate=48.40+4.77×Private−17.78×Accept.pct+4.02×Elite10+0.00066×F.Undergrad−0.00196×P.Undergrad+0.00122×Outstate+0.00153×Room.Board−0.00182×Personal+0.0842×PhD+0.306×perc.alumni−0.0004465×Expend

# Question h:

residuals <- rstudent(final_model)
predicted_values <- predict(final_model)

# Scatter plot residuals vs predicted_values
plot(predicted_values, residuals, main="Studentized Residuals vs. Predicted Values",
     xlab="Predicted Values", ylab="Studentized Residuals", col="blue", pch=16)
abline(h=0, col="red")

# By seeing at the scatter plot, we can see variation in studentized residuals first increased and then decreased. Hence our model didn't performed better as variation is present.Also there are some outliers which has studentized residuals > 3.

# Question i:

residuals <- residuals(final_model)

# Create a normal probability plot

qqnorm(rstandard(final_model), main="Normal Q-Q Plot")
qqline(rstandard(final_model), col="red")

# By seeing at the nor probability plot, we can see that there are many points which do not follow the line. Hence model does not fit well.

# Question j:

# Finding outliers
residuals_standardized <- rstandard(final_model)
outliers_indices <- which(residuals_standardized > 3)

# Extract values with standardized residuals greater than 3
outliers_values <- residuals_standardized[outliers_indices]

# Show the indices and values of outliers
cat("Indices of outliers:", outliers_indices, "\n")
cat("Values of outliers:", outliers_values, "\n")

cooksd <- cooks.distance(final_model)
# Find indices of influential points with Cook's distance > 1
influential_indices <- which(cooksd > 1)

influenceIndexPlot(final_model)

# We can see three indexes which has outliers i.e. Standardized Residuals>3. 

# Question k:

summary(final_model)

# The final model's R-squared value is 0.4431, indicating that it explains approximately 44.31% of the variation in graduation rates among universities. The Adjusted R-squared is 0.4351.
# The model provides a moderate level of explanatory power for predicting graduation rates.
# The F-statistic is 55.33, and the p-value is very low (2.2e-16), indicating that the overall model is statistically significant.

# Question l:

# Based on the summary of the model,
The R-squared score of 0.4431 indicates that the model appears to fit the data quite well. This suggests that the model explains approximately 44.31% of the variation in graduation rates.
# Top 3 most Important Predictors:
1. perc.alumni - A positive coefficient indicates that a higher percentage of alumni donations is associated with higher graduation rates.
2. P.Undergrad -  A negative coefficient indicates that increasing the number of part-time undergraduate students coincides with a decrease in graduation rates.
3. Outstate - A positive coefficient indicates that a higher percentage of outstate is associated with higher graduation rates.

The variable 'Private' has a positive coefficient (4.770) and is statistically significant (p=0.00486). According to the model, private universities have greater graduation rates than public universities.

The "Elite10" variable has a positive coefficient, showing that top universities have higher graduation rates. However, the significance level is minimal (p-value = 0.04492).



# Problem 2:

# Question 1

# Creating an interaction model 

interaction_model <- lm(Grad.Rate ~ Elite10+ Accept.pct+ Outstate+ perc.alumni + Expend + Elite10 * Accept.pct + Elite10 * Outstate + Elite10 * perc.alumni + Elite10 * Expend, data = df)
summary(interaction_model)

# The Elite101:perc.alumni interaction term is not statistically significant (p-value = 0.362485).
# Other interaction terms have p-values below 0.05, indicating significance.


# Question 2

interaction_modelM2 <- lm(Grad.Rate ~ Elite10+ Accept.pct+ Outstate+ perc.alumni + Expend + Elite10 * Accept.pct + Elite10 * Outstate + Elite10 * Expend, data = df)
summary(interaction_modelM2)

# Grad.Rate=53.14+35.85×Elite10−15.05×Accept.pct+0.00232×Outstate+0.3334×perc.alumni−0.000951×Expend−21.64×Elite10×Accept.pct−0.002253×Elite10×Outstate+0.001057×Elite10×Expend

# Question 3

# The coefficient for Elite101 is 35.85, indicating that, on average, being an "Elite10" University is associated with an increase of 35.85 units in the Graduation Rate, holding other variables constant.
# The coefficient for Accept.pct is -15.05, suggesting that as the percentage of applicants accepted increases by one unit, the Graduation Rate is expected to decrease by 15.05 units, holding other variables constant.
# The coefficient for Outstate is 0.00232, suggesting that for each additional unit in out-of-state tuition, the Graduation Rate is expected to increase by 0.00232 units.
# The coefficient for perc.alumni is 0.3334, indicating that as the percentage of alumni who donate increases by one unit, the Graduation Rate is expected to increase by 0.3334 units.
# The coefficient for Expend is -0.000951, suggesting that for each additional unit in instructional expenditure per student, the Graduation Rate is expected to decrease by 0.000951 units, holding other variables constant.


# Question 4

# Load required libraries
library(caret)
library(Metrics)

set.seed(123)

# Define the control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 5)

final_model_cv <- train(Grad.Rate ~ Private + Accept.pct + Elite10 + F.Undergrad + 
                    P.Undergrad + Outstate + Room.Board + Personal + PhD + perc.alumni + 
                    Expend, data = df,method = "lm", trControl = ctrl)


predictions_M1 <- predict(final_model_cv)
mape_M1 <- mape(df$Grad.Rate, predictions_M1)
cat("MAPE for Model M1:", mape_M1, "\n")


# Question 5:

interaction_modelM2_cv <- train(Grad.Rate ~ Elite10+ Accept.pct+ Outstate+ perc.alumni + Expend + Elite10 * Accept.pct + Elite10 * Outstate + Elite10 * Expend, 
                                data = interaction_df,method = "lm", trControl = ctrl)

summary(interaction_modelM2_cv)

predictions_M2 <- predict(interaction_modelM2_cv, newdata = df)

mape_M2 <- mape(df$Grad.Rate, predictions_M2)
cat("MAPE for Model M2:", mape_M2, "\n")



