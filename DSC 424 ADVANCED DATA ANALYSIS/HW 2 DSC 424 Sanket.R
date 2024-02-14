# Problem 1:


a) Why do we use regularized regressions?  Give examples of when you would use ridge versus lasso regressions?

-> We use regularized regressions, such as ridge and lasso regressions, to handle situations where traditional linear regression models may overfit or become unstable due to multicollinearity (high correlation between predictor variables).

Ridge regression: It's useful when we have many correlated predictors and we want to shrink the coefficients towards zero without eliminating them entirely. For example, in a dataset where multiple predictors are highly correlated, ridge regression can help in stabilizing the estimates.

Lasso regression: It's beneficial when we want a sparse model with fewer predictors, as it tends to shrink less important predictors to exactly zero, effectively performing variable selection. For instance, in a dataset with a large number of predictors but only a few are expected to be truly influential, lasso regression can help in identifying and selecting those important predictors.

b) How would we treat overfitting in a model with too many variables compared to the sample size?
  
-> To treat overfitting in a model with too many variables compared to the sample size, we can employ various techniques:
  
Feature selection: We can use techniques like ridge regression or lasso regression, which penalize the coefficients of less important predictors, effectively performing feature selection and reducing the model complexity.

Cross-validation: We can use techniques like k-fold cross-validation to assess model performance on unseen data. By splitting the data into training and validation sets multiple times and evaluating the model's performance, we can detect and mitigate overfitting.

Regularization: Regularized regression techniques like ridge and lasso regressions can also help in reducing overfitting by penalizing large coefficients.

c) How do we check the assumptions of linear regression in R?  Give an example for how each assumption may be violated.

-> In R, we can check the assumptions of linear regression using various diagnostic tools and visualizations:
  
  Linearity: We can check for linearity by plotting the observed values against the predicted values from the linear regression model. If the relationship between the predictors and the response variable is not linear, it may violate the assumption of linearity.

Homoscedasticity: We can assess homoscedasticity by plotting the residuals (the differences between observed and predicted values) against the predicted values. If the spread of residuals is consistent across all levels of the predicted values, homoscedasticity is met. Violations may manifest as a funnel shape or patterns in the residual plot.

Independence of residuals: We can check for independence by examining the autocorrelation plot of residuals or using statistical tests like the Durbin-Watson test. If there is a pattern in the autocorrelation plot or if the Durbin-Watson test indicates significant autocorrelation, it suggests violations of independence.

Normality of residuals: We can assess normality by plotting a histogram or a Q-Q plot of the residuals. If the residuals are approximately normally distributed, the assumption is met. Violations may appear as skewed or heavy-tailed distributions in the histogram or deviations from the straight line in the Q-Q plot.


# --------------------------------------------------------------------------------------------------

# Problem 3:

1) How are they applying Factoring Analysis?
-> In this study, researchers wanted to create a tool with the help of Exploratoray Analysis and PCA to measure how good teachers are at using a mix of online and in-person teaching, called hybrid teaching. 
   They have done this in below 5 steps :
   a) Understanding what to measure
   b) Checking if their tool makes sense
   c) Testing the questions
   d) Seeing how the questions fit together
   e) Making sure the tool is reliable

2) What kind of factor rotation do they use?
-> Used Promax rotation as a rotation method.

3) How many factors do they concentrate on in their analysis? How did they arrive at these number of factors?
-> In their analysis, the researchers identified and retained five factors that represented different aspects of hybrid education competence
   They arrived at the number of factors through the process of exploratory factor analysis (EFA), which is a statistical technique used to uncover the underlying structure of a set of variables.
   The researchers conducted exploratory factor analysis (EFA) on the collected data from educators in social and health care, and health sciences fields. During this process, they examined the eigenvalues associated with each factor. Eigenvalues represent the amount of variance explained by each factor. Factors with eigenvalues greater than 1 were considered for retention.
   After identifying the initial set of factors, the researchers examined the pattern of factor loadings for each variable. Factor loadings indicate the strength and direction of the relationship between variables and factors. They looked for variables that loaded strongly on each factor and interpreted the meaning of these factors based on the variables they comprised.
   Based on the pattern of factor loadings and the interpretability of the factors, the researchers determined the number of meaningful factors that best represented the dimensions of hybrid education competence. They named and interpreted these factors according to the variables that loaded onto them and the conceptual framework established in the study.
   
4) Explain the breakdown of the factors and the significance of their names.
-> Competence in planning and resourcing hybrid teaching: 
   This means educators can effectively organize and gather everything they need for teaching both in-person and online at the same time. They're good at making schedules and choosing the right materials for lessons.
   Technological competence in hybrid teaching: 
   This is about educators being comfortable using technology to teach. They know how to use computers, internet tools, and other gadgets to make sure their lessons run smoothly, whether students are in class or learning remotely.
   Interaction competence in hybrid teaching: 
   This is about how well educators can encourage students to talk to each other and participate in class, whether they're in person or online. Good interaction means students feel involved and engaged in their learning.
   Digital pedagogy competence in hybrid teaching: 
   This is about educators knowing how to teach effectively using digital tools. They understand different ways of teaching and testing students online, and they can adapt their teaching style to fit different situations.
   Ethical competence in hybrid teaching: 
   This is about educators making sure they do the right thing when teaching online. They respect students' privacy, make sure everyone has a fair chance to learn, and follow rules for online behavior.

5) How do they evaluate the stability of the components (i.e., factorability)?
-> To evaluate the stability of the components, or factorability, the researchers used two main criteria:
   a) Kaiser-Meyer-Olkin (KMO) Measure: 
   This measure assesses the sampling adequacy for factor analysis. It indicates whether the variables in the dataset are suitable for factor analysis. A KMO value closer to 1 indicates better suitability, with values above 0.5 generally considered acceptable.
   
   b) Bartlett’s test of sphericity was used in order to assume factorability of correlation matrix.  


6) Do they use these factors in later analysis, such as regression?  If so, what do they discover?
-> Yes, the factors like planning lessons, using technology, interacting with students, teaching online skills, and being fair were important for teachers in hybrid classes. They used these factors to understand how good they were at hybrid teaching. They found out that they needed to look at each factor more closely to really understand how well they were doing. Overall, the study showed that it's important for teachers to be good at all these things to teach hybrid classes well.

7) What overall conclusions does Factor Analysis allow them to draw?
-> Factor Analysis helped researchers understand what skills are important for educators who teach both online and in-person. They created a tool called HybridEduCom to measure these skills, like planning lessons, using technology, interacting with students, and being ethical. They tested this tool with 206 educators and found it reliable. This means it can be used to improve teaching and training programs. Overall, the study shows how important it is for educators to be skilled in both online and traditional teaching methods, especially in today's digital world.




# ----------------------------------------------------------------------------------------------------------

# Problem 4:

# Load necessary libraries
library(DescTools)
library(Hmisc) #Describe Function
library(psych) #Multiple Functions for Statistics and Multivariate Analysis
library(GGally) #ggpairs Function
library(ggplot2) #ggplot2 Functions
library(vioplot) #Violin Plot Function
library(corrplot) #Plot Correlations
library(REdaS) #Bartletts Test of Sphericity
library(psych) #PCA/FA functions
library(factoextra) #PCA Visualizations
library("FactoMineR") #PCA functions
library(ade4) #PCA Visualizations

# Read the CSV file
df <- read.csv("D:/Assignments_Depaul/DSC_424_Advance_Data_Analysis/HW2/BIG5.csv", header = TRUE)
View(df)
dim(df)

names(df)

# checking if data has NA values
sum(is.na(df))

# Checking the structure of data
str(df)

summary(df$E1)

histogram(df$E2)


# Checking the corrplot matrix
cor_matrix <- cor(df)

library(caret)
highly_correlated_vars <- findCorrelation(cor_matrix, cutoff = 0.75)  
colnames(df[highly_correlated_vars])

# As we have high correlation between variables N8 and N7, we will remove one of the variable

df <- df[, -highly_correlated_vars]
dim(df)

# PCA_Plot functions

PCA_Plot = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

PCA_Plot_Secondary = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC3, y = PC4, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC3", y = "PC4")
}

PCA_Plot_Psyc = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = as.data.frame(unclass(pcaData$loadings))
  s = rep(0, ncol(loadings))
  for (i in 1:ncol(loadings))
  {
    s[i] = 0
    for (j in 1:nrow(loadings))
      s[i] = s[i] + loadings[j, i]^2
    s[i] = sqrt(s[i])
  }
  
  for (i in 1:ncol(loadings))
    loadings[, i] = loadings[, i] / s[i]
  
  loadings$.names = row.names(loadings)
  
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

PCA_Plot_Psyc_Secondary = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = as.data.frame(unclass(pcaData$loadings))
  s = rep(0, ncol(loadings))
  for (i in 1:ncol(loadings))
  {
    s[i] = 0
    for (j in 1:nrow(loadings))
      s[i] = s[i] + loadings[j, i]^2
    s[i] = sqrt(s[i])
  }
  
  for (i in 1:ncol(loadings))
    loadings[, i] = loadings[, i] / s[i]
  
  loadings$.names = row.names(loadings)
  
  print(loadings)
  p + geom_text(data=loadings, mapping=aes(x = PC3, y = PC4, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC3", y = "PC4")
}


# PCA / FA

# Test KMO Sampling Adequancy
library(psych)
KMO(df)
# The overall MSA is 0.91, which suggests that your dataset is suitable for factor analysis.
# Additionally, the MSA for each individual item is generally high, with most variables having an MSA above 0.8, indicating that each variable contributes adequately to the factor analysis.


# Test Bartlett's test of Sphericity
library(REdaS)
bart_spher(df)

# The small p-value (< 2.22e-16) suggests that the observed correlation matrix is significantly different from the identity matrix, providing evidence against the null hypothesis of sphericity.


#Parallel Analysis (Horn's parallel analysis)
comp <- fa.parallel(df)
comp


# ----------------------------------- Create PCA 

PCA = prcomp(df, center = T, scale = T)
PCA

# Checking the scree plot
plot(PCA, main="Scree plot", xlab="PC")
abline(1,0)

summary(PCA)

#Check PCA visualizations
PCA_Plot(PCA) #PCA_plot1
PCA_Plot_Secondary(PCA) #PCA_Plot2
biplot(PCA) #Biplot

#Running PCA again after removing the irrelevant variables
PCA2 = psych::principal(df, rotate="varimax", nfactors=7, scores=TRUE)
print(PCA2$loadings, cutoff=.4, sort=T)

ls(PCA2)

# Checking eigen vlues
PCA2$values

# To select number of components
table(PCA2$values > 1)

eigenvalues <- PCA2$values

# Calculate the cumulative sum of eigenvalues
cumulative_variance <- cumsum(eigenvalues) / sum(eigenvalues)

# Find the number of components needed to explain 100% of the variance
num_components_100 <- which.max(cumulative_variance >= 1)

# Print the result
cat("Number of components to explain 100% of the variance:", num_components_100, "\n")

desired_variance_explained <- 0.95
num_components_desired <- which.max(cumulative_variance >= desired_variance_explained)
cat("Number of components to explain 95% of the variance:", num_components_desired, "\n")


# Question A:
# With the help of above information, we can see we required all 49 components to explain 100% of the variance.
# With the help of eigen values method, we can see 7 components have eigen values greater than 1. 
# Hence 7 principle components were selected by scree plot.
# Number of components to explain 95% of the variance: 42
# We will go with number of components as 7 as more components increase model complexity, which may lead to overfitting, especially if we have limited data.


#PCA Variables
pca_var<-fviz_pca_var(PCA,
                      col.var = "contrib", # Color by contributions to the PC
                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                      repel = TRUE     # Avoid text overlapping
)
pca_var

# ------------------- Formula for PCA 

# Extract loadings for the first component
loadings_component1 <- PCA2$loadings[, 1]
loadings_component1

# Question B:
# Compute the formula for the first component
# PC1 = 0.690 * E1 - 0.736 * E2 + 0.653 * E3 - 0.754 * E4 + 0.741 * E5 - 0.635 * E6 + 0.743 * E7 - 0.628 * E8 + 0.652 * E9 - 0.698 * E10 - 0.096 * N1 + 0.064 * N2 - 0.134 * N3 + 0.112 * N4 - 0.057 * N5 - 0.070 * N6 - 0.029 * N7 - 0.051 * N9 - 0.249 * N10 - 0.043 * A1 + 0.352 * A2 + 0.122 * A3 + 0.024 * A4 - 0.155 * A5 - 0.037 * A6 - 0.338 * A7 + 0.105 * A8 + 0.094 * A9 + 0.320 * A10 + 0.037 * C1 - 0.055 * C2 - 0.078 * C3 + 0.074 * C4 - 0.024 * C5 - 0.044 * C6 - 0.081 * C7 + 0.055 * C8 + 0.018 * C9 + 0.058 * C10 + 0.015 * O1 - 0.043 * O2 + 0.015 * O3 - 0.008 * O4 + 0.198 * O5 - 0.099 * O6 + 0.068 * O7 + 0.021 * O8 - 0.161 * O9 + 0.183 * O10
# After rotating the components, each component represents a linear combination of the original variables in such a way that the first component captures the maximum amount of variance in the data. This means that the first component contains the most information compared to any other component. Rotating the components ensures that each subsequent component captures the maximum remaining variance orthogonal to the previous components.
# The names of the components will be as follows
# Social Butterfly & Emotional Stability: How much you enjoy socializing and how well you handle stress and emotions.
# Sensitive & Moody: How easily you get upset and your tendency to experience mood swings.
# Empathetic & Caring: How much you care about others' feelings and emotions.
# Organized & Responsible: How well you plan ahead and take care of your duties.
# Curious & Creative: How interested you are in new ideas and how imaginative you can be.
# Outgoing & Talkative: How much you enjoy talking to people and engaging in social activities.
# Efficient & Disciplined: How well you manage tasks and stick to routines and rules.


# Question C:

# --------------- Calculating the scores

scores <- PCA2$scores

# Calculating the principle components 
for (i in 1:7) {
  # Get the column index for the current component
  component_col <- scores[, i]
  
  # Find the index of the subject with the highest score
  max_index <- which.max(component_col)
  
  # Find the index of the subject with the lowest score
  min_index <- which.min(component_col)
  
  # Print the results
  cat("Principal Component", i, ":\n")
  cat("Subject with the highest score:", "Index:", max_index, "Score:", component_col[max_index], "\n")
  cat("Subject with the lowest score:", "Index:", min_index, "Score:", component_col[min_index], "\n\n")
}

# Create an empty data frame to store the scores
score_table <- data.frame(
  Subject_Index = numeric(),
  PC1 = numeric(),
  PC2 = numeric(),
  PC3 = numeric(),
  PC4 = numeric(),
  PC5 = numeric(),
  PC6 = numeric(),
  PC7 = numeric(),
  stringsAsFactors = FALSE
)

# Function to add scores to the table
add_scores <- function(subject_index) {
  scores <- c(subject_index, scores[subject_index, ])
  score_table[nrow(score_table) + 1, ] <<- scores
}

# Add scores for subjects with highest and lowest scores in each component
add_scores(4177)  # Highest score in PC1
add_scores(629)   # Lowest score in PC1
add_scores(12089) # Highest score in PC2
add_scores(19065) # Lowest score in PC2
add_scores(17760) # Highest score in PC3
add_scores(9864)  # Lowest score in PC3
add_scores(16225) # Highest score in PC4
add_scores(15237) # Lowest score in PC4
add_scores(445)   # Highest score in PC5
add_scores(6106)  # Lowest score in PC5
add_scores(13095) # Highest score in PC6
add_scores(19065) # Lowest score in PC6
add_scores(2794)  # Highest score in PC7
add_scores(19065) # Lowest score in PC7

# Principal component scores for each subject 
score_table

# Question D:

# ------------------------------------ Factor Analysis

# Conducting factor analysis

fit = factanal(df, 7)
print(fit$loadings, cuttoff=0.4, sort=T)

# Looking at the values, we can observe differences in the loadings between the two methods.Check below examples
# For variable E1, in factor analysis, it has a loading of 0.657 on Factor1, while in PCA, it has a loading of 0.690 on RC1.
# For variable A4, in factor analysis, it has a loading of 0.790 on Factor4, while in PCA, it has a loading of 0.807 on RC1.
# For variable O3, in factor analysis, it has a loading of 0.593 on Factor5, while in PCA, it has a loading of 0.732 on RC3.
# Despite these differences, the interpretation of the factors remains somewhat consistent. Both analyses identify similar latent constructs (e.g., Extraversion, Neuroticism, Agreeableness) based on the variables' loadings on each factor.
# The underlying latent constructs remain similar, allowing for a consistent interpretation of the results in most cases.
