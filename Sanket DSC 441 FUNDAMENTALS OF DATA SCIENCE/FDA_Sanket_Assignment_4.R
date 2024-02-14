
# Problem 1:

# Loading required libraries
library(dplyr)

# Question 1:

# Load red and white wine datasets

red_wine <- read.csv("winequality-red.csv", header = T,sep = ";")
white_wine <- read.csv("winequality-white.csv",header = T,sep = ";")

# Add a type column to each dataset
red_wine$type <- "red"
white_wine$type <- "white"

head(red_wine)
head(white_wine)

# Checking class of all variables
sapply(red_wine, class)
sapply(white_wine, class)

# Merge the two datasets
df <- full_join(red_wine, white_wine)

# Question 2:

library(ggplot2)
library(caret)

# Separate dependent and independent variables
x <- df[, -ncol(df)]
y <- df$type

# Scale the data
scaled_x <- scale(x)  

# Perform PCA
df_pca <- prcomp(scaled_x, center = TRUE, scale. = TRUE)

# Extract the first two principal components to create a projection of the data to 2D
pc1 <- df_pca$x[, 1]
pc2 <- df_pca$x[, 2]

# Create a new dataframe for plotting
df_scatterplot <- data.frame(PC1 = pc1, PC2 = pc2, Type = y)

# Creating scatterplot
ggplot(df_scatterplot, aes(x = PC1, y = PC2, color = Type)) + geom_point() + labs(title = "PCA Projection of Wine Quality Data")

# Question 3:

# By seeing the scatterplot in question b, we can see the data is well separated by type hence we will perform KNN as it might be the best choice.

# Question 4:

library(class)
library(e1071)
library(rpart)
library(caret)

# Convert type to factor
df$type <- as.factor(df$type)

# Split the data into training and testing sets
set.seed(123)
train_indices <- createDataPartition(df$type, p = 0.8, list = FALSE)
df_train <- df[train_indices, ]
df_test <- df[-train_indices, ]

# kNN

k_values <- 1:10  # Add more values as needed
tune_grid <- expand.grid(k = k_values)

knn_model_tuned <- train(
  type ~ ., 
  data = df_train, 
  method = "knn", 
  preProcess = c("center", "scale"), 
  tuneGrid = tune_grid,  # Specify the tuning grid
  trControl = trainControl(method = "cv", number = 5)  # 5-fold cross-validation
)
print(knn_model_tuned)

predictions_knn <- predict(knn_model_tuned, newdata = df_test)
knn_accuracy <- sum(predictions_knn == df_test$type) / length(df_test$type)

# Decision Trees
colnames(df) <- make.names(colnames(df))
tree_model <- rpart(type ~ ., data = df_train, method = "class")
tree_predictions <- predict(tree_model, df_test, type = "class")
tree_accuracy <- sum(tree_predictions == df_test$type) / length(df_test$type)

# SVM
df_train$type <- as.factor(df_train$type)
df_test$type <- as.factor(df_test$type)
svm_model <- svm(type ~ ., data = df_test, kernel = "linear", cost = 1)
svm_predictions <- predict(svm_model, df_test)
svm_accuracy <- sum(svm_predictions == df_test$type) / length(df_test$type)

# Compare accuracies
accuracy_df <- data.frame(Classifier = c("kNN", "Decision Tree", "SVM"),
                          Accuracy = c(knn_accuracy, tree_accuracy, svm_accuracy))
print(accuracy_df)

# kNN classifier performed the best based in terms of accuracy, as it achieved the highest accuracy of 99.46%.

# Question 5:

# Add predicted labels to the original dataframe
df_scatterplot$knn_labels <- predict(knn_model_tuned, newdata = df)
df_scatterplot$dt_labels <- predict(tree_model, df, type = "class")
df_scatterplot$svm_labels <- predict(svm_model, df)

# Scatterplot with classifier labels
ggplot(df_scatterplot, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = knn_labels), alpha = 0.5) +
  labs(title = "kNN Classifier Labels on PCA Projection")

ggplot(df_scatterplot, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = dt_labels), alpha = 0.5) +
  labs(title = "Decision Tree Classifier Labels on PCA Projection")

ggplot(df_scatterplot, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = svm_labels), alpha = 0.5) +
  labs(title = "SVM Classifier Labels on PCA Projection")


# Problem 2 :

library(tidyverse)
library(class)

# Question 1 :

data("Sacramento")

head(Sacramento)
dim(Sacramento)

# Check for unique values
sapply(Sacramento, function(x) n_distinct(x))

# Separating target variable from data
target_variable <- data.frame(Sacramento$type)

# Drop type variable
Sacramento <- Sacramento[, !colnames(Sacramento) %in% "type"]

# Converting to dummies
Sacramento_dummies <- Sacramento %>%
  select(-zip) %>%  # We can exclude zip varible as there is no dependency 
  model.matrix(~ . - 1, data = .) %>%  # Create dummy variables for all variables
  as.data.frame()

Sacramento_dummies$type <- target_variable$Sacramento.type

head(Sacramento_dummies)
dim(Sacramento_dummies)

# Question 2:
# Choosing distance function for kNN
# For high dimensionality, Manhattan distance (p = 1) might be a good choice.

# Question 3:

library(kknn)

#target_variable <- "type"

# setup a tuneGrid with the tuning parameters
tuneGrid <- expand.grid(kmax = 3:7,                        
                        kernel = c("rectangular", "cos"),  
                        distance = 1:3)
suppressWarnings({
kknn_fit <- train(type ~ ., 
                  data = Sacramento_dummies,
                  method = 'kknn',
                  trControl = ctrl,
                  preProcess = c('center', 'scale'),
                  tuneGrid = tuneGrid)})
kknn_fit

# Predicting the type
pred_knn <- predict(kknn_fit, Sacramento_dummies)

# Generate confusion matrix
confusionMatrix(Sacramento_dummies$type, pred_knn)

table(Sacramento_dummies$type, pred_knn)

knn_results = kknn_fit$results # gives just the table of results by parameter
head(knn_results)

# group by k and distance function, create an aggregation by averaging
knn_results <- knn_results %>%
  group_by(kmax, kernel) %>%
  mutate(avgacc = mean(Accuracy))
head(knn_results)

# plot aggregated (over Minkowski power) accuracy per k, split by distance function
ggplot(knn_results, aes(x=kmax, y=Accuracy, color=kernel)) + 
  geom_point(size=3) + geom_line()

# The final values used for the model were kmax = 7, distance = 1(Manhattan) and kernel = cos.

# Problem 3 :

# Question 1:
library(cluster)
library(stats)
library(factoextra)
library(ggplot2)
library(tidyverse)
library(caret)

head(df)

# Select columns for clustering
df_wine <- select(df, -type)

#set.seed(123)
#preproc <- preProcess(df_wine, method=c("center", "scale"))
#df_wine <- predict(preproc, df_wine)

# Find the knee
fviz_nbclust(df_wine, kmeans, method = "wss")

# Comparing the average silhoutte scores 
fviz_nbclust(df_wine, kmeans, method = "silhouette")

# By seeing at both the knee plot, we cannot see a clear elbow but with silhouette method, we can see model will perform better at k=4. Hence we will choose k=4 to fit the model. 

# Fit the data using KMeans
fit <- kmeans(df_wine, centers = 4, nstart = 25)
fit

# Displaying the cluster plot
fviz_cluster(fit, data = df_wine)

# Question 2:

# Defining distance and linkage functions

distance_functions <- c("euclidean", "manhattan")
linkage_functions <- c("complete", "ward.D2")

# Using euclidean distances 
dist_mat_euclidean <- dist(df_wine, method = 'euclidean')

# Euclidean and Complete
hfit_euclidean_complete <- hclust(dist_mat_euclidean, method = 'complete')
# Build the new model
hac_euclidean_complete <- cutree(hfit_euclidean_complete, k=4)
# Displaying the cluster plot
fviz_cluster(list(data = df_wine, cluster = hac_euclidean_complete))

# Euclidean and Ward.D2
hfit_euclidean_ward <- hclust(dist_mat_euclidean, method = 'ward.D2')
# Build the new model
hac_euclidean_ward <- cutree(hfit_euclidean_ward, k=4)
# Displaying the cluster plot
fviz_cluster(list(data = df_wine, cluster = hac_euclidean_ward))


# Using manhattan distances
dist_mat_manhattan <- dist(df_wine, method = 'manhattan')

# Manhattan and Complete
hfit_manhattan_complete <- hclust(dist_mat_manhattan, method = 'complete')
# Build the new model
hac_manhattan_complete <- cutree(hfit_manhattan_complete, k=4)
# Displaying the cluster plot
fviz_cluster(list(data = df_wine, cluster = hac_manhattan_complete))

# Manhattan and Ward.D2
hfit_manhattan_ward <- hclust(dist_mat_manhattan, method = 'ward.D2')
# Build the new model
hac_manhattan_ward <- cutree(hfit_manhattan_ward, k=4)
# Displaying the cluster plot
fviz_cluster(list(data = df_wine, cluster = hac_manhattan_ward))

# Question 3:

# Create a dataframe
result_1 <- data.frame(Type = df$type, Kmeans = fit$cluster, hac_euclidean_complete = hac_euclidean_complete, hac_euclidean_ward = hac_euclidean_ward, hac_manhattan_complete = hac_manhattan_complete, hac_manhattan_ward = hac_manhattan_ward)

# Crosstab for K Means
result_1 %>% group_by(Kmeans) %>% select(Kmeans, Type) %>% table()

# Crosstabulation for hac_euclidean_complete
result_1 %>% group_by(hac_euclidean_complete) %>% select(hac_euclidean_complete, Type) %>% table()

# Crosstabulation for hac_euclidean_ward
result_1 %>% group_by(hac_euclidean_ward) %>% select(hac_euclidean_ward, Type) %>% table()

# Crosstabulation for hac_manhattan_complete
result_1 %>% group_by(hac_manhattan_complete) %>% select(hac_manhattan_complete, Type) %>% table()

# Crosstabulation for hac_manhattan_ward
result_1 %>% group_by(hac_manhattan_ward) %>% select(hac_manhattan_ward, Type) %>% table()

# Question 4:

rotated_data = data.frame(PC1=pc1, PC2=pc2)
rotated_data$Color <- df$type

# PCA plot to show type lable
ggplot(data = rotated_data, aes(x = PC1, y = PC2, col = Color)) + geom_point()

# PCA plot to show Kmeans cluster lable
rotated_data$Kmeans_Clusters = as.factor(fit$cluster)
# Plot and color by labels
ggplot(data = rotated_data, aes(x = PC1, y = PC2, col = Kmeans_Clusters)) + geom_point()

# PCA plot to show HAC cluster lable
rotated_data$HAC_Clusters = as.factor(hac_euclidean_ward)
# Plot and color by labels
ggplot(data = rotated_data, aes(x = PC1, y = PC2, col = HAC_Clusters)) + geom_point()


# Question 5:

# By looking at the cluster plots, we can see that, k-means shows better separation of clusters than HAC. But we can see a clear separation between two classes if we plot the data by type column in original dataset.

# By looking at the cross tabulation, 
# K-Means and HAC (Euclidean, Ward Linkage) seem to perform well, aligning closely with the original labels.
# HAC with Euclidean distance and Complete linkage formed one large cluster (Cluster 1). 
# HAC with Manhattan distance and both linkages shows misclassification in Cluster 4.

# Problem 4 :

library(dplyr)

# Read Star Wars data
starwars_data <- dplyr::starwars

dim(starwars_data)

# Removing variables (name, films, vehicles, starships)
starwars_data <- select(starwars_data, -c(name, films, vehicles, starships))

dim(starwars_data)

sapply(starwars, class)

starwars_data <- na.omit(starwars_data)



head(starwars_data)

# Converting categorical variables to factors
starwars_data <- starwars_data %>%
  mutate_if(is.character,as.factor)

sapply(starwars_data, class)

levels(starwars_data$species)

# Question 1:

library(cluster)
library(dendextend)
library(cluster)

# finding distances with the help of gower metric as we have categorical and numeric data
gower_dist <- daisy(starwars_data, metric = "gower")

summary(gower_dist)

#preproc_1 <- preProcess(starwars_data, method=c("center", "scale"))

#predictors_1 <- predict(preproc_1, starwars_data)

# Finding the optimum number of clusters with the help of below plots
# Knee plot
fviz_nbclust(starwars_data, FUN = hcut, method = "wss")

# silhouette method
fviz_nbclust(starwars_data, FUN = hcut, method = "silhouette")

# With the help of above plots, we will choose number of clusters = 6.

# fit the data using average linkage method as we have more number of clusters
hfit <- hclust(gower_dist, method = 'average')

# Build the new model
hac_gower_average <- cutree(hfit, k=6)

# Question 2:

# Build hierarchical agglomerative clustering using optimal_k_silhouette
hac_model <- hclust(gower_dist, method = "average")
dendrogram <- as.dendrogram(hac_model)

# Plot dendrogram
plot(dendrogram, main = "Dendrogram of Star Wars Characters")

# We can see that some of the characters does not seem to fit easily.

# Advantages:
# We can easily identify the patterns in the dendogram and we can decide the number of clusters by cutting the dendogram's vertical lines.
# Clustering algorithms can detect patterns and structures in data that are not visible when only looking at summary statistics.
# Anomalies are identified based on deviations from the established clusters, providing for a more detailed interpretation of new data points.
# Clustering methods are less sensitive to individual outliers compared to traditional methods.

# Disadvantages:
# The performance of clustering algorithms can be sensitive to the choice of parameters, such as the number of clusters (k) and the linkage method.
# Noisy data may be treated as clusters which can lead to missclassifications.

# Question 3:

starwars_data
sapply(starwars_data, class)
sapply(starwars_data, function(x) n_distinct(x))

starwars_data_dummies <- starwars_data %>%
  model.matrix(~ . - 1, data = .) %>%  # Create dummy variables for all variables (excluding intercept)
  as.data.frame()

dim(starwars_data_dummies)

# Center scale allows us to standardize the data 
#preproc <- preProcess(starwars_data_dummies, method=c("center", "scale"))

#predictors <- predict(preproc, starwars_data_dummies)

# Finding the optimum number of clusters with the help of silhouette method
fviz_nbclust(starwars_data_dummies, kmeans, method = "silhouette")

fviz_nbclust(starwars_data_dummies, kmeans, method = "wss")

# With the help of plots, we can take k=4 to fit the k means model.

# Fit the data with k-means
kmeans_4 <- kmeans(starwars_data_dummies, centers = 4, nstart = 25)
kmeans_4

# display the cluster plot
fviz_cluster(kmeans_4, data = starwars_data_dummies)

# Question 4:

# Create a dataframe
result_4 <- data.frame(Species = starwars_data$species, HAC = hac_gower_average, Kmeans = kmeans_4$cluster)

# Crosstabulation for HAC
result_4 %>% group_by(HAC) %>% select(HAC, Species) %>% table()

# Crosstab for K Means
result_4 %>% group_by(Kmeans) %>% select(Kmeans, Species) %>% table()

levels(starwars_data$species)

# By looking at the cross tabulation of K-means and HAC, we can say both the methods performed similar in terms of clustering the data. We can consider HAC performed slightly better as it has differentiated the data very well between all the classes.



