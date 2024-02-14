# ------------------------------------- Fundamentals of Data Science Assignment 1 ------------------------------------------------


getwd()

# Importing data in R

Data = read.csv("adult.csv", header = T)
Data

# Question a :

summary(Data)

summary(Data$age)

# For variable "age", we can see that the 1st Quartile value i.e. at 25th percenile is 28
# Value at 75th percentile is 48 and mean is 38.58. Magnitude of 25th percentile and 75th percentile is around 10 units away from the mean.
# Hence we can say by looking at summary of "age" variable, data is slightly normally distributed.
# Also there are no NA values in the data.

summary(Data$hours.per.week)

# For variable "hours.per.week", 1st Quartile value is 40, 3rd quartile value is 45 and mean is 40.44.
# As value at 75th percentile is more units away from mean than 25th percentile, we can guess that data is left skewed.
# No NA values present in the data.

library(psych)
describe(Data$hours.per.week)


# Question b :

# As both the variables are numeric, we can use scatterplot to compare them.

plot(x = Data$age,y = Data$hours.per.week, xlab = "Age", ylab = "Hours per week", main = "Age vs Hours per week")

# by looking at the scatter plot, we can conclude that there is no linear relation between the two variables.

hist(Data$age,xlab = "Age",col = "brown",border = "black")

hist(Data$hours.per.week,xlab = "Age",col = "brown",border = "black")

# By looking at the histograms, we can say our assumptions based on summary were wrong. Data for variable "age" is right skewed.
# For variable "hours.per.week", there are more data points near value = 40. Hence we cannot conlude about the normality of the data.

# Question c : 

library("dplyr")   
num_cols <- lapply(Data, is.numeric)
print(num_cols)

# With the help of lapply function, we found following numeric columns. 
age
fnlwgt
education.num
capital.gain
capital.loss
hours.per.week

# Creating scatterplot matrix for all numeric variables..

pairs(~age+fnlwgt+education.num+capital.gain+capital.loss+hours.per.week,data = Data,main = "Scatterplot for all variable")

# It will be more difficult if we create separate scatterplot for two variables.
# But if we create scatterplot matrix, we can compare linear relation between all numeric variables in a single plot.
# By looking at the plot, we can conclude there is no linear relation between any of the variable.

# Question d :

# We have already differentiated the categorical and numerical variables in below variable.
print(num_cols)

workclass
education
marital.status
occupation
relationship
race
sex
native.country
income.bracket

# 1. Use histograms to visualize the distribution


WorkClass <- table(Data$workclass)
prop.table(WorkClass)


# Save the proportion table in an object 
workclass.ptb <- prop.table(WorkClass)

# Convert the proportion table to a data frame
WorkClass.df <- as.data.frame(workclass.ptb)

# Converting to a data frame lost the names
names(WorkClass.df) <- c("Class", "Percent")

library(scales)
library(ggplot2)

# Use the proportion table as the data frame in a call to ggplot()
ggplot(data=WorkClass.df, mapping=aes(x=Class, y=Percent)) + geom_col() + scale_y_continuous(label=percent)



Data$workclass %>% count(species)

count(Data$workclass)

library(ggplot2)
Data %>% group_by(sex) %>% count()
Data %>% ggplot(aes(x = sex)) + geom_bar() + labs(title = "Bar chart for Sex")


# Question e :

cross_tabulation <- table(Data$education,Data$income.bracket)
cross_tabulation

# As per the cross tabulation, we can see that people with Bachelors degree have most number of records which fall in >50K category.
# At education level "Preschool", there are 0 records who fall under >50K category.
# There are highest number of records with education level "HS-grad"

# Use a heatmap for visualization
heatmap(cross_tabulation,, main = "Cross Tabulation Heatmap")




# Problem 2 -----

# Question 1 :

Even_Data = read.csv("population_even.csv", header = T)
Odd_Data = read.csv("population_odd.csv", header = T)

df = merge(x = Even_Data, y = Odd_Data, by = "STATE")#,ll.x = TRUE)
df

# We joined data based on common variable named "STATE".


# Question 2 :

str(df)

a)
colnames(df)
# There is no duplicate "STATE" column created in the joining process.

df2 <- within(df, rm(NAME.y))
colnames(df2)

b)
df <- df %>% rename("2010" = "POPESTIMATE2010",
                                        "2011" = "POPESTIMATE2011",
                                        "2012" = "POPESTIMATE2012",
                                        "2013" = "POPESTIMATE2013",
                                        "2014" = "POPESTIMATE2014",
                                        "2015" = "POPESTIMATE2015",
                                        "2016" = "POPESTIMATE2016",
                                        "2017" = "POPESTIMATE2017",
                                        "2018" = "POPESTIMATE2018",
                                        "2019" = "POPESTIMATE2019"
)
colnames(df)

c)
# Reordering data by Year

df<-df %>% select(order(colnames(df)))
df


# Problem 3 

# Reading old data again as df$2011 not working as it is taking as Numeric Value.

df = merge(x = Even_Data, y = Odd_Data, by = "STATE",ll.x = TRUE)

sum(is.na(df)) 
which(is.na(df))

# Finding which column has the missing values.
sapply(df, function(x) sum(is.na(x)))


# Finding which state has NA value in a respective column.
df$NAME.x[is.na(df$POPESTIMATE2011)]

# Replacing NA values for Column "POPESTIMATE2011" :
df$POPESTIMATE2011[is.na(df$POPESTIMATE2011)]<-mean(c(df$POPESTIMATE2010[df$NAME.x==df$NAME.x[is.na(df$POPESTIMATE2011)]],df$POPESTIMATE2012[df$NAME.x==df$NAME.x[is.na(df$POPESTIMATE2011)]]))
df

# Replacing NA values for Column "POPESTIMATE2013" : 
df$POPESTIMATE2013[is.na(df$POPESTIMATE2013)]<-mean(c(df$POPESTIMATE2012[df$NAME.x==df$NAME.x[is.na(df$POPESTIMATE2013)]],df$POPESTIMATE2014[df$NAME.x==df$NAME.x[is.na(df$POPESTIMATE2013)]]))
df

# Replacing NA values for Column "POPESTIMATE2015" : 
df$POPESTIMATE2015[is.na(df$POPESTIMATE2015)]<-mean(c(df$POPESTIMATE2014[df$NAME.x==df$NAME.x[is.na(df$POPESTIMATE2015)]],df$POPESTIMATE2016[df$NAME.x==df$NAME.x[is.na(df$POPESTIMATE2015)]]))
df

# Replacing NA values for Column "POPESTIMATE2017" : 
df$POPESTIMATE2017[is.na(df$POPESTIMATE2017)]<-mean(c(df$POPESTIMATE2016[df$NAME.x==df$NAME.x[is.na(df$POPESTIMATE2017)]],df$POPESTIMATE2018[df$NAME.x==df$NAME.x[is.na(df$POPESTIMATE2017)]]))
df

# Replacing NA values for Column "POPESTIMATE2019" : 
df$POPESTIMATE2019[is.na(df$POPESTIMATE2019)]<-mean(c(df$POPESTIMATE2018[df$NAME.x==df$NAME.x[is.na(df$POPESTIMATE2019)]],df$POPESTIMATE2020[df$NAME.x==df$NAME.x[is.na(df$POPESTIMATE2019)]]))
df

# Checking if all missing values replaced or not.
sapply(df, function(x) sum(is.na(x)))

# All missing values replaced as given.


# Question 4 :

df <- df %>% rename("2010" = "POPESTIMATE2010",
                    "2011" = "POPESTIMATE2011",
                    "2012" = "POPESTIMATE2012",
                    "2013" = "POPESTIMATE2013",
                    "2014" = "POPESTIMATE2014",
                    "2015" = "POPESTIMATE2015",
                    "2016" = "POPESTIMATE2016",
                    "2017" = "POPESTIMATE2017",
                    "2018" = "POPESTIMATE2018",
                    "2019" = "POPESTIMATE2019"
)
colnames(df)


# Get the maximum population for a single year for each state
max_population_year <- df %>% rowwise() %>% mutate(Max_Population = max(c_across(starts_with("20"))))
head(max_population_year)

# To get the sum of population statewise
total_population_year <- df %>% rowwise() %>% mutate(total_population = sum(c_across(starts_with("20"))))
head(total_population_year)

# Refer last column to see max population and total population state wise. We just replaced max function by sum function to get sum of the population. 

# Question 5:

# Get the total US population for one single year
total_us_population_2010 <- sum(df$POPESTIMATE2010)
total_us_population_2010


# Problem 3 ----------------

library(tidyverse)

df <- df %>% rename("2010" = "POPESTIMATE2010",
                    "2011" = "POPESTIMATE2011",
                    "2012" = "POPESTIMATE2012",
                    "2013" = "POPESTIMATE2013",
                    "2014" = "POPESTIMATE2014",
                    "2015" = "POPESTIMATE2015",
                    "2016" = "POPESTIMATE2016",
                    "2017" = "POPESTIMATE2017",
                    "2018" = "POPESTIMATE2018",
                    "2019" = "POPESTIMATE2019"
)
colnames(df)
df


# Reshape the data to have year and population columns
df_new <- df %>%
  pivot_longer(cols = starts_with("20"), names_to = "year", values_to = "population") %>%
  mutate(year = as.integer(str_extract(year, "\\d+")))  # Convert year to integer

# Choosing 3 states 

selected_states <- c("Arizona", "California", "Illinois")

# Filter data for selected states
df_states <- df_new %>% filter(NAME.x %in% selected_states)

library(ggplot2)

# Create a line graph
ggplot(df_states, aes(x = year, y = population, color = NAME.x)) +
  geom_line() +
  labs(title = "Population Vs Time for 3 States",
       x = "Year",
       y = "Population") 

df_states$year



# Prolem 4 -------------------

Question a. Describe two ways in which data can be dirty, and for each one, provide a potential solution.

Ans ->

1. Missing values
Issue - The accuracy of the analysis can be significantly affected by missing values since they can result in incorrect conclusions about the data. 
Solution - We can use imputation methods such as mean imputation, KNN Imputation, Forward fill etc.
           Also if the number of rows impacted by NA are less than 2% of overall rows, we can simply remove those rows from data which are having NA values.

2. Outliers 
Issue - The outcomes of statistical modelling and data analysis might be affected by outliers. 
        The mean and the standard deviation may be significantly impacted. 
        Normality may be reduced if the outliers aren't distributed randomly.
Solution - First detect outliers by following methods.
          i) IQR method
          ii) Standard deviation method
          There are several approaches to handle outliers, such as capping, removing them or by treating them as missing values.
           
# Question b :

i) We can use clustering algorithms to group customers who buy similar things. We can use K-Means clustering, KNN etc.

ii) Binary outcomes, such whether a consumer would purchase milk, can be predicted using classification algorithms like decision trees, random forest or logistic regression. 
  
iii) 

# Question c : 

a. Organizing the customers of a company according to education level. 
-> Yes this is a data mining task as we are categorizing customers based on some of the attributes such as education level. We can pull out otcomes from it such as which education level has more number of customers etc.


b. Computing the total sales of a company. 
-> No, this is not a data mining task as it doesn't involve discovering patterns or relationships in the data.

c. Sorting a student database according to identification numbers. 
-> We cannot say this is a data mining task as its just ordering our data based on some of the attributes. We are not discovering any patterns here,

d. Predicting the outcomes of tossing a (fair) pair of dice. 
-> It is a process of predicting something but we cannot say this is a data mining process as we are not discovering any relationships or patterns here. We are just prediting some of the outcomes.

e. Predicting the future stock price of a company using historical records.
-> Yes, this is a data mining process as we need to analyse the historical data first, we need a lot of preprocessing of the data if required. Also we need to find patterns and relations between variables to decide if we can keep those variables or not. And then we can predict the future stock price.







----------------------



a. Dirty Data and Potential Solutions:
  
  Missing Values:
  
  Issue: Missing values can lead to biased or incomplete analysis.
Solution: Imputation methods, such as mean imputation, forward-fill, or advanced methods like k-Nearest Neighbors (KNN) imputation, can be used to fill in missing values.
Outliers:
  
  Issue: Outliers can skew statistical measures and affect model performance.
Solution: Identify and handle outliers, either by removing them if they are errors or transforming them if they represent valid but extreme observations.
b. Data Mining Functionalities:
  
  Clustering:
  
  Question: What are five groups of customers who buy similar things?
  Functionality: Clustering can help group customers with similar purchase patterns, allowing businesses to tailor marketing strategies to specific customer segments.
Classification:
  
  Question: Can I predict if a customer will buy milk based on what else they bought?
  Functionality: Classification algorithms, such as decision trees or logistic regression, can predict binary outcomes, like whether a customer will buy milk based on their purchase history.
Association Rule Mining:
  
  Question: What are different sets of products that are often purchased together?
  Functionality: Association rule mining, like Apriori algorithm, can identify sets of products frequently purchased together, informing product placement or promotions.
c. Data Mining Tasks:
  
  Organizing Customers by Education Level:
  
  Task: Yes, this is a data mining task.
Explanation: This involves categorizing customers based on a specific attribute (education level), which is a common task in data mining, such as clustering or classification.
Computing Total Sales of a Company:
  
  Task: No, this is not a typical data mining task.
Explanation: It is a straightforward aggregation task that doesn't involve discovering patterns or relationships in the data.
Sorting a Student Database by Identification Numbers:

Task: No, this is not a data mining task.
Explanation: Sorting is a basic database operation, and data mining typically involves discovering patterns or extracting knowledge from data.
Predicting Outcomes of Tossing a Pair of Dice:

Task: No, this is not a data mining task.
Explanation: It is a deterministic process, and data mining is more about discovering patterns in large, complex datasets.
Predicting Future Stock Prices Using Historical Records:

Task: Yes, this is a data mining task.
Explanation: Predicting stock prices involves analyzing historical data patterns to make predictions about future trends, which is a core aspect of data mining.

            
            
mean(c(18, 8, 22, 10, 12, 5, 4, 32, 2, 9, 16, 25, 26, 28))            
            

