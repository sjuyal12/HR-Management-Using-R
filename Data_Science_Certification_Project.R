                             # Data Science 2.0 Certification Project

# Set the current working directory
setwd("C:\\softwares\\R_Datasets")

# Load the data into R
Employee_Data <- read.csv("338_cert_proj_datasets_v3.0.csv")

# Basic details of Employees_dataset of Organisation using R functions

# View the dataset
View(Employee_Data)

#Dimensions of Dataset
dim(Employee_Data)

#Class of the dataset
class(Employee_Data)

# Structure of the Employee dataset
str(Employee_Data)

# Levels of dataset
levels(Employee_Data)

# Number of rows and columns of dataset
nrow(Employee_Data)
ncol(Employee_Data)

# Overall summary of Employee dataset
summary(Employee_Data)

# Find the correlation values of the attributes of our data

# Finding correlation between attributes helps us to get the strong relation between any two variables
# We converting attributes from int to numeric since cor function working on numeric data
# Note: We are not calculating correlation of DEPARTMENT AND SALARY attributes since they are factors so
# there is no use

Employee_Data_Cor <- Employee_Data[,(1:8)] # Ignoring last two factor variables department and Salary

Employee_Data_Cor$number_project <- as.numeric(Employee_Data_Cor$number_project)
Employee_Data_Cor$average_montly_hours <- as.numeric(Employee_Data_Cor$average_montly_hours)
Employee_Data_Cor$time_spend_company <- as.numeric(Employee_Data_Cor$time_spend_company)
Employee_Data_Cor$Work_accident <- as.numeric(Employee_Data_Cor$Work_accident)
Employee_Data_Cor$left <- as.numeric(Employee_Data_Cor$left)
Employee_Data_Cor$promotion_last_5years <- as.numeric(Employee_Data_Cor$promotion_last_5years)

head(Employee_Data_Cor) # Head function will return the first 6 rows of dataframe

# Find the Correlation
cr <- cor(Employee_Data_Cor)
library("corrplot")

# Correlation Value
corrplot(cr)

# Corrgrm function is used to see the  relation visualization with  color 
# value blue value gives positive relation ,red indicates negative relation and close 
# to white means neutral,find the screenshot below

library(corrgram)
corrgram(Employee_Data_Cor)

# Employee data only with left the company

Employee_Data_Left <- subset(Employee_Data_Cor,left ==1) # Filtering data with only left employees
summary(Employee_Data_Left)

# Finding the correlation values on the dataset contains where employees left the company

cr1 <- cor(Employee_Data_Left)
library("corrplot")

# Correlation Value
corrplot(cr1)
library(corrgram)
corrgram(Employee_Data_Left)

# Visualizing the characteristics of the whole data and only the people who left, 
# by using the plots and histograms

Employee_Data_Hist <- subset(Employee_Data, left == 1) # Filtering Overall data and people only who left 
summary(Employee_Data_Hist)
nrow(Employee_Data_Hist)

par(mfrow=c(1,2))

# Using the HISTOGRAMS

hist(Employee_Data_Hist$satisfaction_level,col= "green",breaks= 40,main = "Satisfaction Level")
# From Satisfaction level plot we can concluded that average satisfaction level is 0.45 to 0.55
# for the employees who  left the company

hist(Employee_Data_Hist$last_evaluation,col= "green",breaks= 30,main = "Last Evaluation")
# From Last Evaluation plot We can conclude that 50% employees having 0.5 in the last evaluation

hist(Employee_Data_Hist$number_project,col= "green",breaks= 30,main = "Number of Project")
# From Number of Project plot we noticed that the most of the  employees who left company did 
# only 2 projects

hist(Employee_Data_Hist$average_montly_hours,col= "green",breaks= 30,main = "Averae Monthly Hours")
# From Average Monthly Hours plot We can conclude that 50% employees who left the company
# spending 200 - 220 hours per month in the office

hist(Employee_Data_Hist$time_spend_company,col= "green",breaks= 30,main = "time spent in company")
# From above Time spend company plot we can concluded that 40% employees who are leaving 
# spending only 3hours.

hist(Employee_Data_Hist$Work_accident,col= "green",breaks= 30,main = "work accident")
# From work accident plot  we can conclude that 99% employees not having any work accident

hist(Employee_Data_Hist$promotion_last_5years,col= "green",breaks= 30,main = "promotion in last 5 years")
# From Promotion of last 5 years plot we can conclude that there is promotion for last 5 years 
# for around 99% employees who left the company

plot(Employee_Data_Hist$salary,col= "green",breaks= 100,main = "salary")

# Evaluating the values of each attributes for both left and non-left employees

Employee_Data_Left_Evaluate <- subset(Employee_Data,left ==1)
Employee_Data_NotLeft_Evaluate <- subset(Employee_Data,left ==0)

# Evaluation of attributes who left the organisation/company

hist(Employee_Data_Left_Evaluate$satisfaction_level,col= "green",
     breaks= 40,main = "Satisfaction Level") 
# Average satisfaction level is between 0.45 to 0.55 who left the company

hist(Employee_Data_Left_Evaluate$last_evaluation,col= "green",
     breaks= 30,main = "Last Evaluation") 
# Maximum employees who left the company having last evaluation of 0.6

hist(Employee_Data_Left_Evaluate$number_project,col= "green",
     breaks= 30,main = "Number of Project")
# Most of the employees who left the company did only 2 projects

hist(Employee_Data_Left_Evaluate$average_montly_hours,col= "green",
     breaks= 30,main = "Averae Monthly Hours") 
# Average monthly hours of employees who left is around 220hours

hist(Employee_Data_Left_Evaluate$time_spend_company,col= "green",
     breaks= 30,main = "time spent in company") 
# Maximum employees who left company spend around 3 to 4 hours on average per day

hist(Employee_Data_Left_Evaluate$Work_accident,col= "green",
     breaks= 30,main = "work accident")
# 98% there is no work accident in the who left company

hist(Employee_Data_Left_Evaluate$promotion_last_5years,
     col= "green",breaks= 30,main = "promotion in last 5 years")
# 99% Employees did not receive promotion over past 5 years who left the company

plot(Employee_Data_Left_Evaluate$salary,col= "green",
     breaks= 100,main = "salary") 
# Maximum employees getting low level salary and less than 1% employees who left company 
# got high salary

# Applying Table function on each attribute by filtering data who left the company

table(Employee_Data_Left_Evaluate$satisfaction_level) 
# Majority employees having less satisfaction who left the company

table(Employee_Data_Left_Evaluate$left)
# 3571 employees left the company

table(Employee_Data_Left_Evaluate$number_project)
# Those who have worked in less number of projects (2) have left the company most.

table(Employee_Data_Left_Evaluate$average_montly_hours)
# Maximum employees working more hours in the project and Those who clocked less number of 
# hours have left the company most.

table(Employee_Data_Left_Evaluate$promotion_last_5years)
# 99 % employees who left the company did not receive promotion from past 5 years

table(Employee_Data_Left_Evaluate$salary) 
# Only 5 - 10 % employess exactly 82 employees only got high salary who left the company.
# Majority employees got low and medium level salary.

table(Employee_Data_Left_Evaluate$time_spend_company)
# 50% employees who left the company spending 4-5 hours  a day


# Evaluation of each attributes who are not left the organisation/company

hist(Employee_Data_NotLeft_Evaluate$satisfaction_level,col= "green",
     breaks= 40,main = "Satisfaction Level")
# Maximum employees who are not leaving company having satisfaction level an average of 0.7 and above

hist(Employee_Data_NotLeft_Evaluate$last_evaluation,col= "green",
     breaks= 30,main = "Last Evaluation")
# Last evaluation of employees who are not leaving an average of 0.7 and above

hist(Employee_Data_NotLeft_Evaluate$number_project,col= "green",
     breaks= 30,main = "Number of Project")
# Maximum employees who are not leaving company did 3 to 4 projects

hist(Employee_Data_NotLeft_Evaluate$average_montly_hours,col= "green",
     breaks= 30,main = "Averae Monthly Hours") 
# Average monthly hours of employees is about 200hours and above who are not leaving the company

hist(Employee_Data_NotLeft_Evaluate$time_spend_company,
     col= "green",breaks= 30,main = "time spent in company")
# Most of the Employess spending 3hours a day in Office who are not leaving company

hist(Employee_Data_NotLeft_Evaluate$Work_accident,col= "green",
     breaks= 30,main = "work accident")
# 90% there is no work accident for the employees who are not leaving the company

hist(Employee_Data_NotLeft_Evaluate$promotion_last_5years,
     col= "green",breaks= 30,main = "promotion in last 5 years") 
# 97% there is no promotion in last 5 years

plot(Employee_Data_NotLeft_Evaluate$salary,col= "green",
     breaks= 100,main = "salary")
# 50% employees are getting medium level salary, 15% employees getting High level salary

# Table function on each attribute by filtering data on which employees not leaving company

table(Employee_Data_NotLeft_Evaluate$satisfaction_level) 
# Majority employees having More satisfaction level who are not leaving the company

table(Employee_Data_NotLeft_Evaluate$left)
# 11428 employees not left the company

table(Employee_Data_NotLeft_Evaluate$number_project) 
# Approximately 65% employees working average of 3 to 4 projects

table(Employee_Data_NotLeft_Evaluate$average_montly_hours)
# Maximum employees working an average of 200hours monthly in the Company.

table(Employee_Data_NotLeft_Evaluate$promotion_last_5years)
# 99 % employees who are not leaving company does not get the promotion over the past 5 years 
# and it is similar to employees who left the company

table(Employee_Data_NotLeft_Evaluate$salary)
# 10 % employess exactly 1155 employees only got high salary and 50% employees got medium level salary

table(Employee_Data_NotLeft_Evaluate$time_spend_company)
# 50% employees who are not leaving the company spending more than 4hrs a day

# Applying Summary function on employees who left the company

summary(Employee_Data_Left_Evaluate)

# From summary results we can conclude that regarding employees leaving company: 
# Average satisfaction level is 0.44 and average monthly hours of each employee is 207 hours, 
# Time spending in a company for each day is about 3.877 hours and salary is very low for 2172
# employees and Promotion less almost less 0.005% for past 5 years

# Applying Summary function on employees who are not leaving the company
summary(Employee_Data_NotLeft_Evaluate)

# From summary results we can conclude that regarding employees who are not leaving the company: 
# Satisfaction level of employees who are in company is 0.66 and number of projects did by each 
# employees on average of 4 projects. Apart from this Salary is medium for 50% employees and 
# 10% got high salary that are in the company.

# Analyse the department wise turn outs and find out the percentage of employees 
# leaving from each department


Employee_Data_Agg_Left <- subset(Employee_Data,left == 1)
Employee_Data_Agg_NotLeft <- subset(Employee_Data, left == 0)

Left_Each_department <- table(Employee_Data_Agg_Left$department)
Left_Each_department

# The maximum employees leaving from sales department(1014) and followed by Technical department(697)
# and least is from Management department (91)

Total_Department <- table(Employee_Data$department)
Total_Department
# Maxmimum Employees are in Sales department (4140) and least employees in Management Department(630)

# % Calculation of Employees who left the company from each department
Percentage <- (Left_Each_department/Total_Department)*100
Percentage

# But regarding percentage wise employees leaving each department, 29.09% employees leaving from HR department and followed 
# by 26.59% employees leaving from Accounting department and least % employees leaving from 
# Management department that is around 14.44%

#Build models using Decision Tree, Random Forest, Naïve Bayes and SVM techniques 
#and find out the most accurate one

                           #DECISION TREE MODEL

Employee_Data$left <- as.factor(Employee_Data$left)
set.seed(3)
splitting_Data <- sample(2, nrow(Employee_Data), prob = c(0.7,0.3), replace = T)
Training_Data <- Employee_Data[splitting_Data==1,]
Testing_Data <- Employee_Data[splitting_Data==2,]
# The above steps indicates that splitting data into training and test data sets in 
# such a way that 70% data in training and 30% data in test data set to predict the results

library(rpart)

Decision_Tree_Model <- rpart(left ~ . , data = Training_Data)
Decision_Tree_Model
summary(Decision_Tree_Model)

# Predict the Decision Tress results
Predict_Decision_Tree <- predict(Decision_Tree_Model, newdata = Testing_Data,type = "class")

Predict_Decision_Tree

# Confusion Matrix

library(caret)
confusionMatrix(table(Predict_Decision_Tree, Testing_Data$left))
# From the above Confusion matrix results which are applied on the decision tree model, 
# the accuracy of the model is 96.99%.

                                 # RANDOM FOREST MODEL

library(randomForest)
Decision_Random_Model <- randomForest(left ~ . ,data = Training_Data, ntree = 20)
Decision_Random_Model

summary(Decision_Random_Model)

# Predict the results

Predict_Random_Forest <- predict(Decision_Random_Model,newdata = Testing_Data)
Predict_Random_Forest

#confusion Matrix

library(caret)
confusionMatrix(table(Predict_Random_Forest,Testing_Data$left))
# From the above Confusion matrix results which are applied on the Random forest model, 
# the accuracy of the model is 99.03%.

# Importance Function
importance(Decision_Random_Model) # Extract variable important measure

                           # Naive Bayes Classifier Model 

library(e1071)

Naive_Bayes_Model <- naiveBayes(left ~ . , data = Training_Data)
Naive_Bayes_Model

# Predict the Test Results

Predict_Naive_Bayes <- predict(Naive_Bayes_Model,newdata = Testing_Data,type= "class")
Predict_Naive_Bayes

# Confustion Matrix

library(caret)
confusionMatrix(table(Predict_Naive_Bayes,Testing_Data$left))
# From the above Confusion matrix results which are applied on the Naive Bayes classifier model,
# the accuracy of the model is 78.63%.

                         # Building a Support Vector Machine (SVM)

SVM_Model <- svm(left  ~ ., data = Training_Data,kernel = "linear",cost = 0.1,scale = F)
SVM_Model

# Predict the values using Predict function

Predict_SVM <- predict(SVM_Model,Testing_Data,type = "class")
Predict_SVM

table(Predict_SVM,Testing_Data$left)
mean(Predict_SVM==Testing_Data$left)


# Confustion Matrix

library(caret)
confusionMatrix(table(Predict_SVM,Testing_Data$left))
# From the above Confusion matrix results which are applied on the Support vector machine model,
# the accuracy of the model is 79.01%.
summary(SVM_Model)

# After building all the Classification models on the Employees data set, Random Forest model 
# is the most accurate classification algorithm and which is predict the results with accuracy of 
# 99.03% and followed by Decision trees with accuracy of 96.03% and SVM of 79.01%. The least 
# accuracy one is Naive bayes with an accuracy of 78.63%. 

# From these results we can conclude that RANDOM FOREST is the best model with most accuracy 
# model which is suitable to the employee's dataset.

# RANDOM FOREST is the best model to forecast and predict the employees who are leaving the company.


                                 # CONCLUSION #

# I have loaded the Dataset in to R and applied R code on the Employees dataset and build the 
# classification models on the employees' dataset and evaluated the each attribute/column of complete 
# dataset by filtering employees who left the company and employees who are not leaving the company. 
# As per VP request after evaluating each attribute who left company the following are reasons for 
# the best and most experienced employees are leaving prematurely.

# 1.	From the dataset 3571 employees left the company till now.
# 2.	Majority of the employees who left the company having less satisfaction level and with an average of 0.45 to 0.55.
# 3.	There is no work accident regarding employees who left the company and noticed only 2% work accidents occurred.
# 4.	Majority of the employees who left the company getting low level salary  and medium level salary and only 5% employees exactly 82 getting high level salary.
# 5.	Those who have worked in less number of projects (2) in the company have left the company most.
# 6.	Those who clocked less number of hours have left the company most.
# 7.	Average monthly hours of employees who left the company is around 220hours.
# 8.	As per Evaluation attribute 50% employees got 0.5 evaluation only those who left the company.
# 9.	Maximum employees who left company spend around 3 to 4 hours on average per day.
# 10.	99% Employees did not receive promotion over past 5 years who left the company.
# 11.	50% employees who left the company spending 4-5 hours a day.
# 12.	We can conclude that the maximum employees leaving from sales department (1014) and followed by Technical department (697) and least is from Management department (91).
# 13.	Regarding employees leaving each department in terms of percentages, 29.09% employees leaving from HR department and followed by 26.59% employees leaving from Accounting department and least % employees leaving from Management department that is around 14.44%.
  
  
  











