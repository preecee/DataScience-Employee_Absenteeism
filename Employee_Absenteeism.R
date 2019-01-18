##--------------------------------------------------------------------------------------------------------------------------##
#### Project Assignment by Preeti Chauhan ##
##This R code is for project : Employee Absenteeism
##The objective of this Case is Prediction of losses due to employee absenteeism in 2011
##--------------------------------------------------------------------------------------------------------------------------##

#/*I am going to divide whole project in to 8 parts:
# --->  1.) Define and categorize problem statement
# --->  2.) Gather the data
# --->  3.) Prepare data for consumption
# --->  4.) Perform Exploratory Data Analysis
# --->  5.) Models Building
# --->  6.) Evaluate and compare Model performances and choose the best model
# --->  7.) Hypertune the selected model
# --->  8.) Produce sample output with tuned model*/

#-----------------------------------------------------------------------------------------------------------------------------------##

## ----------- Part 1: Define and categorize the problem statement --------------
#### The problem statement is to "Predict the losses due to employee absenteeism in 2011"
##### This is clearly a 'Supervised machine learning regression problem' to predict a number based on the input features
## ----------- Part 1 ends here ----------------- 
##-------------------------------------------------------------------------------------------------------------------------------------------------------------------#

##------------- Import all the required libraries--------------

library(readxl)

#---- for model building
library(caret)
library(randomForest)
library(gbm)

#---- for visualization---
library(ggplot2)
library(corrgram) 
#------ for model evaluation -----

#---- All the required libraried imported-----------------
##-------------------------------------------------------------------------------------------------------------------------------------------------------------------#

## ------------------- Part 2: Gather the data -----------------
# set the working directory
set.seed(1)
setwd('D:/DataScience_Edwisor/edWisor/Projects/Employee_Absenteeism/')
### Here data is provided as .csv file with the problem.
### Let's import the data 
emp_absntsm <- read_xls("D:/DataScience_Edwisor/edWisor/Projects/Employee_Absenteesim/Dataset/Absenteeism_at_work_Project.xls")
head(emp_absntsm)

#----- Let's rename column for ease of operation
names(emp_absntsm) <- c('ID','Absence_Reason','Absence_Month','Absence_Day','Seasons','Transportation_Expense','Work_Distance','Service_Time','Age','Average_Workload','Hit_Target','Disciplinary_Failure','Education','Son','Drinker','Smoker','Pet','Weight','Height','BMI','Absent_Hours')

categorical_var <- c('Absence_Reason','Absence_Month','Absence_Day','Seasons','Disciplinary_Failure','Education','Son','Drinker','Smoker','Pet')
continous_var <- c('ID','Transportation_Expense','Work_Distance','Service_Time','Age','Average_Workload','Hit_Target','Weight','Height','BMI')
target_var <- c('Absent_Hours')
##---------- Part 2 ends here --------------------------
##-------------------------------------------------------------------------------------------------------------------------------------------------------------------#

# ------------Part 3 : Prepare the data for consumption(Data Cleaning) ---------------
#### 3a.) Check the shape/properties of the data
#### 3b.) Completing -- Perform missing value analysis and impute missing values if necessary
#### 3c.) Correcting -- Check for any invalid data inputs , for outliers or for any out of place data
#### 3d.) Creating -- Feature extraction . Extract any new features from existing features if required
#### 3e.) Converting -- Converting data to proper formats
#-------------------------------------------------------------------------------------
#### --------3a.) Check the shape/properties of the data
## Check the shape of the data
dim(emp_absntsm)
# what we can infer:
## ->the dataset has 740 observations and 21 features


## Check the properties of the data
str(emp_absntsm)
summary(emp_absntsm)
# what we can infer:
# ->There are null values in the dataset. We need to handle null values
# -> The datatypes are int,num and factor

final_col = c('Transportation_Expense','Work_Distance','Service_Time','Age','BMI','Drinker','Smoker','Height','Weight','Pet','Son','Education','Disciplinary_Failure','Hit_Target')

#### ------------------3b.) Correcting -- Check for any invalid data inputs 
# From above observations data doesnot seem to have any invalid datatypes to be handled

# However feature 'Absence_Month' have an imvalid value 0. Lets drop it.
# ALso, as we can see, 'Absent_Hours' are 0 in some places.
# This could be result of cancelled or withdrwan leaves. Lets drop these

emp_absntsm = emp_absntsm[which(emp_absntsm$Absent_Hours > 0),]
emp_absntsm = emp_absntsm[which(!is.null(emp_absntsm$Absence_Month)) & !(emp_absntsm$Absence_Month == 0),] 

# --------- 3c.) Completing -- Perform missing value analysis and impute missing values if necessary
#Checking nulls
len <- nrow(emp_absntsm)
(sapply(emp_absntsm, function(x) sum(is.na(x)))/len)*100
# what we can infer:
# ->There are  null values in almost all the columns of the dataset, although in small amount
#----drop rows with missing values and Nas -------

emp_absntsm = emp_absntsm[!(rowSums(is.na(emp_absntsm))),]

#### 3d.) ------- Converting -- Converting data to proper formats
# features like 'Absence_Month','Education' are categories here. Lets convert to categories
categoryFeatureList = c('Absence_Reason','Absence_Month','Absence_Day','Seasons','Disciplinary_Failure','Education','Son','Drinker','Smoker','Pet')
continousFeatureList = c('ID','Transportation_Expense','Work_Distance','Service_Time','Age','Average_Workload','Hit_Target','Weight','Height','BMI')
target_var = c('Absent_Hours')
emp_absntsm[,categoryFeatureList] <- data.frame(apply(emp_absntsm[categoryFeatureList], 2, as.factor))
str(emp_absntsm)

# Let's check for the outliers in EDA step

# ------------Part 3 : Prepare the data for consumption(Data Cleaning) ENDS here---------------
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------

# ------------Part 4 : Exploratory Data Analysis(EDA) STARTS here -----------

#----- 4 a.) Outlier Analysis -----------
## -- Lets do the outlier analysis ----
## -- Visualize continous variables and 
##  count with respect to categorical variables with boxplots ---




ggplot(aes_string(y = "Absent_Hours"), data = emp_absntsm) + geom_boxplot()
ggplot(aes_string(y = "Transportation_Expense"), data = emp_absntsm) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "Work_Distance"), data = emp_absntsm) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "Service_Time"), data = emp_absntsm) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "Age"), data = emp_absntsm) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "Average_Workload"), data = emp_absntsm) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "Hit_Target"), data = emp_absntsm) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "Weight"), data = emp_absntsm) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "Height"), data = emp_absntsm) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "BMI"), data = emp_absntsm) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)

ggplot(aes_string(y = "Absent_Hours", x = "Absence_Reason"), data = emp_absntsm) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "Absent_Hours", x = "Absence_Month"), data = emp_absntsm) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "Absent_Hours", x = "Absence_Day"), data = emp_absntsm) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "Absent_Hours", x = "Seasons"), data = emp_absntsm) + geom_boxplot(outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "Absent_Hours", x = "Disciplinary_Failure"), data = emp_absntsm) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "Absent_Hours", x = "Education"), data = emp_absntsm) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "Absent_Hours", x = "Son"), data = emp_absntsm) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "Absent_Hours", x = "Drinker"), data = emp_absntsm) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "Absent_Hours", x = "Pet"), data = emp_absntsm) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)

#checking the outliers values for 'Absent_Hours'
boxplot.stats(emp_absntsm$Absent_Hours)$out

# what we can infer from above boxplots:
# --> Target feature 'Absent_hours', has many outliers. It needs to be handled( will handle it after exploratory analysis)
# -> Not many outliers in independent features. Data seems balanced.

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------

#---- 4b.) Correlation Analysis
#--- Explore continous features
#--- Explore categorical features

###---calculating correlation of each continous variable with target variable 'Absent_hours'
PearsonCoef = cor(x = emp_absntsm[continousFeatureList],y=emp_absntsm$Absent_Hours,method = 'pearson')
cor_df <- data.frame(PearsonCoef)
cor_df$Variables <- rownames(cor_df)

##-----plotting bargraph for correlation analysis --------------
corr_barplot <-ggplot(data=cor_df, aes(x=Variables, y=PearsonCoef)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=PearsonCoef), vjust=-0.35, color="black", size=3)+
  theme_minimal()
corr_barplot

## from the above correlation plot, it is evident that 'Work_Distance','Transportation_Expense','Age' have good correlation with target variable 'Absent_Hours'.

###------heatmap for correlation matrix---------##
##to check multicollinearity ---##
corrgram(emp_absntsm, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="Absenteeism Correlation Data")

# This shows that there is multicollinearity in the dataset. BMI and Weight are highly correlated. 'Service_Time' and 'Age' are also correlated
#Will have to deal with multi collinearity by removing few features from the dataset.

#Should we do ANOVA test for correlation btw categorical variable and target continous variables. Let's see'
#First explore the relationships through more visualizations

#---- Visualizing PAIRPLOTS---
pairs(emp_absntsm[,continousFeatureList])
# evident from this 'temp' and 'atemp' are highly correlated. One needs to be dropped.

#---------------- Check distribution of target variables ---------------

d <- density(emp_absntsm$Absent_Hours)
plot(d, main="Kernel Density of Total Hours of Absence")
polygon(d, col="red", border="blue") 

# what we can infer from above analysis of continous variables:
# -> Target variable 'Absent_Hours' is not normally distributed, which is not a good thing. 
# -> We have to look in to this, before feeding the data to model.

# -> 'Work_Distance','Age','Average_Workload' has good correlation with target feature 'Absent_Hours'.
# -> Let's drop others from further analysis.

# -> There is multi collinearity in dataset. 'Work_Distance' and 'Transportation_Expense' are correlated. 
# -> However, since p(Transportation_Expense) > p(Work_Distance), we'll drop Transportation_Expense from further analysis.
#-----------------------------------------------------------------------------------------------------------------------------------------------------------
# --------------Explore categorical features-----------
head(emp_absntsm[,categoryFeatureList])

#All the values in categorical variables are already 'numbers'. No need to do 'string' -> 'number' conversions as storing categorical data in numbers is more efficient.

#------- Lets see the distribution of each categorical variable with pie-chart distribution

library(dplyr)
#------------------------------------------
emp_cat <-emp_absntsm[,categoryFeatureList]

# -------------create pie plot for 'Absence_Reason'-------------
df_pie <- emp_cat %>%
  group_by(Absence_Reason) %>%
  summarise(counts = n())
df_pie <- df_pie %>%
  arrange(desc(Absence_Reason)) %>%
  mutate(prop = round(counts*100/sum(counts), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
#  pie plot
ggplot(df_pie, aes(x = "", y = prop, fill = Absence_Reason)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0)+
  theme_minimal()

# -------------create pie plot for 'Son'--------
df_pie <- emp_cat %>%
  group_by(Son) %>%
  summarise(counts = n())
df_pie <- df_pie %>%
  arrange(desc(Son)) %>%
  mutate(prop = round(counts*100/sum(counts), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
#  pie plot
ggplot(df_pie, aes(x = "", y = prop, fill = Son)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0)+
  theme_minimal()
# -------------create pie plot for 'Education'--------------
df_pie <- emp_cat %>%
  group_by(Education) %>%
  summarise(counts = n())
df_pie <- df_pie %>%
  arrange(desc(Education)) %>%
  mutate(prop = round(counts*100/sum(counts), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
#  pie plot
ggplot(df_pie, aes(x = "", y = prop, fill = Education)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0)+
  theme_minimal()
# -------------create pie plot for 'Pet'--------------
df_pie <- emp_cat %>%
  group_by(Pet) %>%
  summarise(counts = n())
df_pie <- df_pie %>%
  arrange(desc(Pet)) %>%
  mutate(prop = round(counts*100/sum(counts), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
#  pie plot
ggplot(df_pie, aes(x = "", y = prop, fill = Pet)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0)+
  theme_minimal()
# -------------create pie plot for 'Drinker------------
df_pie <- emp_cat %>%
  group_by(Drinker) %>%
  summarise(counts = n())
df_pie <- df_pie %>%
  arrange(desc(Drinker)) %>%
  mutate(prop = round(counts*100/sum(counts), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
#  pie plot
ggplot(df_pie, aes(x = "", y = prop, fill = Drinker)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0)+
  theme_minimal()
# -------------create pie plot for 'Absence_Month'-----
df_pie <- emp_cat %>%
  group_by(Absence_Month) %>%
  summarise(counts = n())
df_pie <- df_pie %>%
  arrange(desc(Absence_Month)) %>%
  mutate(prop = round(counts*100/sum(counts), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
#  pie plot
ggplot(df_pie, aes(x = "", y = prop, fill = Absence_Month)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0)+
  theme_minimal()

# -------------create pie plot for 'Absence_Day'-----
df_pie <- emp_cat %>%
  group_by(Absence_Day) %>%
  summarise(counts = n())
df_pie <- df_pie %>%
  arrange(desc(Absence_Day)) %>%
  mutate(prop = round(counts*100/sum(counts), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
#  pie plot
ggplot(df_pie, aes(x = "", y = prop, fill = Absence_Day)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0)+
  theme_minimal()

# -------------create pie plot for 'Smoker'-----
df_pie <- emp_cat %>%
  group_by(Smoker) %>%
  summarise(counts = n())
df_pie <- df_pie %>%
  arrange(desc(Smoker)) %>%
  mutate(prop = round(counts*100/sum(counts), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
#  pie plot
ggplot(df_pie, aes(x = "", y = prop, fill = Smoker)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0)+
  theme_minimal()

### These pie distributions are based on the frequency of the 'leaves' taken , not on the tot no. of leaves taken.

#What we can infer from above piplot:

#-> From 'Reason' distribution, we can see that most frequent leaves are taken for the reason 23,28,27
#--------> #23 - medical consultation (23),
#--------> #28 - dental consultation (28)
#--------> #27- physiotherapy (27), 
#--------> #13 - Diseases of the musculoskeletal system and connective tissue 
#--------> #19 - Injury, poisoning and certain other consequences of external causes
#--------> #10 - Diseases of the respiratory system

#->From, 'Month' distribution, we can see that frquency of leaves are more or less uniformally distributed over months, with highest no. of leaves taken in March, Feb and July(holiday season)

#->From, 'Education' distribution, we can see that frquency of leaves are highest for education = 1(highschool)

#->From, 'Weekday' distribution, we can see that frquency of leaves are mostly distributed, with most frequent leaves on 'Monday', which makes sense as most people travel/party over weekend and the mood spills over to Monday :)

#-> From, 'Son' and 'Pet', we can see that people having no kids and no pets(no family responsibilities) tend to take frequent leaves.

#-> 'Social Drinker' takes little more leaves than non drinker.

#------- Lets see how individual categorical features affect count of the rented bike----------
#graph individual categorical features by count

#-- Lets see how 'month' affects the absenteeism----

cat <- aggregate(x=emp_absntsm$Absent_Hours, by=list(Category=emp_absntsm$Absence_Month), FUN=sum)
cat_barplot1 <-ggplot(data=cat, aes(x=Category,y=x)) +
  geom_bar(stat="identity", fill="blue")+
  xlab('Absence_Month') + ylab('Absent_Hours')+
  theme_minimal()
cat_barplot1

#--> Clearly, March tops the month for most absences. This makes sense as this is peak holiday season due to change of weather and clear and sunny sky
#--> Second one is July, which again is the 'holiday' season 
#------------------------------------------------------------------
#-- Lets see how 'Son' affects the absenteeism-----

cat <- aggregate(x=emp_absntsm$Absent_Hours, by=list(Category=emp_absntsm$Son), FUN=sum)
cat_barplot1 <-ggplot(data=cat, aes(x=Category,y=x)) +
  geom_bar(stat="identity", fill="blue")+
  xlab('Son') + ylab('Absent_Hours')
theme_minimal()
cat_barplot1

# Clearly, employee with 3-4 kids tend to take less hours of absence
#------------------------------------------------------------------
#-- Lets see how 'Absence_Reason' affects the absenteeism----

cat <- aggregate(x=emp_absntsm$Absent_Hours, by=list(Category=emp_absntsm$Absence_Reason), FUN=sum)
cat_barplot1 <-ggplot(data=cat, aes(x=Category,y=x)) +
  geom_bar(stat="identity", fill="blue")+
  xlab('Absence_Reason') + ylab('Absent_Hours')
theme_minimal()
cat_barplot1

#### Overall, 
#---> Seems like employee takes most absences for medical consulations/dental consultation/physiotherapy.
#---> these hours can be rduced by setting up a medical consultation/dental consultation/physiotherapy booth(with visiting doctors may be) at office/facility
#---> In long term, introducing exercise/yoga sessions in office once/twice a week will help reduce physiotherapy issues
#------------------------------------------------------------------

#------ Exploratory Data Analysis ENDS Here--------------------------------------------------------------------------------------------------------------------------

#----------------------Prepare data for modelling ------------------

#---- Drop the features which are not very relevant based on above analyses
emp_df <- emp_absntsm[ , (names(emp_absntsm) %in% c('ID','Absence_Month','Son','Drinker','Work_Distance','Service_Time','Age','Average_Workload','Absent_Hours'))]
head(emp_df)


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------

#----------Part 5 : Model Builing starts here ----------------------
#Train the models with both datasets(before and after feature engineering)
# For models,I'll only use the dataset with feature engineering implemented
#------------------------------------------------------------------

# 1.) I am selecting 3 models to test and evaluate
#   -> Linear Regression Model
#   -> Random Forrest (ensemble method using bagging technique)
#   -> Gradient Boosting (ensemble method using boosting technique)
#2.) Cross validation    
#3.) All these 3 models will be compared and evaluated(with and without feature engineering)
#4.) We'll choose the best out of 3

#------------------------------------------------------------------
#----- 5a.) -- Selecting train and test datasets for cross validations
#split in to test and train(after featr engineering)
train_index=createDataPartition(emp_df$Absent_Hours, p=0.8, list = FALSE)
train=emp_df[train_index,]
test=emp_df[-train_index,]
head(train)
head(test)

X_train = train[ , !(names(train) %in% c('Absent_Hours'))]
Y_train = train['Absent_Hours']
X_test = test[ , !(names(test) %in% c('Absent_Hours'))]
Y_test = test['Absent_Hours']

dim(X_train)
dim(X_test)
#--- *AFT <=> After Feature Engineering------
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------

#--- define a function which takes model, predicted and test values and returns evalution matrix: R-squared value and MeanAbsoluteError
#------ Define Function ----------------------------------------
model_eval_matrix <- function (model,actual, predicted)
{
  r_squared = 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))
  mae = MAE(actual, predicted)
  return(c(model,r_squared,mae))
}
#------ Function definition ends here --------------------------
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------

#------------------------------------------------- 5c.) Define and fit models ---------------

#-------------------------  Linear regession model ----------------------------------
lrm_regressor = lm(Absent_Hours~. , data = train) # Define and train the model

summary(lrm_regressor)
#summary(lrm_regressor)$r.squared
#Residual standard error: 780.4 on 529 degrees of freedom
#Multiple R-squared:  0.8555,	Adjusted R-squared:   0.84 
#F-statistic: 54.95 on 57 and 529 DF,  p-value: < 2.2e-16

Y_predict_lrm = predict(lrm_regressor, X_test) #predict with the model

actuals_preds_lrm = data.frame(cbind(actuals=test$Absent_Hours, predicteds=Y_predict_lrm))
actuals_preds_lrm

#calling function and storing performance values
performance_vector_lrm = model_eval_matrix('LRM',test$Absent_Hours,Y_predict_lrm)

#------- Random Forest Model (Ensemble method using Bagging technique) --------------

forest_reg = randomForest(Absent_Hours~. , data = train, ntree = 800, importance = TRUE) # 800 tree is best bestameter as tested
#RF2List

Y_predict_forest = predict(forest_reg, X_test)

actuals_preds_forest <- data.frame(cbind(actuals=test$Absent_Hours, predicteds=Y_predict_forest))
actuals_preds_forest

MAE(test$cnt,Y_predict_forest)

#calling function and storing performance values
performance_vector_rf = model_eval_matrix('RF',test$Absent_Hours,Y_predict_forest)

## ----------- Building Gradient Boosting Model (Ensemble method using Boosting technique) ---------------
#gbm_reg = gbm(cnt~. ,data = train , distribution = "gaussian", n.trees = 2000) # without parameter hypertuning
gbm_reg = gbm(Absent_Hours~. ,data = train , distribution = "gaussian", n.trees = 2000, shrinkage = 0.01, interaction.depth = 4) # with parameter hypertuning

Y_predict_gbm = predict(gbm_reg, X_test, n.trees = 2000)

actuals_preds_gbm <- data.frame(cbind(actuals=test$Absent_Hours, predicteds=Y_predict_gbm))
actuals_preds_gbm

#calling function and storing performance values
performance_vector_gbm = model_eval_matrix('GBM',test$cnt,Y_predict_gbm)

MAE(test$Absent_Hours,Y_predict_gbm)

summary(gbm_reg)

#-------------------------------------------Part 5 ENDS here -------------------------------------------------------------------------------------------------------

#----------------------------------------Part 6 : Model comparisions STARTS here---------------------------

#-----Stroring all model performances in dataframe to compare----

#Clearly, Random Forest proves to be best model here -----
# We'll use Random forest as our final model to predict 2011 losses due to absence
# FINAL MODE :: RANDOM FOREST

#----------------------------------------Part 6 : Model comparisions ENDS here ---------------------

#-------------------------------------------Part 7 : Produce sample output with tuned model STARTS here----------------------

absence_2010=X_test
absence_2010['Actual_Absence'] = Y_test
absence_2010['Predicted_Absence'] = Y_predict_gbm
#--- Sample output(with actual absence and predicted absence) ---
absence_2010

#--------------------------------Part 8: Predicting 2011 absences ----------------

absence_2011 = emp_df
absence_2011$Age = absence_2011$Age+1
absence_2011$Service_Time = absence_2011$Service_Time+1
absence_2011 = absence_2011[ , !(names(test) %in% c('Absent_Hours'))]

#---- predict using random forest model

predicted_absence_2011 = predict(forest_reg, absence_2011)
predicted_absence_2011









































