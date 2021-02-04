#GRIP-TSF-Task-1
#Prediction using Supervised ML (Level-Beginner)
#Prediction of percentage of a student based on number of study hours by linear regression.
library(ggplot2)
student=read.csv("student_studies.csv")
View(student)

#We can see that there is no missing data.
#Let us see the summary of the data. 
summary(student)

#to check structure of our data set
str(student)
#Let's plot our data points on 2-D graph to eyeball our dataset and see if we can manually find any relationship between the data. 
#We can create the plot with the following script:
ggplot(data = student,aes(Hours,Scores))+
  geom_point(size=3,color="blue")+
  labs(title="Hours vs Percentage",
       x= "Hours Studied", y = "Percentage Score")

#**From the graph above, we can clearly see that there is a positive linear relation between the number of hours studied and percentage of score.**

#Now that we have our attributes and labels, the next step is to split this data into training and test sets. 
dt = sort(sample(nrow(student), nrow(student)*0.2))
train<-student[dt,]
test<-student[-dt,]

#We have split our data into training and testing sets, and now is finally the time to train our algorithm

x=train$Hours
y=train$Scores
train_student = lm(formula =y~x,data = train)
summary(train_student)
#Plotting the regression line
#Plotting for the test data
ggplot(test,aes(x=Hours,y=Scores))+
  geom_point(size=3,color="blue")+
  geom_smooth(method=lm)+
  labs(title="Hours vs Percentage",
       x= "Hours Studied", y = "Percentage Score")

# Testing data - In Hour
z=data.frame(x)
print(z)

#Actual Scores
Actual_Scores=train$Scores
ActualScores=data.frame(y)
## Predicting the scores
Predict_scores<- predict(train_student, data.frame(Actual_Scores))

# Comparing Actual vs Predicted
Comparing_Scores=data.frame(Actual_Scores,Predict_scores)
print(Comparing_Scores)


#creating linear regression model using lm() function
lrstudent = lm(formula =Scores~Hours,data = student)
summary(lrstudent)

#we need to find the marks obtained by student if he/she studies for 9.25 hours
test_data <- data.frame(Hours = 9.25)

result=predict(lrstudent,test_data)
print(result)
#Output- For 9.25 Hours of studying, a student is predicted to get 92.90985 marks according to our model.

#The final step is to evaluate the performance of algorithm. 
#This step is particularly important to compare how well different algorithms perform on a particular dataset. 
#For simplicity here, we have chosen the mean square error. There are many such metrics.

library(Metrics) 
result = rmse(Actual_Scores, Predict_scores)
print(result)
