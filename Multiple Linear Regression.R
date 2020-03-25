# Import dataset

data <- read.csv(file = "C:/Users/Hp/Desktop/data science/R/Datasets/insurance.csv")

#Removing the irrelavent variable
data$region <- NULL

#checking tha data type
names(data)
str(data)

# count of level in a variable

table(data$smoker)
table(data$sex)

# converting categorical data to numeric 

# Creating a new column to represent sex and smoker
data$Smoker_flag <- ifelse(data$smoker == 'yes' , 1 , 2)
data$sex_Flag <- ifelse(data$sex == 'male' , 1 , 2)

# Data conversion 

str(data)
data$age <- as.numeric(data$age)
data$children <- as.numeric(data$children)
data$Smoker_flag <- as.numeric(data$Smoker_flag)
data$sex_Flag <- as.numeric(data$sex_Flag)
data$Month <- as.numeric(data$Month)

# checking for data type
str(data)

#check MV

sapply(data, function(x) sum(is.na(data)))

# to identify outlier 
par(mfrow = c(1,1))
boxplot(data$age)          #no
boxplot(data$bmi)          #yes
boxplot(data$children)     #no
boxplot(data$charges)      #yes
boxplot(data$Month)        #no

# Treatment of outlier for bmi

summary(data$bmi)
upper <- 34.69 + 1.5* IQR(data$bmi) ;  upper
data$bmi [data$bmi > upper] <- upper
boxplot(data$bmi)
summary(data$bmi)

# Treatment of outlier for charges

summary(data$charges)
upper <- 16640 + 1.5* IQR(data$charges) ;  upper
data$charges [data$charges > upper] <- upper
boxplot(data$charges)
summary(data$charges)

#data subset
# removing original sex and smoker column
data1 <- data[ , -c(2,5)]


#data partition

set.seed(546)
library(caret)
Train <- createDataPartition(data1$charges , p = 0.70 , list = FALSE)
training <- data1[ Train ,]
testing <- data1[ -Train ,]

#checking correlation

cor(training)

# model building

model <- lm(charges~. , data=training)
summary(model)    #we are getting R value as 1
vif(model)  

# Since R square and adjusted R square value is 1, our model has overfitted

# identifying the solution to overfitting with the help of transformation

hist(training$charges)
hist((1/training$charges))
hist(log(training$charges))

#  model 2 
model2 <- step(lm(log(charges)~. , data = training), direction = "backward")
summary(model2)   # R square value - 91

# collinearity and multi- collinearity
library(car)
vif(model2)

#Assumption of linear Regression

par(mfrow = c(2,2))
plot(model2)

# model error assumption

library(lmtest)
dwtest(model2)

# constant variance assumption in term of Numerica method
ncvTest(model2)

# Prediction

testing$fitted <- predict(model2 , testing)

#  transforming log values to real values

testing$Original <- exp(testing$fitted)
