library(Metrics)
library("dplyr")
library(readxl)
library(MASS)

setwd("C:/Users/sharaa-cont/Desktop/Projects/11. Ad-Hoc/MPG Prediction")
####import Data####
auto = read_xlsx("Auto mpg.xlsx", sheet = "Data")

####Descriptive analysis####
str(auto)
summary(auto)

####Data preparation####
auto$cylinders = as.factor(auto$cylinders)
auto$horsepower = as.numeric(auto$horsepower)
auto$origin = as.factor(auto$origin)
auto$`model year` = as.factor(auto$`model year`)
summary(auto)

####Imputation - Missing value in Horsepower variable####
auto$horsepower[is.na(auto$horsepower)] <- mean(auto$horsepower, na.rm = TRUE)
summary(auto)

####Correlation####
Cor1 <- cor(auto[,c(1,3,4,5,6)])
round(Cor1, 2)
#  MPG is highly correlated with Displacement(-0.8), Horsepower(-0.77) and Weight(-0.83)

plot(auto$cylinders,auto$mpg)


####Train and Test Data####
set.seed(123)
train <- sample(seq_len(nrow(auto)), size = floor(0.8 * nrow(auto)))

auto_train <- auto[train, ]
auto_test <- auto[-train, ]


####Data Visualization####
par(mfrow = c(2,2))
boxplot(auto_train$displacement, col = "Yellow", main = "Boxplot of Displacement")
boxplot(auto_train$horsepower, col = "Blue", main = "Boxplot of Horsepower")
boxplot(auto_train$weight, col = "Red",  main = "Boxplot of Weight")
boxplot(auto_train$acceleration, col = "Pink",  main = "Boxplot of Acceleration")

par(mfrow = c(2,4))
hist(auto_train$displacement, main = "Distribution of displacement")
hist(auto_train$horsepower, col = "Green", main = "Distribution of horsepower")
hist(auto_train$weight, col = "Yellow", main = "Distribution of weight")
hist(auto_train$acceleration, col = "Blue", main = "Distribution of acceleration")
barplot(height = table(auto_train$cylinders),col = "Red",  main = "Distribution of cylinder")
barplot(height = table(auto_train$origin),col = "Blue",  main = "Distribution of Origin")
barplot(height = table(auto_train$`model year`),col = "Blue",  main = "Distribution of Model year")


str(auto)

####Linear Regression####
lm_model = lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration + `model year` + origin, data = auto_train)
summary(lm_model)

#outlier
sum(is.na(auto_train))
cooks.distance(lm_model)
a <- which(cooks.distance(lm_model)> 4/length(auto_train$mpg))
auto_train_1 <- auto_train[-a,]

##Model fitting after removing Outliers
lm_model=lm(mpg~weight+acceleration+horsepower+displacement+origin+`model year`+cylinders,data=auto_train_1)
summary(lm_model)
vif(lm_model)

##Removing Displacement
lm_model=lm(mpg~weight+acceleration+horsepower+origin+`model year`+cylinders,data=auto_train_1)
summary(lm_model)
vif(lm_model)

auto_test$PredVal <- predict.lm(object = lm_model, newdata = auto_test)
auto_train_1$PredVal = predict(lm_model, auto_train_1)

rmse_train_lm_1 = sqrt(sum((auto_train_1$PredVal - auto_train_1$mpg)^2))
rmse_test_lm_1 = sqrt(sum((auto_test$PredVal - auto_test$mpg)^2))
rmse_train_lm_1
rmse_test_lm_1





#Assumptions
shapiro.test(lm_model$residuals)
#Since p-value > 0.05, we accept Null Hypothesis, ie Normality Assumption is satisfied

ncvTest(lm_model)
#Since p-value < 0.05, we can say our data is not homoscedastic 

durbinWatsonTest(lm_model)
#Since p-value<0.05, we reject the null Hypothesis
#Assumption is violated

#Plot for linearity
plot(lm_model_4, which = 1)


#Box-cox Transformation
b <- boxcox(mpg ~ cylinders + horsepower + weight + `model year` + origin, data = auto_train_1)

lambda <- b$x # lambda values
lik <- b$y # log likelihood values for SSE
bc <- cbind(lambda, lik) # combine lambda and lik
sorted_bc <- bc[order(-lik),] # values are sorted to identify the lambda value 
head(sorted_bc, n = 10)

##model building after log transformation
lm_model_1 = lm(log(mpg) ~ cylinders + displacement + horsepower + weight + 
                  acceleration + `model year` + origin, data = auto_train_1)
summary(lm_model_1)
vif(lm_model_1)


lm_model_2 = lm(log(mpg) ~ cylinders + horsepower + weight + acceleration 
                + `model year` + origin, data = auto_train_1)
summary(lm_model_2)
summary(lm_model_2)$coef
vif(lm_model_2)

#Treating outliers
cooks.distance(lm_model_2)
a_1 <- which(cooks.distance(lm_model_2)> 4/length(auto_train_1$mpg))
auto_train_2 <- auto_train[-a_1,]

#Fitting Model after removing outliers
lm_model_3 = lm(log(mpg) ~ cylinders + horsepower + weight + 
                  acceleration + `model year` + origin, data = auto_train_2)
summary(lm_model_3)
vif(lm_model_3)

#Removing 'Acceleration'
lm_model_4 = lm(log(mpg) ~ cylinders + horsepower + weight + 
                  `model year` + origin, data = auto_train_2)
summary(lm_model_4)
summary(lm_model_4)$coef

##Checking for Assumptions

vif(lm_model_4)
shapiro.test(lm_model_4$residuals)
#Since p-value > 0.05, we accept Null Hypothesis, ie Normality Assumption is satisfied

ncvTest(lm_model_4)
#Since p-value > 0.05, we can say our data is homoscedastic 

durbinWatsonTest(lm_model_4)
#Since p-value>0.05, we accept the null Hypothesis
#Assumption is Satisfied

vif(lm_model_4)
plot(lm_model_4, which = 1)


####Running the model on test data by applying the same transformations on test data####
auto_test$new_mpg <- log(auto_test$mpg)
# Predict the charges
auto_test$PredVal <- predict.lm(object = lm_model_4, newdata = auto_test)
auto_train_2$PredVal = predict(lm_model_4, auto_train_2)

colnames(auto_train_2)
colnames(auto_test)

auto_train_2$log_mpg <- log(auto_train_2$mpg)

rmse_train_lm = sqrt(mean((auto_train_2$PredVal - auto_train_2$log_mpg)^2))
rmse_test_lm = sqrt(mean((auto_test$PredVal - auto_test$new_mpg)^2))
rmse_train_lm
rmse_test_lm

colnames(auto_test)
####Accuracy####
cor(auto_test$new_mpg, auto_test$PredVal)

##### MAPE Calculation
mape <- mean(abs((auto_test$PredVal - auto_test$new_mpg))/auto_test$new_mpg)
mape



# Stepwise Regression -----------------------------------------------------

data = read_xlsx("Auto mpg.xlsx", sheet = "Data")

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library("ggplot2") 
library("corrgram")
library("caret")
library("MASS")
library("rpart")
library("dplyr")

# Check how the data was read in
str(data)

# Do data conversions to match up with what it is supposed to be
data$cylinders = as.factor(data$cylinders)
data$horsepower = as.numeric(as.character(data$horsepower))
data$weight = as.numeric(data$weight)
data$`model year` = as.factor(data$`model year`)
data$origin = as.factor(data$origin)

# Impute the mean for the N/A in horsepower
data$horsepower[is.na(data$horsepower)] = mean(data$horsepower, na.rm = TRUE)

# Set a seed to try and reproduce the results
set.seed(32343) 

# Split the data into a training and testing set
intrain = createDataPartition(y = data$mpg, p = .75, list = FALSE) # 75% in training sample
training = data[intrain,]
testing = data[-intrain,]
dim(training) # Just checking the dimensions of the training dataset



# Forward Selection -------------------------------------------------------

# Forward Selection Regression
modelFit_step_forward = lm(mpg ~ cylinders +
                             displacement +
                             horsepower +
                             weight +
                             acceleration +
                             `model year` +
                             origin, data = data)

step_forward = stepAIC(modelFit_step_forward, direction = "forward")

# What did we get?
summary(step_forward)
step_forward$anova

# Make predictions
pred_train_forward = predict(step_forward, training)
pred_test_forward = predict(step_forward, testing)

# Get the RMSE
rmse_train = sqrt(mean((pred_train_forward - training$mpg)^2))
rmse_test = sqrt(mean((pred_test_forward - testing$mpg)^2))
rmse_train
rmse_test


# Backward Elimination ----------------------------------------------------

# Stepwise Regression
modelFit_step_backward = lm(mpg ~ cylinders +
                              displacement +
                              horsepower +
                              weight +
                              acceleration +
                              `model year` +
                              origin, data = data)

step_backward = stepAIC(modelFit_step_backward, direction = "backward")

# What did we get?
summary(step_backward)
step_backward$anova


# Make predictions
pred_train_backward = predict(step_backward, training)
pred_test_backward = predict(step_backward, testing)

# Get the RMSE
rmse_train = sqrt(mean((pred_train_backward - training$mpg)^2))
rmse_test = sqrt(mean((pred_test_backward - testing$mpg)^2))
rmse_train
rmse_test








# Stepwise Regression
modelFit_step = lm(mpg ~ cylinders +
                     displacement +
                     horsepower +
                     weight +
                     acceleration +
                     `model year` +
                     origin, data = data)

step = stepAIC(modelFit_step, direction = "both")

# What did we get?
summary(step)
step$anova


# Make predictions
pred_train = predict(step, training)
pred_test = predict(step, testing)

# Get the RMSE
rmse_train = sqrt(mean((pred_train - training$mpg)^2))
rmse_test = sqrt(mean((pred_test - testing$mpg)^2))
rmse_train
rmse_test

ggplot(testing, aes(x = pred_test, y = mpg)) + 
  geom_point(colour = "blue", size = 3) + 
  geom_smooth(method = "lm") +
  xlab("Predicted MPG") + 
  ylab("Actual MPG") +
  ggtitle("Predicted Values vs. Actual Values") + 
  guides(fill=FALSE)



