heart_data <- read.csv(file="heart_disease.csv", header=TRUE, sep=",")

# Converting appropriate variables to factors  
heart_data <- within(heart_data, {
   target <- factor(target)
   sex <- factor(sex)
   cp <- factor(cp)
   fbs <- factor(fbs)
   restecg <- factor(restecg)
   exang <- factor(exang)
   slope <- factor(slope)
   ca <- factor(ca)
   thal <- factor(thal)
})

head(heart_data, 10)

print("Number of variables")
ncol(heart_data)

print("Number of rows")
nrow(heart_data)

#model number one 
#heart disease using variables age (age), 
#resting blood pressure (trestbps), exercised induced angina (exang), 
#and maximum heart rate achieved (thalach). 

# Create the complete model
model_1 <- glm(target ~ age + trestbps + exang + thalach, data = heart_data, family = "binomial")

summary(model_1)

#dummy variables
contrasts(heart_data$exang)

#GOF test
library(ResourceSelection)

print("Hosmer-Lemeshow Goodness of Fit Test")
hl = hoslem.test(model_1$y, fitted(model_1), g=50)
hl

#wald
conf_int <- confint.default(model_1, level=0.95)
round(conf_int,4)

#confusion matrix

# Predict default or no_default for the data set using the model
default_model_data <- heart_data[c('age', 'trestbps', 'exang', 'thalach')]
pred <- predict(model_1, newdata=default_model_data, type='response')

# If the predicted probability of default is >=0.50 then predict heart disease (default='1'), otherwise predict no 
# default (default='0') 
depvar_pred = as.factor(ifelse(pred >= 0.5, '1', '0'))

# This creates the confusion matrix
conf.matrix <- table(heart_data$target, depvar_pred)[c('0','1'),c('0','1')]
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ": default=")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix), sep = ": default=")

# Print nicely formatted confusion matrix
print("Confusion Matrix")
format(conf.matrix,justify="centre",digit=2)

#Metrics (accuracy, precision, recall)

accuracy <- (134 + 89) / (89+134+31+49)
precision <-  (134)/(134+49)
recall <-  134/(31+134)

round(accuracy, 4)
round(precision, 4)
round(recall, 4)

#ROC and AUC
library(pROC)

labels <- heart_data$target
predictions <- model_1$fitted.values

roc <- roc(labels ~ predictions)

print("Area Under the Curve (AUC)")
round(auc(roc),4)

print("ROC Curve")
# True Positive Rate (Sensitivity) and False Positive Rate (1 - Specificity)
plot(roc, legacy.axes = TRUE)

#Predictions
#person A: 50 years, bps = 140, bloodpress = 122, exercise angina = True
print("Prediction: 50 years, bps = 140, bloodpress = 122, exercise angina = True")
newdata_3a <- data.frame(age = 50, trestbps = 122, thalach = 140, exang = '1')
pred_A <- predict(model_1, newdata_3a, type='response')
round(pred_A, 4)

#person B: 50 years, bps = 165, bloodpress = 130, exercise angina = false
print("Prediction: 50 years, bps = 165, bloodpress = 130, exercise angina = false")
newdata_3b <- data.frame(age = 50, trestbps = 130, thalach = 165, exang = '0')
pred_B <- predict(model_1, newdata_3b, type='response')
round(pred_B, 4)

#model number two
#heart disease 
#using variables age (age), resting blood pressure (trestbps), 
#type of chest pain experienced (cp), maximum heart rate achieved (thalach); 
#Include the quadratic term for age and the interaction term between age and maximum heart rate achieved.

# Create the complete model
model_2 <- glm(target ~ age + trestbps + cp + thalach + I(age^2) + age:thalach, data = heart_data, family = "binomial")

summary(model_2)

#contrasts of cp
contrasts(heart_data$cp)

#wald @ 5%
conf_int <- confint.default(model_2, level=0.95)
round(conf_int,4)

#GOF
library(ResourceSelection)

print("Hosmer-Lemeshow Goodness of Fit Test")
hl = hoslem.test(model_2$y, fitted(model_2), g=50)
hl

#confusion Matrix
# Predict default or no_default for the data set using the model
default_model_data <- heart_data[c('age', 'trestbps', 'cp', 'thalach')]
pred <- predict(model_2, newdata=default_model_data, type='response')

# If the predicted probability of default is >=0.50 then predict heart disease (default='1'), otherwise predict no
# default (default='0') 
depvar_pred = as.factor(ifelse(pred >= 0.5, '1', '0'))

# This creates the confusion matrix
conf.matrix <- table(heart_data$target, depvar_pred)[c('0','1'),c('0','1')]
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ": default=")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix), sep = ": default=")

# Print nicely formatted confusion matrix
print("Confusion Matrix")
format(conf.matrix,justify="centre",digit=2)

#Metrics
accuracy <- (129 + 102) / (129 + 102 + 36 + 36)#correct predictions divided aby all
precision <- 129/(129 + 36) #corect positives divided by all predicted positives
recall <-  129/(129+36)#actual correct positives divided by all real positives

round(accuracy, 4)
round(precision, 4)
round(recall, 4)

#ROC and AUC
library(pROC)

labels <- heart_data$target
predictions <- model_2$fitted.values

roc <- roc(labels ~ predictions)

print("Area Under the Curve (AUC)")
round(auc(roc),4)

print("ROC Curve")
# True Positive Rate (Sensitivity) and False Positive Rate (1 - Specificity)
plot(roc, legacy.axes = TRUE)

#predictions
#person A: 50 years, resting press = 115, no chest pain, max rate = 133
#Person B: 50 years, resting press = 125, typical angina, max rate = 155

print("Prediction: 50 years, resting press = 115, no chest pain, max rate = 133")
newdata_4a <- data.frame(age = 50, trestbps = 115, thalach = 133, cp = '0')
pred_A <- predict(model_2, newdata_4a, type='response')
round(pred_A, 4)


print("Prediction: 50 years, resting press = 125, typical angina, max rate = 155")
newdata_4b <- data.frame(age = 50, trestbps = 125, thalach = 155, cp = '1')
pred_B <- predict(model_2, newdata_4b, type='response')
round(pred_B, 4)

#set seed to 6522048
#training 85%
#number in reach set


set.seed(6522048)

# Partition the data set into training and testing data
samp.size = floor(0.85*nrow(heart_data))

# Training set
print("Number of rows for the training set")
train_ind = sample(seq_len(nrow(heart_data)), size = samp.size)
train.data = heart_data[train_ind,]
nrow(train.data)

# Testing set 
print("Number of rows for the testing set")
test.data = heart_data[-train_ind,]
nrow(test.data)

#traininng vs testing error
#heart disease using variables age (age), 
#sex (sex), chest pain type (cp), resting blood pressure (trestbps), 
#cholesterol measurement (chol), resting electrocardiographic measurement (restecg), 
#exercise-induced angina (exang), and number of major vessels (ca). Use a maximum of 150 trees

library(randomForest)

# checking
#=====================================================================
train = c()
test = c()
trees = c()

for(i in seq(from=1, to=150, by=1)) {
    #print(i)
    
    trees <- c(trees, i)
    
    model_rf3 <- randomForest(target ~ age + sex + cp + trestbps + chol + restecg + exang + ca, data=train.data, ntree = i)
    
    train.data.predict <- predict(model_rf3, train.data, type = "class")
    conf.matrix1 <- table(train.data$target, train.data.predict)
    train_error = 1-(sum(diag(conf.matrix1)))/sum(conf.matrix1)
    train <- c(train, train_error)
    
    test.data.predict <- predict(model_rf3, test.data, type = "class")
    conf.matrix2 <- table(test.data$target, test.data.predict)
    test_error = 1-(sum(diag(conf.matrix2)))/sum(conf.matrix2)
    test <- c(test, test_error)
}
 
#matplot (trees, cbind (train, test), ylim=c(0,0.5) , type = c("l", "l"), lwd=2, col=c("red","blue"), ylab="Error", xlab="number of trees")
#legend('topright',legend = c('training set','testing set'), col = c("red","blue"), lwd = 2 )

plot(trees, train,type = "l",ylim=c(0,1.0),col = "red", xlab = "Number of Trees", ylab = "Classification Error")
lines(test, type = "l", col = "blue")
legend('topright',legend = c('training set','testing set'), col = c("red","blue"), lwd = 2 )

#confusion matrix training

model_rf4 <- randomForest(target ~ age + sex + cp + trestbps + chol + restecg + exang + ca, data=train.data, ntree = 20)

# Confusion matrix
print("======================================================================================================================")
print('Confusion Matrix: TRAINING set based on random forest model built using 20 trees')
train.data.predict <- predict(model_rf4, train.data, type = "class")

# Construct the confusion matrix
conf.matrix <- table(train.data$target, train.data.predict)[c('0','1'),c('0','1')]
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ": ")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix), sep = ": ")

# Print nicely formatted confusion matrix
format(conf.matrix,justify="centre",digit=2)

#metrics
accuracy <- (137+117) / (137+117+0+3)#correct predictions divided aby all
precision <- 137/(137 + 3) #corect positives divided by all predicted positives
recall <-  136/(136 + 0)#actual correct positives divided by all real positives

round(accuracy, 4)
round(precision, 4)
round(recall, 4)

#confusion testing
print("======================================================================================================================")
print('Confusion Matrix: TESTING set based on random forest model built using 20 trees')
test.data.predict <- predict(model_rf4, test.data, type = "class")

# Construct the confusion matrix
conf.matrix <- table(test.data$target, test.data.predict)[c('0','1'),c('0','1')]
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ": ")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix), sep = ": ")

# Print nicely formatted confusion matrix
format(conf.matrix,justify="centre",digit=2)

#metrics
accuracy <- (21+11)/(21+7+7+11)#correct predictions divided aby all
precision <- 21/(21 + 7) #corect positives divided by all predicted positives
recall <-  21/(21 +7)#actual correct positives divided by all real positives

round(accuracy, 4)
round(precision, 4)
round(recall, 4)

#set seed to 6522048
#training 80%
#number in each set
set.seed(6522048)

# Partition the data set into training and testing data
samp.size = floor(0.8*nrow(heart_data))

# Training set
print("Number of rows for the training set")
train_ind = sample(seq_len(nrow(heart_data)), size = samp.size)
train.data = heart_data[train_ind,]
nrow(train.data)

# Testing set 
print("Number of rows for the testing set")
test.data = heart_data[-train_ind,]
nrow(test.data)

#RSME vs number of trees
# maximum heart rate achieved 
#using age (age), sex (sex), chest pain type (cp), resting blood pressure (trestbps), 
#cholesterol measurement (chol), resting electrocardiographic measurement (restecg), 
#exercise-induced angina (exang), and number of major vessels (ca). 
#Use a maximum of 80 trees
set.seed(6522048)
library(randomForest)

# Root mean squared error
RMSE = function(pred, obs) {
    return(sqrt( sum( (pred - obs)^2 )/length(pred) ) )
}

# checking
#=====================================================================
train = c()
test = c()
trees = c()

for(i in seq(from=1, to=80, by=1)) {
    trees <- c(trees, i)
    model_rf7 <- randomForest(thalach ~ age + sex + cp + trestbps + chol + restecg + exang + ca, data=train.data, ntree = i)
    
    pred <- predict(model_rf7, newdata=train.data, type='response')
    rmse_train <-  RMSE(pred, train.data$thalach)
    train <- c(train, rmse_train)
    
    pred <- predict(model_rf7, newdata=test.data, type='response')
     rmse_test <-  RMSE(pred, test.data$thalach)
    test <- c(test, rmse_test)
}
 
plot(trees, train,type = "l",ylim=c(0,40),col = "red", xlab = "Number of Trees", ylab = "Root Mean Squared Error")
lines(test, type = "l", col = "blue")
legend('topright',legend = c('training set','testing set'), col = c("red","blue"), lwd = 2 )

#rsme for training

library(randomForest)
model_rf8 <- randomForest(thalach ~ age + sex + cp + trestbps + chol + restecg + exang + ca, data=train.data, ntree = 10)

# Root mean squared error
RMSE = function(pred, obs) {
    return(sqrt( sum( (pred - obs)^2 )/length(pred) ) )
}

print("======================================================================================================================")
print('Root Mean Squared Error: TRAINING set based on random forest model built using 10 trees')
pred <- predict(model_rf8, newdata=train.data, type='response')
RMSE(pred, train.data$thalach)

#rsme for testing
print("======================================================================================================================")
print('Root Mean Squared Error: TESTING set based on random forest model built using 10 trees')
pred <- predict(model_rf8, newdata=test.data, type='response')
RMSE(pred, test.data$thalach)