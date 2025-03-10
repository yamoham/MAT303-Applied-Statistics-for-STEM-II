# Loading package to show the decision tree
install.packages("rpart.plot")

# Loading credit card default data set
credit_default <- read.csv(file='credit_card_default.csv', header=TRUE, sep=",")

print("head")
head(credit_default, 6)

set.seed(6751342)

# Partition the data set into training and testing data
samp.size = floor(0.70*nrow(credit_default))

# Training set
print("Number of rows for the training set")
train_ind = sample(seq_len(nrow(credit_default)), size = samp.size)
train.data1 = credit_default[train_ind,]
nrow(train.data1)

# Testing set 
print("Number of rows for the validation set")
test.data1 = credit_default[-train_ind,]
nrow(test.data1)

set.seed(6751342)

# creating the model
library(rpart)
model1 <- rpart(default ~ missed_payment + credit_utilize + assets, method="class", data=train.data1, control = rpart.control(minsplit=10))
printcp(model1)

#plotcp(model) # Visualize cross validation results
plotcp(model1, minline = TRUE, lty = 3, col = 2, upper = c("size", "splits", "none"))

set.seed(6751342)
pruned_model1 <- rpart(default ~ missed_payment + credit_utilize + assets, method="class",  data=train.data1, control = rpart.control(cp = 0.21))
printcp(pruned_model1)

library(rpart.plot)
rpart.plot(pruned_model1)

# make predictions on the test data
pred <- predict(pruned_model1, newdata=test.data1, type='class')

# construct the confusion matrix
conf.matrix <- table(test.data1$default, pred)[,c('no','yes')]
rownames(conf.matrix) <- paste("Actual default", rownames(conf.matrix), sep = ": ")
colnames(conf.matrix) <- paste("Prediction default", colnames(conf.matrix), sep = ": ")

# print nicely formatted confusion matrix
print("Confusion Matrix")
format(conf.matrix,justify="centre",digit=2)

# prediction 1
print("Prediction 1 for defaulting (yes or no): missed_payment='no', assets='car_house', credit_utilize=0.30")
newdata1 <- data.frame(missed_payment='no', assets='car_house', credit_utilize=0.30)
predict(pruned_model1, newdata1, type='class')

# prediction 2
print("Prediction 2 for defaulting (yes or no): missed_payment='yes', assets='none', credit_utilize=0.30")
newdata2 <- data.frame(missed_payment='yes', assets='none', credit_utilize=0.30)
predict(pruned_model1, newdata2, type='class')

# Loading economic dataset
economic <- read.csv(file='economic.csv', header=TRUE, sep=",")
head(economic, 6)

set.seed(6751342)

# partition the dataset into training and test data
samp.size = floor(0.80*nrow(economic))

# training set
print("Number of rows for the Training set")
train_ind = sample(seq_len(nrow(economic)), size = samp.size)
train.data2 = economic[train_ind,]
nrow(train.data2)

# testing set 
print("Number of rows for the Testing set")
test.data2 = economic[-train_ind,]
nrow(test.data2)

set.seed(6751342)

library(rpart)
model2 <- rpart(wage_growth ~ economy + unemployment + gdp, method="anova", data=train.data2, minsplit=10)
printcp(model2)

# visualize cross-validation results
plotcp(model2)

set.seed(6751342)

pruned_model2 <- rpart(wage_growth ~ economy + unemployment + gdp, method="anova",  data=train.data2, control = rpart.control(cp = 0.014))
printcp(pruned_model2)

library(rpart.plot)
rpart.plot(pruned_model2)

# Here is a custom R function to calculate Root Mean Squared Error or RMSE for any regression model. The following function (called RMSE) will calculate the Root Mean Squared Error
# based on the formula shown above.
RMSE = function(pred, obs) {
    return(sqrt( sum( (pred - obs)^2 )/length(pred) ) )
}

# calculate RMSE 
pred <- predict(pruned_model2, newdata=test.data2, type='vector')
print("Root Mean Squared Error")
rmse <- RMSE(pred, test.data2$wage_growth)
round(rmse, 4)

# prediction 1
print("Prediction for wage_growth:  economy='no_recession',  unemployment=3.4,  gdp=3.5")
newdata3 <- data.frame(economy='no_recession', unemployment=3.4, gdp=3.5)
predicted_wage_growth <- predict(pruned_model2, newdata3, type='vector')
round(predicted_wage_growth, 4)

# prediction 2
print("Prediction for wage_growth:  economy='recession',  unemployment=7.4,  gdp=1.5")
newdata4 <- data.frame(economy='recession', unemployment=7.4, gdp=1.5)
predicted_wage_growth <- predict(pruned_model2, newdata4, type='vector')
round(predicted_wage_growth, 4)
