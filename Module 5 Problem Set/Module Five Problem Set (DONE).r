# Loading R packages that are needed for some calculations below
install.packages("ResourceSelection")
install.packages("pROC")

# Loading credit card default data set
credit_default <- read.csv(file='credit_card_default.csv', header=TRUE, sep=",")

# Converting appropriate variables to factors  
credit_default <- within(credit_default, {
   default <- factor(default)
   sex <- factor(sex)
   education <- factor(education)
   marriage <- factor(marriage)
   assets <- factor(assets)
   missed_payment <- factor(missed_payment)
})

print("head")
head(credit_default, 6)


# number of columns
print("Number of Columns")
ncol(credit_default)

# number of rows
print("Number of rows")
nrow(credit_default)


# Create the complete model
print("Model 1")
logit <- glm(default ~ credit_utilize + education , data = credit_default, family = "binomial")
summary(logit)



# Hosmer-Lemeshow Goodness of Fit (GOF) Test for Model 1
library(ResourceSelection)
print("Hosmer-Lemeshow Goodness of Fit Test")
hl = hoslem.test(logit$y, fitted(logit), g=50)
hl


# Predict default or no_default for the data set using the model
default_model_data <- credit_default[c('education', 'credit_utilize')]
pred <- predict(logit, newdata=default_model_data, type='response')

# If the predicted probability of default is >=0.50 then predict credit default (default='1'), otherwise predict no credit 
# default (default='0') 
depvar_pred = as.factor(ifelse(pred >= 0.5, '1', '0'))

# This creates the confusion matrix
conf.matrix <- table(credit_default$default, depvar_pred)[c('0','1'),c('0','1')]
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ": default=")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix), sep = ": default=")

# Print nicely formatted confusion matrix
print("Confusion Matrix")
format(conf.matrix,justify="centre",digit=2)


# Receiver Operating Characteristic (ROC) Curve
library(pROC)
labels <- credit_default$default
predictions <- logit$fitted.values

roc <- roc(labels ~ predictions)

print("Area Under the Curve (AUC)")
round(auc(roc),4)

print("ROC Curve")

# True Positive Rate (Sensitivity) and False Positive Rate (1 - Specificity)
plot(roc, legacy.axes = TRUE)


print("Prediction: education is high school (education='1'), credit utilization is 35% (credit_utilize=0.35)")
newdata1 <- data.frame(education="1", credit_utilize=0.35)
pred1 <- predict(logit, newdata1, type='response')
round(pred1, 4)

print("Prediction: education is postgraduate (education='3'), credit utilization is 35% (credit_utilize=0.35)")
newdata2 <- data.frame(education="3", credit_utilize=0.35)
pred2 <- predict(logit, newdata2, type='response')
round(pred2, 4)


# Create the second model
print("Model 2")
logit2 <- glm(default ~ credit_utilize + assets + missed_payment, data = credit_default, family = "binomial")
summary(logit2)


# Hosmer-Lemeshow Goodness of Fit (GOF) Test for Model 2
print("Hosmer-Lemeshow Goodness of Fit (GOF) Test for Model 2")
library(ResourceSelection)
hl = hoslem.test(logit2$y, fitted(logit2), g=50)
hl


# Predict default or no_default for the data set using the model
default_model_data <- credit_default[c('credit_utilize', 'assets', 'missed_payment')]
pred <- predict(logit2, newdata=default_model_data, type='response')

# If the predicted probability of default is >=0.50 then predict credit default (default='1'), otherwise predict no credit 
# default (default='0')
depvar_pred = as.factor(ifelse(pred >= 0.5, '1', '0'))

# This creates the confusion matrix
conf.matrix <- table(credit_default$default, depvar_pred)[c('0','1'),c('0','1')]
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ": ")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix), sep = ": ")

# print nicely formatted confusion matrix
print("Confusion Matrix")
format(conf.matrix,justify="centre",digit=2)


# Receiver Operating Characteristic (ROC) Curve
library(pROC)
labels <- credit_default$default
predictions = logit2$fitted.values

roc <- roc(labels ~ predictions)

print("Area Under the Curve (AUC)")
round(auc(roc),4)

print("ROC Curve")

# True Positive Rate (Sensitivity) and False Positive Rate (1 - Specificity)
plot(roc, legacy.axes = TRUE)


print("Prediction: credit utilization is 35% (credit_utilize=0.35), owns a car (assets='1'), missed payment (missed_payment='1')")
newdata3 <- data.frame(credit_utilize=0.35, assets='1', missed_payment='1')
prediction1 <- predict(logit2, newdata3, type='response')
round(pred1, 4)

print("Prediction: credit utilization is 35% (credit_utilize=0.35), owns a car and a house (assets='3'), missed payment (missed_payment='0')")
newdata4 <- data.frame(credit_utilize=0.35, assets='3', missed_payment='0')
prediction2 <- predict(logit2, newdata4, type='response')
round(pred2, 4)