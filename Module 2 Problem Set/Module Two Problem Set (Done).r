
# Loading mtcars data set from a mtcars.csv file
mtcars <- read.csv(file='mtcars.csv', header=TRUE, sep=",")

# Converting appropriate variables to factors  
mtcars2 <- within(mtcars, {
   vs <- factor(vs)
   am <- factor(am)
   cyl  <- factor(cyl)
   gear <- factor(gear)
   carb <- factor(carb)
})

myvars <- c("mpg","hp","qsec","drat")
mtcars_subset <- mtcars2[myvars]

# Print the correlation matrix
print("cor")
corr_matrix <- cor(mtcars_subset, method = "pearson")
round(corr_matrix, 4)

model1 <- lm(mpg ~ hp + qsec + drat  + hp:qsec + hp:drat, data=mtcars_subset)
summary(model1)

# Subsetting data to only include the variables that are needed
myvars <- c("mpg","hp","qsec","drat")
mtcars_subset <- mtcars2[myvars]

# predicted values
print("fitted for model 1")
fitted_values <- fitted.values(model1) 
fitted_values

# residuals
print("residuals for model 1")
residuals <- residuals(model1)
residuals

plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values Model 1",
     xlab = "Fitted Values", ylab = "Residuals",
     col="red", 
     pch = 19, frame = FALSE)

qqnorm(residuals, pch = 19, col="red", frame = FALSE)
qqline(residuals, col = "blue", lwd = 2)

# confidence intervals for model parameters
print("confint")
conf_95_int <- confint(model1, level=0.95) 
round(conf_95_int, 4)

newdata <- data.frame(hp=175, qsec=14.2, drat=3.91)

print("prediction interval")
prediction_pred_int <- predict(model1, newdata, interval="predict", level=0.95) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model1, newdata, interval="confidence", level=0.95) 
round(prediction_conf_int, 4)


myvars <- c("mpg","hp","qsec", "cyl")
mtcars_subset <- mtcars2[myvars]

# Create the model
model2 <- lm(mpg ~ hp + qsec + hp:qsec + cyl, data=mtcars_subset)
summary(model2)

# predicted values
print("fitted for model 2")
fitted_values <- fitted.values(model2) 
fitted_values

# residuals
print("residuals for model 2")
residuals <- residuals(model2)
residuals

plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values Model 2",
     xlab = "Fitted Values", ylab = "Residuals",
     col="red", 
     pch = 19, frame = FALSE)

qqnorm(residuals, pch = 19, col="red", frame = FALSE)
qqline(residuals, col = "blue", lwd = 2)

# confidence intervals for model parameters
print("confint")
conf_95_int <- confint(model2, level=0.95) 
round(conf_95_int, 4)

newdata <- data.frame(hp=175, qsec=14.2, cyl='6')

print("prediction interval")
prediction_pred_int <- predict(model2, newdata, interval="predict", level=0.95) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model2, newdata, interval="confidence", level=0.95) 
round(prediction_conf_int, 4)