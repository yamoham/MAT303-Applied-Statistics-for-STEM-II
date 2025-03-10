
economic <- read.csv(file='economic.csv', header=TRUE, sep=",")

# Print the first six rows
print("head")
head(economic, 6)

plot(economic$unemployment, economic$wage_growth, 
     main = "Scatterplot of Wage Growth and Unemployment",
     xlab = "Unemployment", ylab = "Wage Growth",
     col="red", 
     pch = 19, frame = FALSE)

# Create the second order regression model and print the statistics
model1 <- lm(wage_growth ~ unemployment + I(unemployment^2), data=economic)
summary(model1)

newdata <- data.frame(unemployment=2.54)

print("prediction interval")
prediction_pred_int <- predict(model1, newdata, interval="predict", level=0.95) 
round(prediction_pred_int,4)

print("confidence interval")
prediction_conf_int <- predict(model1, newdata, interval="confidence", level=0.95) 
round(prediction_conf_int,4)

# Create the second order regression model and print the statistics
model2 <- lm(wage_growth ~ unemployment + gdp + unemployment:gdp + I(unemployment^2) + gdp + I(gdp^2) , data=economic)
summary(model2)

newdata <- data.frame(unemployment=2.50, gdp=6.50)

print("prediction interval")
prediction_pred_int <- predict(model2, newdata, interval="predict", level=0.95) 
round(prediction_pred_int,4)

print("confidence interval")
prediction_conf_int <- predict(model2, newdata, interval="confidence", level=0.95) 
round(prediction_conf_int,4)

# Create the second order regression model and print the statistics
model3 <- lm(wage_growth ~ unemployment + economy + unemployment:economy + I(unemployment^2) + I(unemployment^2):economy, data=economic)
summary(model3)

newdata <- data.frame(unemployment=2.50, economy='no_recession')

print("prediction interval")
prediction_pred_int <- predict(model3, newdata, interval="predict", level=0.95) 
round(prediction_pred_int,4)

print("confidence interval")
prediction_conf_int <- predict(model3, newdata, interval="confidence", level=0.95) 
round(prediction_conf_int,4)