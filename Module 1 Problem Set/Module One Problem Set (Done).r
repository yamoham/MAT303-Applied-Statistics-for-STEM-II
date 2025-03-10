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

# Create scatterplot 1 mpg vs drat
plot(mtcars2$drat, mtcars$mpg, 
     main = "Scatterplot of Fuel Efficiency (mpg) vs Rear Axle Ratio (drat)",
     xlab = "Rear Axle Ratio", ylab = "Fuel Efficiency",
     xlim=c(1, 6),
     ylim=c(0, 40),
     col="red", 
     pch = 19, frame = FALSE)

# Create scatterplot 2 mpg vs hp
plot(mtcars2$hp, mtcars$mpg, 
     main = "Scatterplot of Fuel Efficiency (mpg) vs Horse Power (hp)",
     xlab = "Horse Power", ylab = "Fuel Efficiency",
     xlim=c(0, 400),
     ylim=c(0, 40),
     col="red", 
     pch = 19, frame = FALSE)

# Selecting mpg, drat, and hp variables to subset the data
myvars <- c("mpg","drat","hp")
mtcars_subset <- mtcars2[myvars]

# Print the correlation matrix
print("cor")
corr_matrix <- cor(mtcars_subset, method = "pearson")
round(corr_matrix, 4)

# Create the multiple regression model and print the statistics
model <- lm(mpg ~ drat + hp, data=mtcars_subset)
summary(model)

#fitted values
fitted_values <- fitted(model) 
fitted_values

#residuals
residuals <- residuals(model)
residuals

#residuals against fitted values
plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     col="red", 
     pch = 19)

qqnorm(residuals, pch = 19, col="red", frame = FALSE)
qqline(residuals, col = "blue", lwd = 2)

# confidence intervals for model parameters
print("confint")
conf_95_int <- confint(model, level=0.95) 
round(conf_95_int, 4)

# prediction and confidence interval for drat = 3.15 and hp = 120
newdata <- data.frame(drat=3.15, hp=120)

print("prediction interval")
prediction_pred_int <- predict(model, newdata, interval="predict", level=0.95) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model, newdata, interval="confidence", level=0.95) 
round(prediction_conf_int, 4)