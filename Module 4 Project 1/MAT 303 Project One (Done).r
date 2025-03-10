housing <- read.csv(file="housing_v2.csv", header=TRUE, sep=",")

# converting appropriate variables to factors  
housing <- within(housing, {
   view <- factor(view)
   backyard <- factor(backyard)
})

# number of columns
ncol(housing)

# number of rows
nrow(housing)

# Print the first six rows
print("head")
head(housing, 6)

plot(housing$sqft_living, housing$price, 
     main = "Scatterplot of price against living area in sq ft",
     xlab = "living area in sq ft", ylab = "price",
     col="red", 
     pch = 19, frame = FALSE)

plot(housing$age, housing$price, 
     main = "Scatterplot of price against age of home",
     xlab = "age of home", ylab = "price",
     col="red", 
     pch = 19, frame = FALSE)

myvars <- c("price","sqft_living", "age")
housing_subset <- housing[myvars]

# Print the correlation matrix
print("cor")
corr_matrix <- cor(housing_subset, method = "pearson")
round(corr_matrix, 4)


# Subsetting data to only include the variables that are needed
myvars <- c("price", "sqft_living", "sqft_above", "age", "bathrooms", "view")
housing_subset <- housing[myvars]

# Create the model
model1 <- lm(price ~ sqft_living + sqft_above + age + bathrooms + view, data=housing_subset)
summary(model1)

# fitted values for model 1
fitted_values <- fitted.values(model1)

# residuals for model 1
residuals <- residuals(model1)

plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     col="red", 
     pch = 19, frame = FALSE)

qqnorm(residuals, pch = 19, col="red", frame = FALSE)
qqline(residuals, col = "blue", lwd = 2)

newdata <- data.frame(sqft_living=2150, sqft_above=1050, age=15, bathrooms=3, view='0')

# Prediction 1 for living area of 2,150sq ft, sqft_above 1050, age 15, 3 bathrooms and road view
print("prediction interval")
prediction_pred_int <- predict(model1, newdata, interval="predict", level=0.90) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model1, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int, 4)

newdata <- data.frame(sqft_living=4250, sqft_above=2100, age=5, bathrooms=5, view='2') 

# Prediction 2 for living area of 4,250sq ft, sqft_above 2100, age 5, 5 bathrooms and lake view 
print("prediction interval")
prediction_pred_int <- predict(model1, newdata, interval="predict", level=0.90) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model1, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int, 4)

plot(housing$school_rating, housing$price, 
     main = "Scatterplot of price against school rating",
     xlab = "school rating", ylab = "price",
     col="red", 
     pch = 19, frame = FALSE)

plot(housing$crime, housing$price, 
     main = "Scatterplot of price against crime rate per 100,000 people",
     xlab = "crime rate", ylab = "price",
     col="red", 
     pch = 19, frame = FALSE)

# Subsetting data to only include the variables that are needed
print("Second Order Regression Model for Model 2")
myvars <- c("price", "school_rating", "crime")
housing_subset <- housing[myvars]

# Create the model
model2 <- lm(price ~ school_rating + crime + school_rating:crime + I(school_rating^2) + I(crime^2), data=housing_subset)
summary(model2)

# fitted values for model 2
fitted_values <- fitted.values(model2) 

# residuals for model 2
residuals <- residuals(model2)

plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values for model 2",
     xlab = "Fitted Values", ylab = "Residuals",
     col="red", 
     pch = 19, frame = FALSE)

qqnorm(residuals, pch = 19, col="red", frame = FALSE)
qqline(residuals, col = "blue", lwd = 2)

newdata <- data.frame(school_rating=9.80, crime=81.02)

# Prediction for school rating of 9.80 and a crime rate of 81.02 per 100,000 people
print("prediction interval")
prediction_pred_int <- predict(model2, newdata, interval="predict", level=0.90) 
round(prediction_pred_int,4)

print("confidence interval")
prediction_conf_int <- predict(model2, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int,4)

newdata <- data.frame(school_rating=4.28, crime=215.50) 

# Prediction for school rating of 4.28 and a crime rate of 215.50 per 100,000 people
print("prediction interval")
prediction_pred_int <- predict(model2, newdata, interval="predict", level=0.90) 
round(prediction_pred_int,4)

print("confidence interval")
prediction_conf_int <- predict(model2, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int,4)

# Subsetting data to only include the variables that are needed
print("Nested Model for Model 2")
myvars <- c("price", "school_rating", "crime")
housing_subset <- housing[myvars]

# Create the reduced model for model 2
model2_reduced <- lm(price ~ school_rating + crime + school_rating:crime, data=housing)
summary(model2_reduced)

# Perform the Nested Model F-test
anova(model2, model2_reduced)
