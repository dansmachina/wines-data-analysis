# Load libraries
library(readr)
library(dplyr)
library(tseries)
library(lmtest)

# Load dataset
df <- read_delim("../data/processed/wines.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Train and test dataset, split 80% (data has been shuffled previously, no need to sample randomly)
split <- nrow(df)*0.8
train <- df[1:split,]
test <- df[split:nrow(df),]

# Fit the model using all variables
model1 <- lm(quality~fixed_acidity+volatile_acidity+citric_acid
             +residual_sugar+chlorides+free_sulfur_dioxide
             +total_sulfur_dioxide+density+pH+sulphates+alcohol, data=train)
summary(model1)

# Citric_acid removed from the model
model1 <- update(model1, ~.-citric_acid)
# Check if chlorides can also be removed
summary(model1)

# It can indeed be removed, do it
model1 <- update(model1, ~.-chlorides)
summary(model1)

# Check the assumptions
# Linearity
raintest(model1) # Yes, linear

# Test normality
jarque.bera.test(residuals(model1)) # Answer: No normality

# Equal variances
bptest(model1) # No constant variance

# Testing independence of the residuals
dwtest(model1, alternative="two.sided") # Not independent

# Residuals
par(mfrow=c(2,2))
plot(model1, which=c(1:4), ask=F)

sd(df$quality)

# Assess the model
predicted <- predict(model1, test)
rmse <- (sqrt(mean((test$quality - predicted)^2)))
rmse

## Alternative model based on the sweetness of wines

# Adding the new attribute
df$residual_sugar2 <- df$residual_sugar
df$residual_sugar2[df$residual_sugar < 4.0] <- "dry"
df$residual_sugar2[df$residual_sugar >= 4.0 & df$residual_sugar < 12.0] <- "medium dry"
df$residual_sugar2[df$residual_sugar >= 12.0] <- "medium"

df$residual_sugar2 <- as.factor(df$residual_sugar2)
df$residual_sugar2 <- relevel(df$residual_sugar2, ref="dry")

# Update train and test sets 
train <- df[1:split,]
test <- df[split:nrow(df),]

# New model
model2 <- update(model1, ~.+residual_sugar2+residual_sugar:residual_sugar2)
summary(model2)

# Test linearity
raintest(model2) # Yes, linear

# Test normality
jarque.bera.test(residuals(model2)) # Answer: No normality

# Equal variances
bptest(model2) # No constant variance

# Testing independence of the residuals
dwtest(model2, alternative="two.sided") # Not independent

# Assess the model
predicted2 <- predict(model2, test)
rmse <- (sqrt(mean((test$quality - predicted2)^2)))
rmse