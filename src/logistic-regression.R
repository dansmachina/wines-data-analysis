# Load libraries
library(readr)
library(ggplot2)

# Load the dataset into a dataframe
df <- read_delim("../../data/processed/wines.csv", 
                 ";", 
                 escape_double = FALSE, 
                 trim_ws = TRUE)

# Train and test dataset, split 80%.
split <- nrow(df)*0.8
train <- df[1:split,]
test <- df[split:nrow(df),]

# Logistic regression model with all the variables.
log.full <- glm(type~fixed_acidity+volatile_acidity
                +citric_acid+residual_sugar+chlorides
                +free_sulfur_dioxide+total_sulfur_dioxide+density
                +pH+sulphates+alcohol, data=train, family=binomial)
summary(log.full)

# Logistic regression model with all the signiticant variables.
log.F <- glm(type~fixed_acidity+volatile_acidity
             +residual_sugar+chlorides
             +free_sulfur_dioxide+total_sulfur_dioxide+density
             +pH+alcohol, data=train, family=binomial)
summary(log.F)

# Overall fit of the model
pchisq(log.F$deviance, log.F$df.residual, lower=F)

# Accuracy definition.
accuracy <- function(model, train, test){
  # Train error
  train_prob <- model$fitted
  train_prob <- ifelse(train_prob>0.5,1,0)
  d_train <- table(train_prob, train$type)
  
  # Test error
  test_prob <- predict(model, newdata = test, type = "response")
  test_prob <- ifelse(test_prob>0.5,1,0)
  d_test <- table(test_prob, test$type)
  
  train_accuracy <- sum(diag(d_train))/sum(d_train)
  test_accuracy <- sum(diag(d_test))/sum(d_test)
  
  return(list("train" = train_accuracy, "test" = test_accuracy))
}

# Validation of the full model
log.F.error <- accuracy(log.F, train, test)

# Train error
log.F.error$train

# Test error
log.F.error$test

# Predicting all red wines model.
log.B <- glm(type~1, data=train, family=binomial)
accuracy(log.B, train, test)

#Stepwise algorithm
step(log.B, scope=list(upper=log.F), direction="forward", step=1)

# Model with one variable.
log.1 <- glm(type~total_sulfur_dioxide, data=train, family=binomial)
accuracy(log.1, train, test)

# Correlation of the variables.
cor(df, df$type)

#Stepwise algorithm
step(log.1, scope=list(upper=log.F), direction="forward", step=1)

# Model with 2 variables.
log.2 <- glm(type~total_sulfur_dioxide+density, data=train, family=binomial)
accuracy(log.2, train, test)
summary(log.2)

# Tradeoff between number of variables in the model and accuracy.
x = 0:9
y = c(0.75, 0.92, 0.95, 0.98, 0.99, 0.997, 0.997, 0.997, 0.997, 0.997)
models <- cbind.data.frame(x,y)
spline_model <- as.data.frame(spline(models$x, models$y))

plot <- ggplot(models,aes(x=x, y=y)) +  geom_line(data = spline_model, aes(x = x, y = y)) +
  geom_point() + scale_x_discrete(limit = x)

plot + labs(x="Number of variables",y="Accuracy") + 
  ggtitle("Accuracy vs. number of variables in logistic regression model") +
  theme(plot.title= element_text(hjust = 0.5))

# Model with 5 variables.
log.3 <- glm(type~total_sulfur_dioxide+density+residual_sugar+alcohol+volatile_acidity, data=train, family=binomial)
summary(log.3)

# Scatterplot of sulfur and density coloured by type.
ggplot(train, aes(x=total_sulfur_dioxide, y=density))+geom_point(aes(colour=factor(type))) + 
  scale_color_manual(values=c("white","red")) +
  theme(panel.background = element_rect(fill = 'grey', colour = 'black'), plot.title= element_text(hjust = 0.5)) +
  ggtitle("Plot total SO2 vs. density and coloured by type.")