library(readr)

# Setting working directory
# Javier
setwd("/mnt/sda1/Dropbox/EIT/ida-Intelligent_Data_Analysis/elective/wines-data-analysis/src")

df <- read_delim("../data/processed/wines.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Train and test dataset, split 80%.
split = nrow(df)*0.8
train = df[1:split,]
test = df[split:nrow(df),]

# Logistic regression model with all the variables.
log.F=glm(type~fixed_acidity+volatile_acidity+citic_acid+residual_sugar+chlorides+free_sulfur_dioxide+total_sulfur_dioxide+density+ 
            pH+sulphates+alcohol, data=train, family=binomial)

# Simplest logistic regression model
log.B=glm(type~1, data=train, family=binomial)

#Stepwise algorithm
Step.for=step(log.B, scope=list(upper=log.F), direction="forward")

log.m=glm(type~total_sulfur_dioxide + density + residual_sugar + alcohol + 
            volatile_acidity + chlorides + free_sulfur_dioxide + sulphates + 
            citic_acid,data=train,family=binomial)

summary(log.m)#AIC 451

# p-value for overall test
pchisq(404.08, 5835, lower=F)

# Simpler model.
log.m2=glm(type ~ total_sulfur_dioxide + density + residual_sugar + alcohol + 
             volatile_acidity + chlorides, data=train, family=binomial)
log.m2=glm(type ~ total_sulfur_dioxide + density, data=train, family=binomial)
summary(log.m2)# AIC 490

# Comparing models
anova(log.m2,log.m, test="Chisq")
# p-value supports model 1, but maybe model 2 more understable?

# Train error
train_prob=log.m2$fitted
train_prob=ifelse(train_prob>0.5,1,0)
train_confusion = table(train_prob, train$type)

confusion = train_confusion
c = confusion[1]+confusion[4]
t = c + confusion[2] + confusion[3]
c/t

# Test error
test_prob = predict(log.m2, newdata = test, type = "response")
test_prob = ifelse(test_prob>0.5,1,0)
test_confusion = table(test_prob, test$type)

confusion = test_confusion
c = confusion[1]+confusion[4]
t = c + confusion[2] + confusion[3]
c/t
