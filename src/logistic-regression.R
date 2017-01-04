library(readr)

# Setting working directory
# Javier
setwd("/mnt/sda1/Dropbox/EIT/ida-Intelligent_Data_Analysis/elective/wines-data-analysis/src")

df <- read_delim("../data/processed/wines.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Logistic regression...
log.F=glm(type~fixed_acidity+volatile_acidity+citic_acid+residual_sugar+chlorides+free_sulfur_dioxide+total_sulfur_dioxide+density+ 
            pH+sulphates+alcohol, data=df, family=binomial)


log.B=glm(type~1, data=df, family=binomial)
Step.for=step(log.B, scope=list(upper=log.F), direction="forward")

log.m=glm(type~total_sulfur_dioxide + density + residual_sugar + alcohol + 
            volatile_acidity + chlorides + free_sulfur_dioxide + sulphates + 
            citic_acid,data=df,family=binomial)

summary(log.m)#AIC 451

# p-value for overall test
pchisq(432.42, 6485, lower=F)

# Simpler model.
log.m2=glm(type ~ total_sulfur_dioxide + density + residual_sugar + alcohol + 
             volatile_acidity + chlorides, data=df, family=binomial)
summary(log.m2)# AIC 490

# Comparing models
anova(log.m2,log.m, test="Chisq")
# p-value supports model 1, but maybe model 2 more understable?

myprob=log.m$fitted
myprob=ifelse(myprob>0.5,1,0)
table(myprob, df$type)