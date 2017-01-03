# Setting working directory

# Javier
setwd("/mnt/sda1/Dropbox/EIT/Intelligent Data Analysis/elective/wines-data-analysis")

white <- read_delim("data/winequality-white.csv",";", escape_double = FALSE, trim_ws = TRUE)
white$type = 0

red <- read_delim("data/winequality-red.csv",";", escape_double = FALSE, trim_ws = TRUE)
red$type = 1

df <- rbind(white, red)
colnames(df) <- c("fixed_acidity", "volatile_acidity", "citic_acid", "residual_sugar", 
                     "chlorides", "free_sulfur_dioxide", "total_sulfur_dioxide", "density", 
                     "pH", "sulphates", "alcohol", "quality", "type")


# Remove missing values
df <- df[complete.cases(df), ]
# Quality and type are categorical variables.
df$quality <- as.factor(df$quality)
df$type <- as.factor(df$type)

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

log.m2=glm(type ~ total_sulfur_dioxide + density + residual_sugar + alcohol + 
             volatile_acidity + chlorides, data=df, family=binomial)
summary(log.m2)# AIC 490

# Comparing models
anova(log.m2,log.m, test="Chisq")
# p-value supports model 1, but maybe model 2 more understable?

myprob=log.m$fitted
myprob=ifelse(myprob>0.5,1,0)
table(myprob, df$type)


r2multv<-function(x){
  r2s=1-1/(diag(solve(cov(x)))*diag(cov(x)))
  r2s
}

# function myvif
myvif<-function(x){
  myvif=1/(1-r2multv(x))
  myvif
  sort(myvif, decreasing=TRUE)
}

myvif(cbind(df$total_sulfur_dioxide, df$density, df$residual_sugar, df$alcohol, 
              df$volatile_acidity,  df$chlorides,  df$free_sulfur_dioxide,  df$sulphates, 
              df$citic_acid))
