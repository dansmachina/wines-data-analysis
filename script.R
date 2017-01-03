# Setting working directory

# Javier
setwd("/mnt/sda1/Dropbox/EIT/Intelligent Data Analysis/elective/wines-data-analysis")

df <- read.table("data10.txt",  header = FALSE)
str(df)

colnames(df) <- c("fixed_acidity", "volatile_acidity", "citic_acid", "residual_sugar", 
                  "chlorides", "free_sulfur_dioxide", "total_sulfur_dioxide", "density", 
                  "pH", "sulphates", "alcohol", "quality", "type")

# Quality and type are categorical variables.
df$quality <- as.factor(df$quality)
df$type <- as.factor(df$type)

# Logistic regression...
log.F=glm(type~fixed_acidity+volatile_acidity+citic_acid+residual_sugar+chlorides+free_sulfur_dioxide+total_sulfur_dioxide+density+ 
            pH+sulphates+alcohol, data=df, family=binomial)


log.B=glm(type~1,data=df,family=binomial)
Step.for=step(log.B, scope=list(upper=log.F),direction="forward")

log.m=glm(type~total_sulfur_dioxide+sulphates,data=df,family=binomial)
