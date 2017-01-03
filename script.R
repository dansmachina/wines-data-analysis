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
