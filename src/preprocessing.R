library(readr)

# Setting working directory
# Javier
setwd("/mnt/sda1/Dropbox/EIT/ida-Intelligent_Data_Analysis/elective/wines-data-analysis/src")

white <- read_delim("../data/raw/winequality-white.csv",";", escape_double = FALSE, trim_ws = TRUE)
white$type = 0

red <- read_delim("../data/raw/winequality-red.csv",";", escape_double = FALSE, trim_ws = TRUE)
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

#shuffle dataset
df <- df[sample(nrow(df)),]

# Write dataset into csv file.
write.table(df, file="../data/processed/wines.csv", sep=";", col.names = TRUE, row.names = FALSE)

