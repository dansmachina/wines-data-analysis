library(readr)

# Setting working directory
# Jose
setwd("/home/jose/repos/wines-data-analysis/src")

df <- read_delim("../data/processed/wines.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Quality and type are categorical variables.
#df$quality <- as.factor(df$quality)
df$type <- as.factor(df$type)

model1 = lm(quality~fixed_acidity+volatile_acidity+citic_acid
            +residual_sugar+chlorides+free_sulfur_dioxide
            +total_sulfur_dioxide+density+pH+sulphates+alcohol, data=df)

