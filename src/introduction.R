# Load libraries
library(readr)
library(ggplot2)
library(corrplot)
library(ppcor)
library(RColorBrewer)
library(mvoutlier)

## Preprocessing 

# Load individual datasets
white <- read_delim("../data/raw/winequality-white.csv",";", escape_double = FALSE, trim_ws = TRUE)
red <- read_delim("../../data/raw/winequality-red.csv",";", escape_double = FALSE, trim_ws = TRUE)

# Create a new column, where 0 indicates white wine and 1 red wine.
white$type = 0
red$type = 1

# Combine both datasets.
df <- rbind(white, red)

# Rename columns.
colnames(df) <- c("fixed_acidity", "volatile_acidity", "citric_acid", "residual_sugar", 
                  "chlorides", "free_sulfur_dioxide", "total_sulfur_dioxide", "density", 
                  "pH", "sulphates", "alcohol", "quality", "type")

# Quality and type are categorical variables.
df$quality <- as.factor(df$quality)
df$type <- as.factor(df$type)

# Remove missing values.
df <- df[complete.cases(df), ] # two entries.

# Shuffle dataset.
set.seed(111)
df <- df[sample(nrow(df)),]

# Write dataset into csv file.
write.table(df, file="../data/processed/wines.csv", sep=";", col.names = TRUE, row.names = FALSE)

## Multivarite Descriptive Statistics

# Loading the dataset into a dataframe
df <- read_delim("../data/processed/wines.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Summary of the variables.
summary(df)

# Create types variable to represent their distribution.
type.0 = subset(df, type==0)
type.1 = subset(df, type==1)
type.0$type <- 'white'
type.1$type <- 'red'
types <- rbind(type.0, type.1)
types$type = as.factor(types$type)

# Types barplot
ggplot(types, aes(type, ..count..)) + geom_bar(aes(fill=type)) + scale_fill_manual(values=c("red","white")) +
  theme(panel.background = element_rect(fill = 'grey', colour = 'black'), plot.title= element_text(hjust = 0.5))

# Quality barplot by types.
ggplot(types, aes(quality))  + geom_histogram(aes(fill=type), bins=6) + 
  scale_fill_manual(values=c("red","white")) + ylab("Wine samples") + xlab("Quality") + facet_grid(. ~ type) +
  theme(panel.background = element_rect(fill = 'grey', colour = 'black'), plot.title= element_text(hjust = 0.5))

# Correlation analysis
r <- cor(df[,1:11])
partial.corr <- pcor(df[,1:11])$estimate
corrplot(r, method="ellipse")

# Function to calculate the coefficient of determination
r2multv<-function(x){
  r2s=1-1/(diag(solve(cov(x)))*diag(cov(x)))
  r2s
}

r2multv(df[,1:11])

# Effective dependence coefficient
1-det(cor(df[,1:11]))^(1/11)