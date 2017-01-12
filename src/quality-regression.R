library(readr)
library(dplyr)
library(tseries)

# Setting working directory
# Jose
setwd("/home/jose/repos/wines-data-analysis/src")

df <- read_delim("../data/processed/wines.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Quality and type are categorical variables.
#df$quality <- as.factor(df$quality)
df$type <- as.factor(df$type)

# Train and test dataset, split 80% (data has been shuffled previously, no need to sample randomly)
split = nrow(df)*0.8
train = df[1:split,]
test = df[split:nrow(df),]

# Fit the model using all vars
model1 = lm(quality~fixed_acidity+volatile_acidity+citic_acid
            +residual_sugar+chlorides+free_sulfur_dioxide
            +total_sulfur_dioxide+density+pH+sulphates+alcohol, data=train)
summary(model1)

# Citic_acid no ayuda, fuera
model1 = update(model1, ~.-citic_acid)
summary(model1)

# Chlorides fuera tambien
model1 = update(model1, ~.-chlorides)
summary(model1)

# "... but the p-value of R2 is still <0.05 so we are at least 95% confident that a relationship 
# does exist between at least some of the input variables and the quality ranking."

# 2. Assess the model. A: Check the assumptions

jarque.bera.test(residuals(model1))
# Answer: No normality

# Equal variances
library(lmtest)
bptest(model1) # No constant variance

dwtest(model1, alternative="two.sided") # Not independent
Box.test(residuals(model1)) # SI INDEPENDIENTES SEGUN ESTE TEST

# Residuals
par(mfrow=c(2,2))
plot(model1, which=c(1:4), ask=F)

# Un ojo a ese outlier gigante

# Conclusiones: en cuanto a hipotesis nuestro modelo es bastante mierda. Ademas parece que esta bien
# para qualitys entre 4 y 6 mas o menos, los valores mas grandes van mal. Esto es normal porque la
# muestra que tenemos es bastante mala (hacer histograma)

# 2. Assess the model. B: Evaluate performance
predicted <- predict(model1, test)

# Percentage of correct predictions aka accuracy (we are rounding the predicted variable)
# Around 50% are correct - not bad
sum(test$quality == round(predicted))/nrow(test)

# But the above is a simplification, we are treating quality as a continuous var, so lets check
# more conventional metrics, in this case rmse
rmse <- (sqrt(mean(test$quality - predicted)^2))
rmse

# Pseudo rmse (after discretizing the output var by rounding)
# Aumenta bastante, pero ahora - yo diria - es una cifra mas indicativa
rmse.discret <- (sqrt(mean((test$quality - round(predicted))^2)))
rmse.discret

# Conclusiones: No funciona mal en cuanto a resultados, pero no nos fiamos, entre que los
# datos son malillos (en cuanto a distribucion) y que no hemos usado la tecnica adecuada (ver abajo)
# En cualquier caso, posiblemente seria mejor usar Logistic Regression (o algun metodo de clasificacion)
# para predecir la quality, que realmente no es continua.