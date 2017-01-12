library(datasets)
library(ggplot2)
ds <- diamonds

#1
ggplot(ds, aes(x=color,y=price, fill=color))+geom_boxplot()
ggplot(ds, aes(x=color,y=price, fill=color))+geom_boxplot()+ scale_y_log10()

#2
# TODO: Buscar binwidth adecuado
ggplot(data = ds, aes(x = price)) +geom_histogram(binwidth=300) + ggtitle("Histogram of price")+ scale_x_continuous(limits = c(0, 20000))
  geom_rect(mapping=aes(xmin=meanPrice, xmax=meanPrice+300, ymin=0, ymax=1200), fill='red', color="red", alpha=0.5) +
  geom_rect(mapping=aes(xmin=medianPrice, xmax=medianPrice+300, ymin=0, ymax=2000), fill='yellow', color="yellow", alpha=0.5)


# TODO: Calcular varianza. 
varPrice = var(ds$price)
varPrice
sqrt(varPrice)
# However, the mean is affected by extreme values so it may not be the best measure of center to use in a skewed distribution.
meanPrice = mean(ds$price)
meanPrice
# It is probably the best measure of center to use in a skewed distribution.
medianPrice = median(ds$price)
medianPrice

library("moments", lib.loc="~/R/win-library/3.3")
kurtosis(ds[,c(7)])
skewness(ds[,c(7)])


library(tseries)
jarque.bera.test(ds$price)
jarque.bera.test(log(ds$price))

# COMO LA MEJORA DE LOG ES MUY GRANDE, A PARTIR DE AHORA OPERAREMOS CON EL LOG DEL PRICE

library(MASS)
library(car)
boxcox(ds$price~1)

summary(powerTransform(ds$price))

par(mfrow=c(1,2))
qqPlot(ds$price, dist="norm", main="price vs. normality quantiles")
qqPlot(log(ds$price), dist="norm", main="log(price) vs. normality quantiles")

## OUTLIERS 
par(mfrow=c(1,1))
boxplot(ds$price, outcol="red", ylab="prince in US dollars") 
title('Boxplot of price')
#It gives you the observation classified as outlier
length(boxplot(ds$price)$out)

x <- setdiff(ds$price, boxplot(ds$price)$out)
newframe = ds[ds$price %in% x,]

ggplot(data = ds, aes(x = price)) +geom_histogram(data=subset(ds,price < 11000),  binwidth = 300) + 
  geom_histogram(data=subset(ds,price >= 11000),fill='red', alpha = 0.8, binwidth = 300) + ggtitle('Histogram of price without outliers') + 
  scale_x_continuous(limits = c(0, 20000))

jarque.bera.test(log(x))

# Conclusi?n: con el log mejora la normalidad, pero sigue sin ser normal. Hipotesis es que los outliers 
# impiden esta normalidad. Quitamos los outliers para comprobar si es normal y de esta manera
# si fuera normal podriamos usar los datos para modelos (por ejemplo de regresion) que requieran
# dicha condicion, y a pesar que mejora, sigue sin ser normal.
# Por lo tanto, decidimos no remover los outliers, 

# BIVARIATE ANALYSIS
library(ggplot2)
ggplot(ds, aes(x=carat,y=price))+geom_point()

library(MVN)
library(dplyr)
par(mfrow=c(1,2))
ds2 <- sample_n(ds, nrow(ds)*0.5)
mardiaTest(cbind(ds2$carat,log(ds2$price)), qqplot=T)
powerTransform(cbind(ds2$carat,log(ds2$price)))
rdoorT=bcPower(cbind(ds2$carat,log(ds2$price)), c(-0.103,0.4012))
mardiaTest(rdoorT, qqplot=T)
ds2$price2 <- log(ds2$price)

# OUTLIERS
mean=colMeans(ds2[,c(1, 11)])
#variables hbat[,6:18]
hbat618=ds2[,c(1, 11)]
# mean vector, cov matrix and squared mahalanobis distance
meanhbat618=sapply(hbat618, mean)
covhbat618=cov(hbat618)
mahalanobis618=mahalanobis(hbat618,meanhbat618,covhbat618)
mahalanobis618
# 95th percentile of a chi-squared distribution with 13 degrees of freedom (we are using 13 variables)
#Position of outliers
which(mahalanobis618 > qchisq(0.95,df=2))
#We got 6 outliers, their rows in the data set
pos=which(mahalanobis618 > qchisq(0.95,df=2))
pos
mahalanobis618[pos]

## To plot outliers in a different color
x=rep(1, 26970)
x[pos]=0
# We plot them on a scatterplot of variables 6 and 7 (they are outliers for the whole set of variables 6:18).
plot(ds2$carat,ds2$price2,col=x+2,pch=16) + title('Outliers based on Malahanobis distance')


library(mvoutlier)
corr.plot(ds2$carat, log(ds2$price))



#3
library(corrplot) # Used for the corrplots
library(RColorBrewer) # For the color palettes

# cex.before <- par("cex")
# par(cex = 0.85)

corrplot.mixed(r, col=brewer.pal(n=8, name="PuOr"),
               tl.col="black", title = "Pairwise correlation", 
               mar=c(2,0,2,0))
# par(cex=cex.before)


library(ppcor)
partial.corr <- pcor(ds[,c(1,5,7,8,9,10)])$estimate

#Visualizing partial correlations
corrplot.mixed(partial.corr, col=brewer.pal(n=8, name="PuOr"),
               tl.col="black", title = "Partial correlations", 
               mar=c(2,0,2,0))


cor(ds[,c(1,5,7,8,9,10)])
#Evidenemtente, la depth no tiene relacion lineal con x,y,z (mira formula)
# Precio y carat si
# Precio y x,y,z tambien (debido al tama?o?)

library(RcmdrMisc)
rcorr.adjust(ds[,c(1,5,7,8,9,10)])

library(ppcor)
pcor(ds[,c(1,5,7,8,9,10)])

# como esperabamos, x,y,z desaparece relacion. El precio y carat tiene algo de correlacion, pero no tanto
#como esperabamos, probablemente porque influye tambien quality y cut.

r2multv<-function(x){
  r2s=1-1/(diag(solve(cov(x)))*diag(cov(x)))
  r2s
}

#use it on data set "medifis"
r2multv(ds[,c(1,5,7,8,9,10)])
          

1-det(cor(ds[,c(1,5,7,8,9,10)]))^(1/6)

eigen(cor(ds[,c(1,5,7,8,9,10)]))
cor(ds[,c(1,8,9,10)])

# el mas peque?o no esta tan pegado a 0 como queremos, pero lo analizamos y vemos que hay unar elacion lineal
# entre tama?o, y el x y z