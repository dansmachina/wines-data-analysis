# wines-data-analysis: Wines quality and classification

The dataset to analyze is composed of the red and white variants of the Portuguese "Vinho verde" wine. In total, there are 6495 different instances of wines, where 4898 are white wine and 1597 red wine. On the other hand, the dataset has 12 variables based on physicochemical tests on different wines plus two categorical variables, one for grading the wine quality by experts between 0 (very bad) and 10 (very excellent) and another binary variable, 0 = white wine and 1 = red wine. The goal of this project is a) to predict the quality of a wine and b) to classify whether a new wine is red or white, given its attributes.

Installation
----------- 
Dependencies:
````
install.packages(readr)
install.packages(ggplot2)
install.packages(corrplot)
install.packages(ppcor)
install.packages(RColorBrewer)
install.packages(mvoutlier)
install.packages(dplyr)
install.packages(tseries)
install.packages(lmtest)
````

Statistical Learning models
----------- 
We have applied different versions of statistical learning models in order to solve both classification and regression problems:

* [Logistic Regression](https://stat.ethz.ch/R-manual/R-patched/install.packages/stats/html/glm.html): the function <i>glm()</i> fits generalized linear models, concretely a logistic regression model when the family is binomial. We propose two different approaches depending on the aim of the study: a) best interpretation and b) best classification.

* [Linear Regression](https://stat.ethz.ch/R-manual/R-devel/install.packages/stats/html/lm.html): the function <i>lm()</i> is used to fit linear models. We propose two different linear regression model: a) one with the original variables and b) a new categorical variable according to the sweetness classification.


