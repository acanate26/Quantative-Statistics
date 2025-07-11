####Research Question 1
setwd("~/Downloads/QMSS Statistics II")
require(data.table)
list.files()
install.packages("readxl")
library(readxl)
Final1 <- read_excel("variables_4_analysis_clusters_proj.csv.xlsx")
head(Final1, n=10)
summary(Final1)

##Regression on the Social Wellbeing
lm1 <- lm(SOC_WELL  ~ INDEXRT + METRO + SOUTH, data = Final1)
summary(lm1)

lm1.res <- resid(lm1)
lm1.fit <- fitted(lm1)

plot(lm1.fit, lm1.res,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")
summary(lm1)

#install.packages("olsr r")
require(olsrr)
ols_vif_tol(lm1)

library(spdep)
nb <- poly2nb(COUNT)
moran.test(residuals(lm1), W)

###1) Yeah, the spatial regression is fine

####2)Yes, the crime rate, based on the results, significantly predicts the 
###level of the social well-being. The negative coefficients means that the 
##higher crime rates are associated with lower well being. The crime rate is a 
###highly significant and negative predictor of county-level social
###well-being in the model.

###3) The spatial lag variable is used as intended, helping to remove the 
##correlation 










