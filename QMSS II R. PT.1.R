setwd("~/Downloads/QMSS Statistics II")
require(data.table)
Mterm_data <- fread("Midterm_Data.csv", 
              stringsAsFactors = F, 
              data.table = F)
summary(Mterm_data)
head(Mterm_data)

##Research Questions 1
burg_cor <- cor(Mterm_data[,c('burg_rt', 'pctump', 'medhsin', 'South_region')])
burg_cor 

install.packages("PerformanceAnalytics")
require(PerformanceAnalytics)

chart.Correlation(Mterm_data[,c('burg_rt', 'pctump', 'medhsin', 'South_region')], 
                  histogram = T, 
                  method = "pearson")

# Path analysis with lavaan package
install.packages("lavaan")
require(lavaan)

# specification of 1st model 

model1 <- '
burg_rt  ~ a * medhsin
pctump   ~ b * South_region
'

# object with estimation of parameters
fit_model1 <- sem(model1, data = Mterm_data)

# check results (with undstandardized coefficients)
summary(fit_model1)

# to include standardized coeffcients

summary(fit_model1, 
        standardized = T, 
        fit.measures = T, 
        rsq = T)

##Model 2
model2 <- '
burg_rt      ~ a * pctump
South_region ~ b * burg_rt + c * medhsin

# Define indirect and total effects
direct  := b
indirect := a * c 
total := b + (a * c)
'

# create model with estimated parameters
fit_model2 <- sem(model2, data = Mterm_data)

# check model with new specification
summary(fit_model2, 
        standardized=T, 
        fit.measures=T, 
        rsq=T)

######The estimated regression coefficient for the burglary rate against the 
###independent variables percent unemployed is approximately 5.211, and the 
###standard coefficient of about 0.251, which is statistically significant. This 
###suggests that a one-unit increase in the percentage unemployment with an 
###average increase of burglary rates. A higher median household income is linked
###to a lower probability of being in the Southern region.  

#Research Question 2
summary(Mterm_data)
head(Mterm_data)

##Research Questions 1
sex_cor <- cor(Mterm_data[,c('rape_rt', 'pmarried', 'urb_contin')])
sex_cor

#install.packages("psych")
#install.packages("GPArotation")
require(psych)
require(GPArotation)

alpha <- alpha(sex_cor)
alpha <- alpha$total$raw_alpha
alpha

factors_crime <- factanal(crime_vars, 
                          factors = 3, 
                          scores = "regression")
factors_crime$loadings




