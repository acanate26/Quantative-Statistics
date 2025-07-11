##Research Question 2 
setwd("~/Downloads/QMSS Statistics II")
require(data.table)
list.files()
library(haven)
mydata1 <- read_dta("lvl1.dta")
mydata2 <-read_dta("lvl2.dta")
colnames("lvl1.dta")

names(mydata1)
names(mydata2)
head(mydata1)
head(mydata2)

Final2 <- merge(mydata1, mydata2, by = "psu")

model <- glm(capun ~ white + femal, data = Final2, family = binomial)
summary(model)

###According to the model's results, white correspondents are 3.5 times more likely
###to support the death penalty than non-white correspondents, holding gender 
##constant. And women are 36% less likely to support the death penalty than males 


library(lme4)
model2 <- glmer(capun ~ pctblk + (1 | psu), 
                data = Final2, 
                family = binomial)
summary(model2)
##According to model2, the results display that for each 1% increase of the 
### pctblack variable, there's an %0.8 decrease in the favoring of the death
##penalty 


full_model <- glmer(
  capun ~ white + femal + age + educ + pctblk + urban + south + (1 | psu),
  data = Final2,
  family = binomial)
summary(full_model)

###Yes, controlling for pctblk in a community affects the level 1 relationships,
##along with other variables, does change the estimated effects based on the model,
###though not by a significant amount

###1) Based on all of the models conducted, HLM is appropriate for this analysis
###The HLM model is appropriate using a standard logistic regression would 
###underestimate standard errors and potentially lead to incorrect inferences.




