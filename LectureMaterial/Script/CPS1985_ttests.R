## t-tests in R
###########################################

library(AER)
## or do install.packages("AER")

data(CPS1985)
## get help with ?CPS1985
str(CPS1985)

attach(CPS1985)  ## load variables into workspace


## t.tests from Statistical Computing
#########################################################
## example of a hypothesis: is wage dependent on gender?
## compare the two groups w.r.t. wage
boxplot(wage~gender)
t.test(wage~gender)  ## H0 (means are equal) rejected
## example of a hypothesis: is expected wage around 9 US Dollar?
mean(wage)
t.test(wage,mu=9)    ## H0 (mean is 9) not rejected
#########################################################

## t.tests in Regression
2#########################################################
lm1 <- lm(log(wage) ~ education + experience + I(experience^2) )
summary(lm1)  ## all coefficients are significantly different from 0 again

lm2 <- lm(log(wage) ~ education + I(education^2) + experience + I(experience^2) )
summary(lm2)  ## education is not significantly different from 0 (at 5%)

lm3 <- lm(log(wage) ~ I(education^2) + experience + I(experience^2) )
summary(lm3)  ## all coefficients are significantly different from 0 again

## conclusion: we should include either education or eduaction ^2 in the model
## when looking at coefficients
## BUT: we will see a better way to compare different models (by F tests)

