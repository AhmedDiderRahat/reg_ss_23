## Exercise 5/4
##############################################
## apply poisson regression on the Affairs data

library(AER)
data("Affairs")
str(Affairs)

## dependent variable: affairs / family=poisson
poisson0 <- glm( affairs ~ 1, family=poisson(), data=Affairs)  ## constant model
summary(poisson0)

D <- deviance(poisson0); D
AIC(poisson0)
D + 2*length( coef(poisson0) )  ## selfmade AIC is different
  ## reason: R uses the log-likelihood, not the deviance

poisson2 <- glm( affairs ~ yearsmarried + rating + gender + children, family=poisson(), data=Affairs) 
summary(poisson2)

anova(poisson0, poisson2, test="Chisq")
  ## p value < 2.2e-16 *** => decide for the model poisson2

## added July 5
################

poisson1 <- glm( affairs ~ yearsmarried + rating, family=poisson(), data=Affairs) 
summary(poisson1)

anova(poisson1, poisson2, test="Chisq")
  ## p value = 0.5055 => models are not significantly different
  ## => we choose the simpler model poisson1

## prediction using poisson1, say for: yearsmarried=10, rating=5
lin.p <- predict(poisson1, newdata=data.frame(yearsmarried=10, rating=5))
lin.p ## linear predictor only

pred <- predict(poisson1, newdata=data.frame(yearsmarried=10, rating=5),type="response")
pred ## estimated expected no. of affairs for a person with yearsmarried=10, rating=5

b <- coef(poisson1)
lin.p1 <- b[1] + b[2]*10 + b[3]*5; lin.p1  ## selfmade linear predictor
pred1 <- exp(lin.p1); pred1                ## selfmade prediction


