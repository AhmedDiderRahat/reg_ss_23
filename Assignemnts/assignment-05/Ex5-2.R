## Ex 5/2
##############################################################

library(AER)
data(Affairs)
##?Affairs ## for help
Affairs$baffairs = as.factor(Affairs$affairs > 0)

probit1 <- glm(formula = baffairs ~ yearsmarried, family = binomial(link = "probit"), data = Affairs)
probit2 <- glm(formula = baffairs ~ yearsmarried + rating, family = binomial(link = "probit"), data = Affairs)
probit3 <- glm(formula = baffairs ~ yearsmarried + children, family = binomial(link = "probit"), data = Affairs)
probit4 <- glm(formula = baffairs ~ yearsmarried + rating + children, family = binomial(link = "probit"), data = Affairs)

pred1 <- predict(probit1, newdata=data.frame(yearsmarried = 15, children = "yes", rating = 2), type="response"); pred1
pred1 <- predict(probit1, newdata=data.frame(yearsmarried = 15), type="response"); pred1

lin.pred1 <- predict(probit1, newdata=data.frame(yearsmarried = 15)); lin.pred1 ## linear predictor

## predicting
coef(probit1)
-0.9768097+0.0351881*15
coef(probit1)[1]+coef(probit1)[2]*15
pnorm(coef(probit1)[1]+coef(probit1)[2]*15)
## using the table for Phi: Phi(-0.45)=0.32636

## compare nested models
anova(probit1,probit2, test="Chisq")
## models are sign. different => decide for the larger model probit2

## compare by AIC
AIC(probit1)
AIC(probit2)  ## smaller than for probit1

AIC(probit1, probit2, probit3, probit4)  ## probit2 has smallest AIC
anova(probit2,probit4, test="Chisq")
## models are not sign. different => decide for the smaller model probit2

## find an optimal model 
probit.all <- glm(formula = baffairs ~ . -affairs , family = binomial(link = "probit"), data = Affairs)
summary(probit.all)

## stepwise search based on AIC
library(MASS)
step.all <- stepAIC(probit.all)
summary(step.all)

AIC(probit1, probit2, probit3, probit4, probit.all, step.all)
