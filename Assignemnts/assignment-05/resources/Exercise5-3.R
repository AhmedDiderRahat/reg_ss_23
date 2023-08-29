## Exercise 5/3
##############################################
## apply logit and probit on the Affairs data

library(AER)
data("Affairs")
str(Affairs)

table(Affairs$affairs)
Affairs$Y <- as.numeric(Affairs$affairs > 0)  ## define binary Y
str(Affairs)
table(Affairs$Y)

attach(Affairs)
table(Y,gender)

## graphical exploration (some examples)
mosaicplot(table(Y,gender), col=c("red","blue"))
mosaicplot(table(Y,children), col=(c("orange","green")))
mosaicplot(table(Y,rating), col=rainbow(5))
mosaicplot(table(rating,Y), col=c("blue","red"))

spineplot(as.factor(Y) ~ yearsmarried)

## logit models
logit1 <- glm(Y ~ yearsmarried, family=binomial(link="logit")) 
                                    ## sufficient: family=binomial()
summary(logit1)
  ## coeff of yearsmarried: 0.05882 >0 (and highly significant)
  ## the longer married, the more likely is an affair

logit2 <- glm(Y ~ yearsmarried + rating + gender + children, family=binomial(link="logit")) 
summary(logit2)
  ## coeff of rating: -0.46536 <0 (and highly significant, others not significant)
  ## with increasing happyness (= higher values of rating) the less likely to have an affair

## prediction, say: yearsmarried=10, rating=5, gender="female",children="yes"
lin.p <- predict(logit2, newdata=data.frame(yearsmarried=10, rating=5, gender="female",children="yes"))
lin.p
p.hat <- predict(logit2, newdata=data.frame(yearsmarried=10, rating=5, gender="female",children="yes"),
                 type="response")
p.hat  ## estimated probability of having an affair

## relation between linear predictor and estimated probability
plogis(lin.p)
1/(1+exp(-lin.p))

## Chi^2 test to compare 2 nested models
anova(logit1, logit2, test="Chisq")
  ## p-value < 5% (or 1%) => high significant difference between logit1 and logit2
  ## so we decide for the larger model

logit3 <- glm(Y ~ yearsmarried + rating + gender + children +age, family=binomial(link="logit")) 
summary(logit3)
anova(logit2, logit3, test="Chisq")
  ## at 1% significance level: we do not reject that both models are the same
  ## => no significant difference between both models
  ## => choose smaller model as there is no difference

## AIC comparison
AIC(logit1, logit2, logit3)  ## optimal: logit3
BIC(logit1, logit2, logit3)  ## optimal: logit2

library(MASS)
logit.s3 <- stepAIC(logit3)
summary(logit.s3)  ## children are skipped

logit.all <- glm(Y ~ . -affairs, data=Affairs, family=binomial(link="logit"))
  ## estimate with all variables (except affairs as we generated Y from affairs)
summary(logit.all)

logit.s <- stepAIC(logit.all)
summary(logit.s)  ## skipped: children, education, occupation

## add-on: how to estimate a probit model
probit.all <- glm(Y ~ . -affairs, data=Affairs, family=binomial(link="probit"))
summary(probit.all)
probit.s <- stepAIC(probit.all)
summary(probit.s)  ## skipped: children, education, occupation

AIC(logit.s)  ## slightly smaller than AIC(probit.s)
AIC(probit.s)

## added June 28
D <- deviance(logit.s); D

## calculate AIC from the deviance
D + 2*length(coef(logit.s))
AIC(logit.s)
D + 2*length(coef(logit.s)) - AIC(logit.s)  ## difference

## calculate log-likelihood
ll <- -0.5*D; ll  ## for binary model

summary(logit.s)
## check again some terms

logit0 <- glm(Y ~ 1, family = binomial(), data = Affairs) ## Null model
summary(logit0)

logit.transform <- function(p) log(p/(1-p))   ## inverse of plogis (or use qlogis)
plogis( coef(logit0) ); mean(Y)               ## identical
logit.transform( mean(Y) ); coef((logit0)))   ## identical


