## Ex 5/1
##############################################################
## R code contributed by Pavlo Kravets

##install.packages("AER")
library(AER)
data(Affairs)
##?Affairs ## for help
Affairs$baffairs = as.factor(Affairs$affairs > 0)

spineplot(baffairs ~ gender, data = Affairs)
spineplot(baffairs ~ age, data = Affairs)
spineplot(baffairs ~ yearsmarried, data = Affairs)
spineplot(baffairs ~ children, data = Affairs)
spineplot(baffairs ~ rating, data = Affairs)

spineplot(children ~ yearsmarried, data = Affairs)

logit1 <- glm(formula = baffairs ~ yearsmarried, family = binomial(link = "logit"), data = Affairs)
logit2 <- glm(formula = baffairs ~ yearsmarried + rating, family = binomial(link = "logit"), data = Affairs)
logit3 <- glm(formula = baffairs ~ yearsmarried + children, family = binomial(link = "logit"), data = Affairs)
logit4 <- glm(formula = baffairs ~ yearsmarried + rating + children, family = binomial(link = "logit"), data = Affairs)


pred1 <- predict(logit1, newdata=data.frame(yearsmarried = 15, children = "yes", rating = 2), type="response"); pred1
pred2 <- predict(logit2, newdata=data.frame(yearsmarried = 15, children = "yes", rating = 2), type="response"); pred2
pred3 <- predict(logit3, newdata=data.frame(yearsmarried = 15, children = "yes", rating = 2), type="response"); pred3
pred4 <- predict(logit4, newdata=data.frame(yearsmarried = 15, children = "yes", rating = 2), type="response"); pred4

## doing the prediction for logit3 "by pocket calculator"
summary(logit3)
-1.80113+0.03867*15+0.48461
1/(1+exp(0.73647))  ## somewhat less precision as with rounded values

## in logit3, coefficients for the 2 variables are not significant
## compare with the null model:
logit0 <- glm(formula = baffairs ~ 1, family = binomial(link = "logit"), data = Affairs)
anova(logit0, logit3, test = "Chisq") 
## => significant difference between logit0 and logit3
