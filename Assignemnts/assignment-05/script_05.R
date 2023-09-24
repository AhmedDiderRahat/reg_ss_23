###############################################################################################
#   Regression Exercise-05                                                                    #
#   Date: 1st July 2023                                                                       #
###############################################################################################

# Preparations
rm(list = ls(all.names = TRUE))

# Exercise 1
# Load the dataset Affairs from the R package AER (library(AER); data(Affairs)).
# Check the data documentation: ?Affairs.

# load the dataset
library(AER)
data("Affairs")

# summary if the data
str(Affairs)

# attach the variables
attach(Affairs)

# check the variable affairs
table(affairs)

# make new variable Y
Affairs$Y <- as.numeric(Affairs$affairs > 0)
attach(Affairs)

table(Y)


# (a) Explore graphically the effect of single explanatory variables on Y (use for example: spineplot, barplot, or mosaicplot).

mosaicplot(table(Y, gender), col=c("red","blue"))
mosaicplot(table(Y, children), col=(c("orange","green")))
mosaicplot(table(Y, rating), col=rainbow(5))


head(Affairs)

logit1 <- glm(formula = baffairs ~ yearsmarried, family = binomial(link = "logit"), data = Affairs)
logit2 <- glm(formula = baffairs ~ yearsmarried + rating, family = binomial(link = "logit"), data = Affairs)
logit3 <- glm(formula = baffairs ~ yearsmarried + children, family = binomial(link = "logit"), data = Affairs)
logit4 <- glm(formula = baffairs ~ yearsmarried + rating + children, family = binomial(link = "logit"), data = Affairs)


pred1 <- predict(logit1, newdata=data.frame(yearsmarried = 15, children = "yes", rating = 2), type="response"); pred1
pred2 <- predict(logit2, newdata=data.frame(yearsmarried = 15, children = "yes", rating = 2), type="response"); pred2
pred3 <- predict(logit3, newdata=data.frame(yearsmarried = 15, children = "yes", rating = 2), type="response"); pred3
pred4 <- predict(logit4, newdata=data.frame(yearsmarried = 15, children = "yes", rating = 2), type="response"); pred4

summary(logit3)

b3_hat <- summary(logit3)$coefficients; b3_hat

y3_hat <- b3_hat[1] + (b3_hat[2] * 15) + (b3_hat[3]*1); y3_hat
#  -0.736417
y3_predict <- 1 / (1 + exp(-y3_hat)); y3_predict


####################################################################################################################

# Exercise 2
# Redo the analysis from Exercise 1 using probit models (use link='probit' in the family
# parameter of glm). In particular: How could you do the prediction here without using R?


probit1 <- glm(formula = baffairs ~ yearsmarried, family = binomial(link = "probit"), data = Affairs)
probit2 <- glm(formula = baffairs ~ yearsmarried + rating, family = binomial(link = "probit"), data = Affairs)
probit3 <- glm(formula = baffairs ~ yearsmarried + children, family = binomial(link = "probit"), data = Affairs)
probit4 <- glm(formula = baffairs ~ yearsmarried + rating + children, family = binomial(link = "probit"), data = Affairs)


pred1 <- predict(probit1, newdata=data.frame(yearsmarried = 15, children = "yes", rating = 2), type="response"); pred1
pred2 <- predict(probit2, newdata=data.frame(yearsmarried = 15, children = "yes", rating = 2), type="response"); pred2
pred3 <- predict(probit3, newdata=data.frame(yearsmarried = 15, children = "yes", rating = 2), type="response"); pred3
pred4 <- predict(probit4, newdata=data.frame(yearsmarried = 15, children = "yes", rating = 2), type="response"); pred4

summary(probit3)

b3_hat <- summary(logit3)$coefficients; b3_hat

y3_hatp <- b3_hat[1] + (b3_hat[2] * 15) + (b3_hat[3]*1); y3_hat
#  -0.736417
y3_predictp <- 1 / (1 + exp(-y3_hatp)); y3_predictp

y3_predict



####################################################################################################################

# Exercise 3
# Compare your estimated models from Exercise 1. Instead of the F test for linear models we
# do now use chi-2 tests. The syntax is similar: anova(glm1,glm2,test="Chisq") Additionally,
# also compare AIC values and apply stepAIC.

anova(logit1, logit2, test=("Chisq"))
# model 2 is significantly better than model 1

AIC(logit1, logit2)
# Model 2 has lower AIC value


AIC(logit1, logit2, logit3, logit4)
# Still model 2 is better as the AIC is smaller and have less df

anova(logit2, logit4, test = "Chisq")
# model 2 is best as there is no significance results

library(MASS)

logit_All = glm(formula = baffairs ~ . -affairs, family = binomial(link = "logit"), data = Affairs)
summary(logit_All)

stepAll <- stepAIC(logit_All)
summary(stepAll)

stepAIC(logit1, logit2)

AIC(logit1, logit2, logit3, logit4, logit_All)

anova(logit2, logit_All, test="Chisq")

####################################################################################################################

# Exercise 4
# Load again the dataset Affairs from the R package AER. We aim to estimate a poisson
# regression now, so that we use Y = affairs. Do the following analyses:

# (a) Explore graphically the effect of single explanatory variables on Y (cf. Exercise 1(a)).

Affairs$Y1 <- as.numeric(affairs)
attach(Affairs)
table(Y1)

mosaicplot(table(Y1, gender), col=c("red","blue"))
mosaicplot(table(Y1, children), col=(c("orange","green")))
mosaicplot(table(Y1, rating), col=rainbow(5))
mosaicplot(table(Y1, yearsmarried), col=rainbow(5))

poil1 <- glm(Y1 ~ yearsmarried, family = poisson(), data = Affairs)
poil2 <- glm(Y1 ~ yearsmarried + rating, family = poisson(), data = Affairs)
poil3 <- glm(Y1 ~ yearsmarried + children, family = poisson(), data = Affairs)
poil4 <- glm(Y1 ~ yearsmarried + rating + children, family = poisson(), data = Affairs)


p1 <- predict(poil1, newdata=data.frame(yearsmarried = 15, children = "yes", rating = 2), type="response"); p1
p2 <- predict(poil2, newdata=data.frame(yearsmarried = 15, children = "yes", rating = 2), type="response"); p2
p3 <- predict(poil3, newdata=data.frame(yearsmarried = 15, children = "yes", rating = 2), type="response"); p3
p4 <- predict(poil4, newdata=data.frame(yearsmarried = 15, children = "yes", rating = 2), type="response"); p4


b1_hat <- coef(poi1); b1_hat

y_val <- b1_hat[1] + b1_hat[2]*15; y_val

y1_hat <- exp(y_val); y1_hat


# AIC value
AIC(poil1, poil2, poil3, poil4)
# model 2 and model 4 has better AIC score

anova(poil2, poil4, test="Chisq")
# No significane betterment 

anova(poil1, poil2, test="Chisq")
# Model 2 is better with 0.1% significant level

