# Load data
require(AER)  ## AER package
data(CPS1985) ## load data set into workspace

## ?CPS1985

lm1 <- lm(wage ~ education + experience + I(experience^2), data=CPS1985)
## put the data=CPS1985 to inform lm use this data (alternatively: attach)
summary(lm1)

## str(lm1)
## str(summary(lm1))

lm1$df.residual ## here: 530 = n-p-1
summary(lm1)$df ##  more df
summary(lm1)$sigma ## sigma.hat

summary(lm1)$r.squared  ## R^2
summary(lm1)$adj.r.squared  ## adjusted R^2

## t-tests for coefficients
###########################

summary(lm1) ## alpha=5%=0.05 -> all are significant at 5% (alos 10%, 1%)
             ## for example, the 4th coeff. is also significant at 0.1%

lm2 <- lm(wage ~ education + experience, data=CPS1985)
summary(lm2)  ## all coeff. are significant at 0.1% (and thus also at 1%, 5%, 10%)

lm3 <- lm(wage ~ education + experience + I(experience^2)+ I(experience^3), data=CPS1985)
summary(lm3)

## F-tests for model comparison
###############################

lm0 <- lm( wage ~ 1, data=CPS1985)   ## constant model, no variables!
                                     ## (only the constant is fitted)
summary(lm0)
mean(CPS1985$wage)

anova(lm0,lm1)  ## F-Test in R by calling anova with
                ## two nested(!) models
## result: p-value very small -> 2nd model is significantly
## different from the 1st model (as it fits better, see e.g. RSS: the 2nd ist significantly better)

summary(lm1) ## last line:
## F-statistic: 48.25 on 3 and 530 DF,  p-value: < 2.2e-16

## remember:
lm1 <- lm(wage ~ education + experience + I(experience^2), data=CPS1985)
lm2 <- lm(wage ~ education + experience, data=CPS1985)
lm3 <- lm(wage ~ education + experience + I(experience^2)+ I(experience^3), data=CPS1985)

## which F-tests are possible (remember: subsets needed in H0)
anova(lm2,lm1)  ## significant difference at 1% (thus also at 5%, 10%)
                ## -> lm1 is significantly better than lm2

anova(lm2,lm3)  ## significant difference at 1% (thus also at 5%, 10%)
                ## -> lm3 is significantly better than lm2

anova(lm1,lm3)  ## no significant difference (not even at 10%)
                ## -> we would choose the smaller model lm1 (as lm3 does not signif. improve)