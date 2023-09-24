## Ex 5/4
##############################################################

library(AER)
data(Affairs)
##?Affairs ## for help

poi1 <- glm(affairs ~ yearsmarried, family = poisson(), data = Affairs)
coef(poi1)
poi1 <- glm(affairs ~ yearsmarried, family = poisson(link="log"), data = Affairs)
coef(poi1)

pred1 <- predict(poi1, newdata=data.frame(yearsmarried = 15), type="response")
pred1

coef(poi1)
coef(poi1)[1]+coef(poi1)[2]*15       ## linear predictor
exp(coef(poi1)[1]+coef(poi1)[2]*15)  ## predicted E(Y|X=15)
