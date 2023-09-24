## Ex 4/5
#################################################################
## R code contributed by 	Maluna Menke

data <- read.csv("AnscombeQuartet.csv")

lm1 <- lm(y1 ~ x1, data = data)
lm2 <- lm(y2 ~ x2, data = data)
lm3 <- lm(y3 ~ x3, data = data)
lm4 <- lm(y4 ~ x4, data = data)
deviance(lm1)
deviance(lm2)
deviance(lm3)
deviance(lm4)
#RSS is nearly the same for all 4 models

summary(lm1)$r.squared 
summary(lm2)$r.squared 
summary(lm3)$r.squared 
summary(lm4)$r.squared 
# the R2 value is nearly the same as well for all 4 models

# i dint not expect that as x and coresponding y are not same
# (e.g. x4 has mostly 8 as value)

plot(data$x1, data$y1)
abline(lm1, col='blue')

plot(data$x2, data$y2)
abline(lm2, col='red')

plot(data$x3, data$y3)
abline(lm3, col='green')

plot(data$x4, data$y4)
abline(lm4, col='orange')

# it can be seen that all the point clouds look very different from one another. 
# Some are more linear (lm1), some are cleary multiple linear (lm2), 
# lm3 has a outliner and would be linear, lm4 has one huge outlier aswell 

