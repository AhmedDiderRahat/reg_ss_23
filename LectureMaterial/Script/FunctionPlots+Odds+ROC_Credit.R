library(evtree)
data("GermanCredit")
str(GermanCredit)
table(GermanCredit$credit_risk)

GermanCredit$Y <- 1- as.numeric(GermanCredit$credit_risk=="good"); table(GermanCredit$Y)
table(GermanCredit$Y)

######################################################################
## Redefine variables
######################################################################
GermanCredit$employ1Y  <- (GermanCredit$employment_duration == "1 <= ... < 4 years") | (GermanCredit$employment_duration == "4 <= ... < 7 years") | (GermanCredit$employment_duration == "... >= 7 years")
            ## employed (>=1 year)
## duration
GermanCredit$d9.12   <- ((GermanCredit$duration >9)&(GermanCredit$duration <=12))  ##  9 < duration <= 12 
GermanCredit$d12.18  <- ((GermanCredit$duration >12)&(GermanCredit$duration <=18)) ## 12 < duration <= 18 
GermanCredit$d18.24  <- ((GermanCredit$duration >18)&(GermanCredit$duration <=24)) ## 18 < duration <= 24
GermanCredit$d24     <- (GermanCredit$duration >24)                   ## 24 < duration

GermanCredit$savings1K <- GermanCredit$savings == "... >= 1000 DM"
GermanCredit$car       <- GermanCredit$purpose=="car"                 ## loan is for a car
GermanCredit$young     <- (GermanCredit$age <35)

######################################################################
## Plotting regression functions
######################################################################

## one-dimensional
g1 <- glm( Y ~ age, data=GermanCredit, family=binomial() )
g2 <- glm( Y ~ age + I(age^2), data=GermanCredit, family=binomial() )

summary(g1)
summary(g2)

m1 <- function(x){ plogis(coef(g1)[1]+coef(g1)[2]*x) }
m2 <- function(x){ plogis(coef(g2)[1]+coef(g2)[2]*x +coef(g2)[3]*x^2) }
plot(GermanCredit$age,GermanCredit$Y, cex=0.8, pch="+", main="g1+2 - data + regression fit")
curve(m1(x), col="red", add=TRUE)
curve(m2(x), col="seagreen", add=TRUE)
curve(predict(g2, newdata=data.frame(age=x), type="response"),lwd=2,add=TRUE)

## two-dimensional
g3 <- glm( Y ~ amount + age, data=GermanCredit, family=binomial())
g4 <- glm( Y ~ amount + I(amount^2) + age, data=GermanCredit, family=binomial())
g5 <- glm( Y ~ amount + I(amount^2) + age + I(age^2), data=GermanCredit, family=binomial())

summary(g3)

## with interaction of amount and age
g6 <- glm( Y ~ amount + I(amount^2) + age + I(age^2) + amount:age, data=GermanCredit, family=binomial())
g6 <- glm( Y ~ amount*age + I(amount^2) + I(age^2), data=GermanCredit, family=binomial()) ## same
summary(g6)

anova(g5,g6,test="Chisq")  ## interaction term not significant

## define grids for the variables 
amount.g <- seq(min(GermanCredit$amount),max(GermanCredit$amount),length=11)
age.g <- seq(min(GermanCredit$age),max(GermanCredit$age),length=11)

p5 <- function(x1,x2,b){ p <- b[1]+b[2]*x1+b[3]*x1^2+b[4]*x2+b[5]*x2^2; return(p) }
p5.o <- outer(amount.g,age.g, p5, coef(g5)) ## linear predictor!

## perspective plot (static in 2d)
persp(amount.g,age.g, p5.o, theta = 30, phi = 30, expand = 0.9, col="lightblue", xlab="amount",ylab="age",zlab="lin.pred")

## contour plot (in 2d)
contour(amount.g,age.g, p5.o, xlab="amount",ylab="age")

## perspective plot (interactive in 3d -> you may rotate the surface!)
library(rgl)
persp3d(amount.g,age.g, p5.o, col="lightblue", alpha=0.5, xlab="amount",ylab="age",zlab="lin.pred")

pred <- predict(g5,newdata=data.frame(age=25,amount=1500))
points3d(1500,25,pred,col="red",size=10)
pred.line <- predict(g5,newdata=data.frame(age=age.g,amount=rep(1500,length(age.g))))
lines3d(rep(1500,length(age.g)),age.g,pred.line,col="orange",lwd=2)

m5 <- function(x1,x2,b){ p <- plogis(b[1]+b[2]*x1+b[3]*x1^2+b[4]*x2+b[5]*x2^2); return(p) }
m5.o <- outer(amount.g,age.g, m5, coef(g5)) ## estimated probabilities!

## perspective plot (static in 2d)
persp(amount.g,age.g, m5.o, theta = 30, phi = 30, expand = 0.9, col="lightblue", xlab="amount",ylab="age",zlab="pred.prob")

## contour plot (in 2d)
contour(amount.g,age.g, m5.o, xlab="amount",ylab="age")

## perspective plot (interactive in 3d -> you may rotate the surface!)
persp3d(amount.g,age.g, m5.o, col="lightblue", alpha=0.5, xlab="amount",ylab="age",zlab="pred.prob")

pred <- predict(g5,newdata=data.frame(age=25,amount=15000),type="response")
points3d(15000,25,pred,col="red",size=10)
pred.line <- predict(g5,newdata=data.frame(age=age.g,amount=rep(15000,length(age.g))),type="response")
lines3d(rep(15000,length(age.g)),age.g,pred.line,col="orange",lwd=2)



######################################################################
## Odds + Odds Ratio
######################################################################

table(GermanCredit$Y)      ## 30% defaults = "bad" customers
table(GermanCredit$young)  ## 54.8% young (age < 35)

t <- table(GermanCredit$young,GermanCredit$Y)
t
mosaicplot(t, col=c("blue","red"))

odds.young0 <- t[1,2]/t[1,1] ## odds for default of old 
odds.young0
odds.young1 <- t[2,2]/t[2,1] ## odds for default of young
odds.young1

odds.01 <- odds.young0 / odds.young1 ## odds ratio old vs. young
odds.01   ## <1, older have less risk of default

g <- glm( Y ~ young, data=GermanCredit, family=binomial() )
summary(g)

b <- coef(g)
b


g.odds.youngFALSE <- exp(b[1]+b[2]*0)  ## old
g.odds.youngFALSE
odds.young0

g.odds.youngTRUE <- exp(b[1]+b[2]*1)  ## young
g.odds.youngTRUE
odds.young1

######################################################################
## ROC curve + AUC
######################################################################

library(ROCR)

## age (problem: reverse order)
pred <- prediction(GermanCredit$age, GermanCredit$Y)
perf <- performance(pred,"tpr","fpr")
AUC <- slot(performance( pred, 'auc' ),"y.values")[[1]]
AUC

plot(perf, main=paste("ROC fuer age, AUC =",round(AUC,3)), colorize=TRUE, lwd=2, xlab="1-F0", ylab="1-F1")
lines(c(0,1),c(0,1),lty=1, col="darkgrey") ## diagonal

## minus age (now: sorted from low to high risk)
pred <- prediction( -GermanCredit$age, GermanCredit$Y)
perf <- performance(pred,"tpr","fpr")
AUC <- slot(performance( pred, 'auc' ),"y.values")[[1]]
AUC

plot(perf, main=paste("ROC for -age, AUC =",round(AUC,3)), colorize=TRUE, lwd=2, xlab="1-F0", ylab="1-F1")
lines(c(0,1),c(0,1),lty=1, col="darkgrey") ## add diagonal

## estimated logit model

g.full <- glm(Y ~ age + amount + I(age^2) + I(amount^2)+ age:amount + employ1Y +d9.12 + d12.18 + d18.24 +d24 + savings + purpose + housing, data=GermanCredit, family=binomial)
## a better "full" model (exclude + credit_risk, as this is = Y)
g.full <- glm(Y ~ . - credit_risk + I(age^2) + I(amount^2) + age:amount, data=GermanCredit, family=binomial)

summary(g.full)
eta <- g.full$linear.predictors

pred <- prediction( eta, GermanCredit$Y)
perf <- performance(pred,"tpr","fpr")
AUC <- slot(performance( pred, 'auc' ),"y.values")[[1]]
AUC

plot(perf, main=paste("ROC for full model, AUC =",round(AUC,3)), colorize=TRUE, lwd=2, xlab="1-F0", ylab="1-F1")
lines(c(0,1),c(0,1),lty=1, col="darkgrey") ## add diagonal

library(MASS)

g.step <- stepAIC(g.full)

summary(g.step)
eta <- g.step$linear.predictors

pred <- prediction( eta, GermanCredit$Y)
perf <- performance(pred,"tpr","fpr")
AUC <- slot(performance( pred, 'auc' ),"y.values")[[1]]
AUC

plot(perf, main=paste("ROC for stepAIC model, AUC =",round(AUC,3)), colorize=TRUE, lwd=2, xlab="1-F0", ylab="1-F1")
lines(c(0,1),c(0,1),lty=1, col="darkgrey") ## add diagonal

