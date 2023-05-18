#################################################################################################
#   Regression (Lecture code-03)                                                                    #
#   Date: 15th May 2023                                                                         #
#################################################################################################

# see the path
getwd()

# /Users/zeotaplaptop/Desktop/BHT-SS23/Regression/reg_ss_23_2

dat <- read.csv2("Dataset/Marketing.csv")    ## see slides 14-15

attach(dat)  ## load data into workspace

plot(x,y, pch=19, main="Marketing")

## simple linear regression
lm1 <- lm(y ~ x, data=dat)
lm1
abline(lm1, col="red")

## multiple linear regressions
lm2 <- lm(y ~ x + I(x^2), data=dat)  ## multiple (quadratic)
lm3 <- lm(y ~ x + I(x^2) + I(x^3), data=dat)  ## multiple (cubic)

lm2
lm3

## use coef to obtain the coefficients
b.lm2 <- coef(lm2); b.lm2
curve(b.lm2[1] + b.lm2[2]*x +b.lm2[3]*x^2, col="blue", add=TRUE)
b.lm3 <- coef(lm3); b.lm3
curve(b.lm3[1] +b.lm3[2]*x +b.lm3[3]*x^2 +b.lm3[4]*x^3, col="green", add=TRUE)

n <- nrow(dat)  ## or length(y)
X1 <- cbind( rep(1,n), x ); X1             ## design matrix for lm1
X2 <- cbind( rep(1,n), x , x^2 ); X2       ## design matrix for lm2
X3 <- cbind( rep(1,n), x , x^2 , x^3); X3  ## design matrix for lm3

## alternatively: obtain design matrix X from model
model.matrix(lm1)
X1
model.matrix(lm2)
X2
model.matrix(lm3)
X3

## lm1: simple linear regression
b1 <- cov(x,y)/var(x); b0 <- mean(y)-b1*mean(x); c(b0,b1) ## "self made" coefficients
coef(lm1)                                                 ## extracted from lm1
b1.hat <- solve( t(X1) %*% X1 ) %*% t(X1) %*% y; b1.hat   ##formula for multiple lin. reg-

## lm2: quadratic regression curve
coef(lm2)                                                 ## extracted from lm2
b2.hat <- solve( t(X2) %*% X2 ) %*% t(X2) %*% y; b2.hat   ##formula for multiple lin. reg-

## residuals
b1 <- coef(lm1)
res1 <- y - b1[1] - b1[2]*x   ## "self made"
plot(x,res1, main="Residual plot: lm1")
plot(x,residuals(lm1), main="Residual plot: lm1")
abline(h=0, col='darkgrey')

## compare residuals for lm1 and lm2
par(mfrow=c(1,2))
ylim <- range(c(residuals(lm1),residuals(lm2)))
plot(x,residuals(lm1), main="Residual plot: lm1", ylim=ylim)
abline(h=0,col="gray")
plot(x,residuals(lm2), main="Residual plot: lm2", ylim=ylim)
abline(h=0,col="gray")
par(mfrow=c(1,1))

## fitted regression curves and residual plots
par(mfrow=c(2,2))
plot(x,y, pch="+",main="lm1"); abline(lm1, col="red")
plot(x,y, pch="+",main="lm2"); curve(b.lm2[1] + b.lm2[2]*x +b.lm2[3]*x^2, col="blue", add=TRUE)
##
ylim <- range(c(residuals(lm1),residuals(lm2)))
plot(x,residuals(lm1), main="Residual plot: lm1", ylim=ylim)
abline(h=0,col="gray")
plot(x,residuals(lm2), main="Residual plot: lm2", ylim=ylim)
abline(h=0,col="gray")
par(mfrow=c(1,1))

## compare residual sum of squares and R^2
RSS1 <- sum( residuals(lm1)^2 ); RSS1
RSS2 <- sum( residuals(lm2)^2 ); RSS2
TSS <- sum( (y-mean(y))^2 )
R2.lm1 <- 1-RSS1/TSS; R2.lm1
R2.lm2 <- 1-RSS2/TSS; R2.lm2

## R^2 extracted from summary
summary(lm1)
str(summary(lm1))
summary(lm1)$r.squared

## for simple linear regression
cor(x,y)^2
summary(lm1)$r.squared

## for simple and multiple linear regression
cor(fitted(lm2),y)^2
summary(lm2)$r.squared

cor(fitted(lm3),y)^2
summary(lm3)$r.squared


## predicting single values
plot(x,y, pch="+"); abline(lm1, col="red")
b.lm1 <- coef(lm1)
x0 <- 150; pred.lm1 <- b.lm1[1] +  b.lm1[2]*x0; pred.lm1
points(x0, pred.lm1, pch=19,col="red")

b.lm2 <- coef(lm2); b.lm2
curve(b.lm2[1] + b.lm2[2]*x +b.lm2[3]*x^2, col="blue", add=TRUE)
x0 <- 150; pred.lm2 <- b.lm2[1] +  b.lm2[2]*x0 + b.lm2[3]*x0^2; pred.lm2
points(x0, pred.lm2, pch=19,col="blue")

## prdicting using predict (also for more than single values)
predict(lm1, newdata=data.frame(x=x0))
pred.lm1 
predict(lm2, newdata=data.frame(x=x0))
pred.lm2 

predict(lm2, newdata=data.frame(x=c(150,170,190)))


summary(lm1)
summary(lm2)
summary(lm3)


summary(lm1)$r.squared
summary(lm2)$r.squared
summary(lm3)$r.squared


summary(lm1)$adj.r.squared
summary(lm2)$adj.r.squared
summary(lm3)$adj.r.squared
