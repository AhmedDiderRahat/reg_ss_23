## Excercise 4/5
#####################################################

data <- read.csv("Dataset/AnscombeQuartet.csv")

lm1 <- lm(y1 ~ x1, data=Anscombe); summary(lm1)$r.squared
lm2 <- lm(y2 ~ x2, data=Anscombe); summary(lm2)$r.squared
lm3 <- lm(y3 ~ x3, data=Anscombe); summary(lm3)$r.squared
lm4 <- lm(y4 ~ x4, data=Anscombe); summary(lm4)$r.squared  ## all R^2 similar


plot(Anscombe$x1,Anscombe$y1, pch="+"); abline(lm1,col="red")
plot(Anscombe$x2,Anscombe$y2, pch="+"); abline(lm2,col="red")
plot(Anscombe$x3,Anscombe$y3, pch="+"); abline(lm3,col="red")
plot(Anscombe$x4,Anscombe$y4, pch="+"); abline(lm4,col="red")
par(mfrow=c(1,1))

head(Anscombe)


par(mfrow=c(2,2))
regmodel <- function(X, Y) {
  
  lobj <- lm(Y ~ X)
  
  print(summary(lobj)$r.squared)
  
  plot(X, Y, pch="+")
  abline(lobj, col="red")
}

regmodel(data$x1, data$y1)
regmodel(data$x2, data$y2)
regmodel(data$x3, data$y3)
regmodel(data$x4, data$y4)


par(mfrow=c(1,1))