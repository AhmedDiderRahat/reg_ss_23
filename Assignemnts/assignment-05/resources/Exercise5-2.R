## Ex5/2
#########################

## pdfs
curve(dlogis(x), from=-5, to=5, col="blue", main="Logistic and Gaussian pdf", ylim=c(0,0.4))
curve(dnorm(x), col="red", add=TRUE)

## cdfs
curve(plogis(x), from=-5, to=5, col="blue", main="Logistic and Gaussian cdf", ylim=c(0,1))
curve(pnorm(x), col="red", add=TRUE)

## variance of Gaussian distribution is 1 (N(0,1) => variance =1)
## => modify our code and use a normal distribution with variance pi^2/3

## pdfs
curve(dlogis(x), from=-7, to=7, col="blue", main="Logistic and Gaussian pdf", ylim=c(0,0.4))
curve(dnorm(x, sd=pi/sqrt(3)), col="red", add=TRUE)

## => the logistic distribution has a bit more fat tails, so it allows slighty more
##    for extremal values (outliers) at the boundaries

## cdfs
curve(plogis(x), from=-7, to=7, col="blue", main="Logistic and Gaussian cdf", ylim=c(0,1))
curve(pnorm(x, sd=pi/sqrt(3)), col="red", add=TRUE)
