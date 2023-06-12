## Ex 4/1
##################################################

## a chi^2 distribution arises from a sum of squared N(0,1) variables
df <- 20
nsim <- 50000
z <- matrix(rnorm(nsim*df), nsim, df)

y <- rowSums(z^2); length(y)  ## y is then chi^2 distributed
plot(fh <- density(y))

## approximate expectation by mean (expection=df)
mean(y)
var(y)  ## variance=2*df

## theoretical chi^2 distribution
lines(fh$x, dchisq(fh$x,df=df), col="blue")

##########################

## t distrbution
df <- 4  ## the larger df, the closer to dnorm
grid <- seq(-5,5,by=0.1)
plot(grid, dt(grid,df=df), type="l", col="blue") ## has more fat tail than dnorm
lines(grid,dnorm(grid))

## approximate expectation by mean (expectation=0)
nsim <- 1000
t <- rt(nsim, df=df)
mean(t)
