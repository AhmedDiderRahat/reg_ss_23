## Excercise 4/3
#####################################################

set.seed(5721)  ## fix the seed to always have the same data

x <- runif(10)
y <- 2 - 2*x + 0.5*x^2 + rnorm(length(x), sd=0.2)
lm1 <- lm( y ~ x )
lm2 <- lm( y ~ x + I(x^2))
lm3 <- lm( y ~ x + I(x^2) + I(x^3) )

X1 <- model.matrix(lm1)
X2 <- model.matrix(lm2)
X3 <- model.matrix(lm3)

P1 <- X1 %*% solve( t(X1) %*% X1 ) %*% t(X1)
P2 <- X2 %*% solve( t(X2) %*% X2 ) %*% t(X2)
P3 <- X3 %*% solve( t(X3) %*% X3 ) %*% t(X3)

trace <- function(P){ sum(diag(P)) }

trace(P1)
trace(P2)
trace(P3)

P2 %*% P1 - P1
max(abs( P2 %*% P1 - P1 ))
max(abs( P3 %*% P1 - P1 ))
max(abs( P3 %*% P2 - P2 ))

