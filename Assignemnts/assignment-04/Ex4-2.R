## Ex 4/2
#################################################################
## R code contributed by 	Christian Tesch + curves corrected (MM)

# Generate data
x <- runif(10)
y <- 2 - 2*x + 0.5*x^2 + rnorm(length(x), sd=0.2)

# Fit regression models
lm1 <- lm( y ~ x)
lm2 <- lm( y ~ x + I(x^2))
lm3 <- lm( y ~ x + I(x^2) + I(x^3) )

# Scatterplot of data
plot(x, y, ylim=c(-1,3))

# a) Plot fitted curves / curves corrected (MM)
abline(lm1, col="seagreen")
curve(lm2$coefficients[1] + lm2$coefficients[2]*x + lm2$coefficients[3]*x^2, add=TRUE, col="blue")
curve(lm3$coefficients[1] + lm3$coefficients[2]*x + lm3$coefficients[3]*x^2 + lm3$coefficients[4]*x^3,
      add=TRUE, col="red")  

anova(lm1, lm2)
anova(lm2, lm3)
anova(lm1, lm3)

# b) 
m1 <- model.matrix(lm1); m1
m2 <- model.matrix(lm2); m2
m3 <- model.matrix(lm3); m3


# function for calculating the hat matrix
# %*% matrix multiplication
hat_matrix <- function(X) X %*% solve(t(X) %*% X) %*% t(X)

# function to check symmetry
is_symmetric <- function(X) all.equal(X, t(X))

# simple model
lm1.P <- hat_matrix(m1)
sum(diag(lm1.P)) # trace = 2 (p+1)
eigen(lm1.P)$values # eigenvalues = [1, 1]
is_symmetric(lm1.P) # TRUE

# square
lm2.P <- hat_matrix(m2)
sum(diag(lm2.P)) # trace = 3
eigen(lm2.P)$values # eigenvalues = [1, 1, 1]
is_symmetric(lm2.P) # TRUE

# cube
lm3.P <- hat_matrix(m3)
sum(diag(lm3.P)) # trace = 4
eigen(lm3.P)$values # eigenvalues = [1, 1, 1, 1]
is_symmetric(lm3.P) # TRUE

# c)
all.equal(lm2.P %*% lm1.P, lm1.P) # P2*P1 ?= P1: TRUE
all.equal(lm3.P %*% lm1.P, lm1.P) # P3*P1 ?= P1: TRUE
all.equal(lm3.P %*% lm2.P, lm2.P) # P3*P2 ?= P2: TRUE

lm1$df.residual #here: 8 = n-p-1
summary(lm1)$sigma #sigma.hat

lm2.P  ## nxn matrix! (MM)

# Corollary of two nested models models (page 47 in lecture script)
# -> only valid when nested model is obtained by excluding predictors without
# introducing new ones!

