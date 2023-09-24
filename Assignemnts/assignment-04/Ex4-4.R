## Ex 4/4
#################################################################
## R code contributed by 	Vipin Singh + small addons (MM, see below)

x <- seq(-10, 10, 0.1)
y <- exp(x) / (1 + exp(x))

plot(x, y, type = "l", col = "red", lwd = 2, xlab = "x", ylab = "y")

# Properties of a cdf
# 1. F(x) is non-decreasing in x
# 2. F(x) -> 0 as x -> -inf
# 3. F(x) -> 1 as x -> inf

# Calculate the pdf from the cdf
pdf <- diff(y)
plot(x[-1], pdf, type = "l", col = "blue", lwd = 2, xlab = "x", ylab = "pdf")

pdf <- exp(-x) / (1 + exp(-x)) ^ 2
plot(x, dlogis(x), type = "l", col = "blue", lwd = 2, xlab = "x", ylab = "pdf")
lines(x, dnorm(x, sd=sqrt(pi^2/3)), col="red", lwd=2)
title("Standard logistic (blue) vs. normal (red, var=pi^2/3)")  ## added (MM)
# Generate Pseudo-random numbers for the logistic distribution
set.seed(1000)
n <- 10000
u <- rlogis(n)

# Plot the histogram of the pseudo-random numbers
hist(u, breaks = 100, freq = FALSE, col = "lightblue", 
     main = "Histogram of pseudo-random numbers")
lines(x, dlogis(x), col = "blue", lwd = 2) ## added (MM)

# Show the mean and variance of the pseudo-random numbers
mean <- round(mean(u), 4)
true_mean <- 0

var <- round(var(u), 4)
true_var <- round(pi ^ 2 / 3, 4)

print(paste("Mean:", mean, "True mean:", true_mean))
print(paste("Variance:", var, "True variance:", true_var))

