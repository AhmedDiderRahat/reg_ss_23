###############################################################################################
#   Regression Exercise-01                                                                    #
#   Date: 04th April 2023                                                                     #
###############################################################################################

# Preparations
rm(list = ls(all.names = TRUE))

# Question 1

# Solution (a)
Sy2 <- ((-1.84 / -0.4)^2) * (1 / 4); Sy2


# Solution (b)
X_mean <- 1.8 * 25 + 32; X_mean
X_std <- 1.8 * 3; X_std


# Solution (c)
x <- c(1, 1, 2, 2, 3, 3)
y <- c(1, 3, 2, 4, 3, 5) 

x_mean <- mean(x); x_mean

y_mean <- mean(y); y_mean


# Solution (d)
r_xy <- 4 / (sqrt(4 * 10)); r_xy




# Exercise 5:

# Solution (b)

X <- c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70)
Y <- c(18, 26, 33, 40, 46, 59, 72, 85, 97, 120, 141)

X_mean <- mean(X); X_mean   # 45

Y_mean <- mean(Y); Y_mean   # 67

Sx2 <- var(X);   Sx2        # 275 

Sy2 <- var(Y);   Sy2        # 1600.6

Sxy <- cov(X,Y); Sxy        # 649.5


# Solution (c)
plot(X, Y, xlim = c(0, max(X)), ylim = c(0, max(Y)),  
     pch=19, main = 'Scatterplot of car data')


# Solution (d)
cor(X, Y)   # 0.9789746


# Solution (e)
lreg_model <- lm(Y ~ X)

summary(lreg_model)

abline(lreg_model, col = 'red', lwd = 2)



# Exercise 6:

# Solution (b)

data <- read.csv('Dataset/MunichRent2003.csv')

# Snapshot of the data
str(data)

# variable attach
attach(data)

# Solution (c)

X <- living.space
Y <- netrent

# variance 
Sx2 <- var(X)
Sy2 <- var(Y)

# Covariance 
Sxy <- cov(X, Y)

# Regression Co-efficient
b1 <- Sxy / Sx2
b0 <- mean(Y) - (b1 * mean(X))

cat('b0= ', b0, ' | b1= ', b1)
# b0=  89.84691  | b1=  6.90056

# Plotting 

plot(X, Y, xlim = c(0, max(X)), ylim = c(0, max(Y)),
     pch=19, main = 'Scatterplot of Munchen Rent', 
     xlab = 'Living Space', ylab = 'Net rent')

abline(b0, b1,  col = 'seagreen', lwd = 3)


# R2 Score calculation
Y_hat <- b0 + (b1 * X)

Sy_hat2 <- var(Y_hat)

r2 <- Sy_hat2 / Sy2
r2 # 0.5005034


# Regression analysis of net rent and year

ll_model <- lm(netrent ~ year)
summary(ll_model)

plot(year, netrent, pch=19, main = 'Scatterplot of Munchen Rent', 
     xlab = 'Year', ylab = 'Net rent')

abline(ll_model, col = 'seagreen', lwd = 2)
