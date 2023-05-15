#################################################################################################
#   Regression (Exercise-02)                                                                    #
#   Date: 13th May 2023                                                                         #
#################################################################################################


# Preparations
rm(list = ls(all.names = TRUE))


# Exercise 1

sales <- c(1585, 1819, 1647, 1496, 921, 1278, 1810, 1987, 1612, 1413)
price <- c(12.50, 10.00, 9.95, 11.50, 12.00, 10.00, 8.00, 9.00, 9.50, 12.50)

# making data frame
df <- data.frame(sales, price)

# print summary
str(df)

# Solution (a):
plot(df)

# Relation between sales and price: The cheaper the price the sales increase. So, the correlation
# coefficient would be negative in sign.

# Solution (b):
lr_model <- lm(df$price ~ df$sales)

summary(lr_model)$coefficient

# b0_hat (intercept) = 15.373424692
# b1_hat (slope) =  -0.003133623

abline(lr_model, col='seagreen', lwd = 2)


# Solution (c):
r_xy <- cor(df$sales, df$price); r_xy # -0.6172626

b1_hat <- -0.003133623

sx <- sd(df$sales); sx
sy <- sd(df$price); sy

new_data <- b1_hat * (sx / sy); new_data # -0.6172626

# Solution (d):

sales_13 <- (13 - 15.373424692) / b1_hat; sales_13



# Solution (e):

# I used Bangladeshi Taka
# Where 1 EUR = 115.662 BDT
price_bdt <- c(12.50, 10.00, 9.95, 11.50, 12.00, 10.00, 8.00, 9.00, 9.50, 12.50) * 115.662
price_bdt

# change layout for drawing multiple plot
par(mfrow = c(2, 1))

# draw original figure
plot(df, main = 'Point Cloud of Sales and Price (EUR)', ylab = 'Price (EUR)')
abline(lr_model, col='seagreen', lwd = 2)


# draw with converted price
df_bdt <- data.frame(sales, price_bdt)
plot(df_bdt, main = 'Point Cloud of Sales and Price (BDT)', ylab = 'Price(BDT)')

# model with converted data
lr_model_bdt <- lm(df_bdt$price_bdt ~ df_bdt$sales)
abline(lr_model_bdt, col='seagreen', lwd = 2)

rxy_bdt <- cor(df_bdt$sales, df_bdt$price_bdt); rxy_bdt     # -0.6172626
r2_bdt <- summary(lr_model_bdt)$r.squared; r2_bdt           # 0.3810132

r2_original <- summary(lr_model)$r.squared; r2_original     # 0.3810132 

#################################################################################################

# Exercise 2

# reset the plotting window again
par(mfrow = c(1, 1))

# Solution (a):

simple.lm <- function(x, y){
  # mean of x
  mx <- mean(x)
  
  #mean of y
  my <- mean(y)
  
  # variance of x, and y
  sx2 <- var(x)
  sy2 <- var(y)
  
  # Co-variance calculation
  sxy <- cov(x, y)
  
  # regression coefficient calculation
  b1_hat  <- sxy / sx2
  b0_hat <- my - (mx * b1_hat)
  
  return ( list(b0_hat = b0_hat, b1_hat = b1_hat))
}

# Solution (b):

# read the file
rent <- read.csv('Dataset/MunichRent2003.csv')

# view data frame
View(rent)

# attached columns
attach(rent)

# get the regression coefficient 
reg_coeff <- simple.lm(x = living.space, y = netrent)

# print the value of the coefficient
str(reg_coeff)

# plot the point cloud
plot(netrent ~ living.space)

# draw the regression line
abline(reg_coeff$b0_hat, reg_coeff$b1_hat, col = 'seagreen', lwd=3)


# Solution (c):

R2 <- function(x, y, b0, b1){
  y_hat <- b0 + (b1 * x)
  
  sy_hat2 <- var(y_hat)
  
  sy2 <- var(y)
  
  #return(y_hat)
  return(sy_hat2 / sy2)
}

# Solution (d):

# my function output
calculated_r2 <- R2(living.space, netrent, reg_coeff$b0_hat, reg_coeff$b1_hat); calculated_r2

# linear modeling
ll_reg <- lm(netrent ~ living.space)
summary(ll_reg)$r.squared



#################################################################################################

# Exercise 3

# Solution (a):

# value of c vector

# initialize c
C <- c(2, 3, 1, 5, 7)

# initialize X
X <- seq(-5, 5, by=0.1); X

# the poly_eval function calculate all the respective value of each X value
poly_eval <- function(X, C){
  
  # init Y with empty vector
  Y <- c()
  
  # iterate over all X
  for (x in X){
    
    # set y, and i=0
    y <- 0
    i <- 0
    
    # iterate for all value of C
    for (c in C) {
      
      # calculate the polynomial term
      y <- y + (c * (x ^ i))
      
      i <- i  + 1
    }
    
    Y <- append(Y, y)
  }
  
  
  return(Y)
}

Y <- poly_eval(X, C)


# Solution (b):

ll <- as.integer(readline('Enter lower limit: '))
up <- as.integer(readline('Enter upper limit: '))

X = seq(ll, up, by=0.1)

Y <- poly_eval(X, C); Y

# Solution (c):
plot(X, Y)

plot_fun <- function(X, Y){
  color <- readline('Enter color: ')
  line_style <- as.integer(readline('Enter line style: '))
  titles <- readline('Enter title of the graph: ')
  
  plot(X, Y, col = color, main = titles, lty = line_style)
  
}


# lty = 0: Blank line
# lty = 1: Solid line (default)
# lty = 2: Dashed line
# lty = 3: Dotted line
# lty = 4: Dot-dashed line
# lty = 5: Long-dashed line
# lty = 6: Two alternating dashes and dots

plot_fun(X, Y)








#################################################################################################

# Exercise 4

# set sample size
n <- 100

# init X
X_calculation <- function(n){
  return(rnorm(n, mean = 0, sd = 1))
}

X <- X_calculation(n); X

# init error term
sigma2 <- 0.49

error_calculation <- function(n, sigma2){
  return(rnorm(n, mean = 0, sd = sqrt(sigma2)))
}

e <- error_calculation(n, sigma2); e

# chose b0 and b1
b0 <- 10
b1 <- 0.8

# calculate value of Y
Y_calculation <- function(b0, b1, X, e){
  return(b0 + (b1 * X) + e)
}

# Solution (a):

reg_func <- function(X, b0, b1, e){
  
  Y <- Y_calculation(b0, b1, X, e)
  
  # draw X, Y in point cloud
  plot(Y ~ X, main='Point cloud of X and Y')
  
  # linear regression model
  ll_model <- lm(Y ~ X)
  
  
  coef <- ll_model$coefficients
  print(coef)
  
  abline(ll_model, col='seagreen', lwd = 3)
  abline(b0, b1, col='darkorange2', lwd=3)
  
  legend("topleft", legend = c("Estimated", "True"), col = c("seagreen", "darkorange2"), 
         lty = 1, lwd=2)
}

reg_func(X, b0, b1, e)


# Solution (b):

tuning_n_and_sigma <- function(n, sigma2, b0, b1){
  X <- X_calculation(n)
  e <- error_calculation(n, sigma2)
  
  reg_func(X, b0, b1, e)
}

tuning_n_and_sigma(n = 100, sigma2 = 0.49, b0, b1)
tuning_n_and_sigma(n = 50, sigma2 = 0.25, b0, b1)
tuning_n_and_sigma(n = 10, sigma2 = 0.64, b0, b1)
tuning_n_and_sigma(n = 1000, sigma2 = 0.64, b0, b1)


# Solution (c):

tuning_n_and_sigma(n = 50, sigma2 = 0.25, b0, b1)
tuning_n_and_sigma(n = 50, sigma2 = 0.25, b0, b1)

# we can set a set for before calculating X

# init X
X_calculation <- function(n){
  set.seed(123)
  return(rnorm(n, mean = 0, sd = 1))
}

tuning_n_and_sigma(n = 50, sigma2 = 0.25, b0, b1)
tuning_n_and_sigma(n = 50, sigma2 = 0.25, b0, b1)


# update reg_function to save X, and Y

reg_func <- function(X, b0, b1, e){
  # get value of Y
  Y <- Y_calculation(b0, b1, X, e)
  
  # make data frame
  df <- data.frame(X, Y)
  
  write.csv(df, file = "Dataset/new_data.csv", row.names = FALSE)
  
  # draw X, Y in point cloud
  plot(Y ~ X, main='Point cloud of X and Y')
  
  # linear regression model
  ll_model <- lm(Y ~ X)
  
  
  coef <- ll_model$coefficients
  print(coef)
  
  abline(ll_model, col='seagreen', lwd = 3)
  abline(b0, b1, col='darkorange2', lwd=3)
  
  legend("topleft", legend = c("Estimated", "True"), col = c("seagreen", "darkorange2"), 
         lty = 1, lwd=2)
}

tuning_n_and_sigma(n = 50, sigma2 = 0.25, b0, b1)
tuning_n_and_sigma(n = 50, sigma2 = 0.25, b0, b1)
 