###############################################################################################
#   Regression (Exercise-03)                                                                  #
#   Date: 18th May 2023                                                                       #
###############################################################################################

# # Source: https://github.com/AhmedDiderRahat/reg_ss_23/blob/main/Assignemnts/assignment-03/RegSSE3.pdf


# Preparations
rm(list = ls(all.names = TRUE))


# Exercise 1


# set directory: 
# setwd('C:\\Users\\DELL\\Desktop\\Summer 2023\\RegDS23\\reg_ss_23')

# load Munich rent data set
rent <- read.csv('Dataset/MunichRent2003.csv')

# attached all columns
attach(rent)


# Solution (a):

prediction_fun <- function(X, Y, x_lab){
  par(mfrow = c(2, 1))
  # create an object of linear regression
  lm_obj <- lm(Y ~ X)
  
  y_hat <- predict(lm_obj)
  
  print(summary(lm_obj))
  
  plot(X, Y, 
       main = paste('Scatter plot of ', x_lab, ' and netrent'),
       xlab = x_lab,
       ylab = 'netrent')
  
  abline(lm_obj, col = 'seagreen', lwd = 3)
  
  e_hat <- (y_hat - netrent)
  
  plot(y_hat, e_hat)
  
  par(mfrow = c(1, 1))
  
  return(list(y_hat = y_hat, e_hat = e_hat))
}

prediction_fun(X = living.space, Y = netrent, 'Living Space')
prediction_fun(X = rooms, Y = netrent, 'Number of Room')


# Solution (b):

proved_fun <- function(X, Y, X_lab){
  
  reg_out <- prediction_fun(X = living.space, Y = netrent, 'Living Space')
  
  Y_hat <- reg_out$y_hat
  e_hat <- reg_out$e_hat
  
  # calculate mean of Y
  mean_Y <- mean(Y)
  
  # calculate mean of Y_hat
  mean_Y_hat <- mean(Y_hat)
  
  # calculate mean of error term
  mean_e_hat <- mean(e_hat)
  
  output <- paste('\n\nThe output of the code: ',
        '\nMean of (Y) is: ', mean_Y, ' AND mean of (y_hat) is: ', mean_Y_hat, 
        '\nMean of (e_hat) is: ', mean_e_hat)
  
  cat(output)
}

proved_fun(X = living.space, Y = netrent, 'Living Space')




###############################################################################################


# Exercise 2

# set sample size n = 500
n = 10000

set.seed(123)

# init X
X <- rnorm(n, mean = 0, sd = 1)

# init e
e <- rnorm(n, mean = 0, sd = sqrt(0.25))

# calculate Y
Y <- 1 + (2*X) + (X^2) + e


# set three regression model
lm1 <- lm(Y ~ X)                     # Simple linear regression
lm2 <- lm(Y ~ X + I(X^2))            # multiple (quadratic)
lm3 <- lm(Y ~ X + I(X^2) + I(X^3))   # multiple (cubic)

# plot the data
plot(X, Y)

# draw lm1
abline(lm1, col = 'seagreen', lwd = 3)

b_hat2 <- coef(lm2); b_hat2


curve(b_hat2[1] + b_hat2[2]*x +b_hat2[3]*x^2, 
      col="darkblue",
      lwd = 3, add=TRUE)


b_hat3 <- coef(lm3); b_hat3

curve(b_hat3[1] + b_hat3[2]*x + (b_hat3[3]*(x^2)) + (b_hat3[4]*(x^3)),
      col="darkorange2", lwd = 3, add=TRUE)

model_summary <- (paste('RSS for linear regression:     ', deviance(lm1), 
            '\nRSS for quardratic regression: ', deviance(lm2),
            '\nRSS for cubic regression:      ', deviance(lm3)))

cat(model_summary)


## compare residuals for lm1, lm2, and lm3
par(mfrow=c(1,3))
ylim <- range(c(residuals(lm1),residuals(lm2), residuals(lm3)))

plot(X,residuals(lm1), main="Residual plot: lm1", ylim=ylim)
abline(h=0,col="gray")
plot(X,residuals(lm2), main="Residual plot: lm2", ylim=ylim)
abline(h=0,col="gray")

plot(X,residuals(lm3), main="Residual plot: lm3", ylim=ylim)
abline(h=0,col="gray")


par(mfrow=c(1,1))



###############################################################################################


# Exercise 3

n = 500

set.seed(123)

# init X
X <- rnorm(n, mean = 0, sd = 1)

# init e
e <- rnorm(n, mean = 0, sd = sqrt(0.25))

# calculate Y
Y <- 1 + (2*X) + (X^2) + e

plot(X, Y)



# Solution (a):

# design matrix for lm1
X1 <- cbind(rep(1,n), X); X1 

# design matrix of lm2
X2 <- cbind(rep(1, n), X, X^2); X2

# design matrix of lm3
X3 <- cbind(rep(1, n), X, X^2, X^3); X3


# Solution (b):

# Rank of a Matrix: The rank of a matrix is the maximum number of linearly 
# independent rows or columns in the matrix. 


# install matrix package
install.packages('Matrix', dep=T)

# load Matrix library
library(Matrix)

# Calculate the rank of the matrix
rank_x1 <- rankMatrix(X1)[1]
rank_x2 <- rankMatrix(X2)[1]
rank_x3 <- rankMatrix(X3)[1]

XTX1 <- t(X1) %*% X1

XTX2 <- t(X2) %*% X2

XTX3 <- t(X3) %*% X3

rank_xtx1 <- rankMatrix(XTX1)[1]
rank_xtx2 <- rankMatrix(XTX2)[1]
rank_xtx3 <- rankMatrix(XTX3)[1]

output_summary <- paste('Rank of X1:', rank_x1, 'AND IS FULL RANK-', (ncol(X1)  == rank_x1),
                        '\nRank of XTX1:', rank_xtx1, 'AND IS FULL RANK- ', (ncol(XTX1)  == rank_xtx1),
                        '\nRank of X2:', rank_x2, 'AND IS FULL RANK-', (ncol(X2)  == rank_x2),
                        '\nRank of XTX2:', rank_xtx2, 'AND IS FULL RANK-', (ncol(XTX2)  == rank_x2), 
                        '\nRank of X3:', rank_x3, 'AND IS FULL RANK-', (ncol(X3)  == rank_x3),
                        '\nRank of XTX3:', rank_xtx3, 'AND IS FULL RANK-', (ncol(XTX3)  == rank_xtx3))
cat(output_summary)


# Solution (c):

hat_matric_fun <- function(X){
  
  # get the inverse matrix of XTX matrix
  det_xtx <- solve(t(X) %*% X)
  
  P <- X %*% det_xtx %*% t(X)
  
  return(P)
}


P1 <- hat_matric_fun(X1); h1
P2 <- hat_matric_fun(X2); h2
P3 <- hat_matric_fun(X3); h3

# The trace of a square matrix is defined as the sum of its diagonal elements.
t1 <- sum(diag(P1)); t1
t2 <- sum(diag(P2)); t2
t3 <- sum(diag(P3)); t3



# Eigenvalues of a square matrix represent the scalar values ?? for which the matrix
# multiplied by a vector is equal to a scalar multiple of that vector.

egv1 <- eigen(P1)$values; egv1
egv2 <- eigen(P2)$values; egv2
egv3 <- eigen(P3)$values; egv3

se1 <- sum(egv1)
# sum of all eigen values = the trace of the matrix

# Solution (d):

proving_func <- function(P){
  
  # calculate square of matrix A
  P2 <- P %*% P
  
  # check A2 and A are same
  eqn_1 <- all.equal(P2, P)
  
  # init identity matrix
  I <- diag(ncol(P))
  
  # get I - P 
  I_P <- (I - P)
  
  
  # calculate (I - P)^2
  I_P2 <- I_P %*% I_P
  
  # check (I - P)2 = (I - P)  
  eqn_2 <- all.equal(I_P2, I_P)
  
  return(list(eqn_1 = eqn_1, eqn_2 = eqn_2))
}


r1 <- proving_func(P1)
r2 <- proving_func(P2)
r3 <- proving_func(P3)

output_summary <- paste('For  model 1: ',
                        '\n\tEQN1 satisfied:', r1$eqn_1, 'AND EQN2 satisfied:', r1$eqn_2,
                        '\nFor  model 2: ',
                        '\n\tEQN1 satisfied:', r2$eqn_1, 'AND EQN2 satisfied:', r2$eqn_2,
                        '\nFor  model 3: ',
                        '\n\tEQN1 satisfied:', r3$eqn_1, 'AND EQN2 satisfied:', r3$eqn_2)


cat(output_summary)



M <- mat <- matrix(c(1, 4, 5, 4, 2, 6, 5, 6, 3), nrow = 3); M
M %*% M

P1



###############################################################################################

# install and load AER
install.packages("AER", dep = T)

require(AER)
data(CPS1985)

attach(CPS1985)

# init X and Y
Y <- wage
X1 <- education
X2 <- experience

lm1 <- lm(log(Y) ~ X1)

summary(lm1)

plot(X1, log(Y))
curve(predict(model, newdata = data.frame(X = x)), add = TRUE, col = "red")

b_hat1 <- coef(lm1); b_hat1

curve(b_hat1[1] + b_hat1[2]*x, 
      col="darkblue",
      lwd = 3, add=TRUE)


lm2 <- lm(log(Y) ~ X1+X2)

b_hat2 <- coef(lm2); b_hat2

a(b_hat2[1] + b_hat2[2]*X1, b_hat2[3]*X2,   
      col="darkblue",
      lwd = 3, add=TRUE)

abline(lm2)
