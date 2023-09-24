###############################################################################################
#   Regression (Exercise-04)                                                                  #
#   Date: 12th June 2023                                                                      #
###############################################################################################


# Preparations
rm(list = ls(all.names = TRUE))



#Exercise 1

#(a) Plot the pdf of the chi2-distribution for different degrees of freedom. See ?dchisq. How
# would you describe the effect of an increasing df parameter?

par(mfrow=c(1,1))

X <- seq(from=-5, to=5, by=0.1)

cols <- rainbow(10)

Y <- dchisq(X, df=1)

plot.new()
plot(
  X, Y, col=cols[1], type="l", xlim=c(-0.5,5), ylim=c(0, 1), main="Chi^2"
)

for (i in 2:5){
  lines(X, dchisq(X, df=i), col=cols[i])
}
legend(
  "topright",
  legend=c("df=1", "df=2", "df=3", "df=4", "df=5"),
  col=cols[1:5],
  lty=c(1,1,1,1,1)
)


# (b) Plot the pdf of the t-distribution for different degrees of freedom. See ?dt. What happens
# to the distribution when df increases? (You may also ask Google to answer. ,)

plot.new()
plot(
  X, dt(X, 1), type="l", col=cols[1],
  ylim=c(0, 0.4), main="t"
)
for (i in 2:5){
  lines(X, dt(X, i), col=cols[i])
}
lines(
  X, dnorm(X), col="blue"
)
legend(
  2.5, 0.4,
  legend=c("df=1", "df=2", "df=3", "df=4", "df=5","normal"),
  col=c(cols[1:5],"blue"),  ## normal added to legend (MM)
  lty=c(1,1,1,1,1)
)

lines(X, dt(X, 20))



# (c) Generate artificial data for different chi2- and t-distributions. You should use sufficiently
# large sample sizes and calculate means and variances. What is your guess about the expectations 
# for both chi^2- and t as well as for the variance of chi2?

chi_sq_data_1 <- rchisq(100000, 1)
chi_sq_data_3 <- rchisq(100000, 3)
chi_sq_data_6 <- rchisq(100000, 6)
chi_sq_data_40 <- rchisq(100000, 40)
chi_sq_data_100 <- rchisq(100000, 100)
chi_sq_data_500 <- rchisq(100000, 500)

r_data_1 <- rt(100000, 1)
r_data_3 <- rt(100000, 3)
r_data_6 <- rt(100000, 6)
r_data_40 <- rt(100000, 40)
r_data_100 <- rt(100000, 100)
r_data_500 <- rt(100000, 500)

chi_sq_means <- c(mean(chi_sq_data_1), mean(chi_sq_data_3), mean(chi_sq_data_6), mean(chi_sq_data_40),
                  mean(chi_sq_data_100), mean(chi_sq_data_500))

chi_sq_vars <- c(var(chi_sq_data_1), var(chi_sq_data_3), var(chi_sq_data_6), var(chi_sq_data_40),
                 var(chi_sq_data_100),  var(chi_sq_data_500))

t_means <- c(mean(r_data_1), mean(r_data_3), mean(r_data_6), mean(r_data_40), 
             mean(r_data_100), mean(r_data_500))

t_vars <- c(var(r_data_1), var(r_data_3), var(r_data_6), var(r_data_40), 
            var(r_data_100), var(r_data_500))

data.frame(
  "df" = c(1, 3, 6, 40, 100, 500),
  "Chi squared means"=chi_sq_means,
  "Chi squared vars"=chi_sq_vars,
  "T means"=t_means,
  "T vars"=t_vars
)


####################################################################################################


# Exercise 2
# We generate artificial regression data:
x <- runif(10)
y <- 2 - 2*x + 0.5*x^2 + rnorm(length(x), sd=0.2)
lm1 <- lm( y ~ x )
lm2 <- lm( y ~ x + I(x^2))
lm3 <- lm( y ~ x + I(x^2) + I(x^3) )


# (a) Do a scatterplot of the data and graphically display the 3 estimated regression functions.

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



####################################################################################################


# Exercise 3
par(mfrow=c(1,1))

# load dataset
data <- read.csv("Dataset/AnscombeQuartet.csv")

# view first 6 rows
head(data)

regmodel <- function(X, Y, x_lab, y_lab) {
  
  # defining the linear model object
  lobj <- lm(Y ~ X)
  
  b_hat <- lobj$coefficients
  
  Y_hat <- b_hat[1] + (X * b_hat[2])
  
  rss <- sum((Y-Y_hat)^2)
  
  print(rss)
  
  # print the r-squared
  print(summary(lobj)$r.squared)
  
  # plot the model
  plot(X, Y, pch="+", xlab = x_lab, ylab = y_lab)
  
  # draw the regression line
  abline(lobj, col="seagreen", lwd = 2)
}

par(mfrow=c(2,2))

# call the function
regmodel(data$x1, data$y1, 'x1', ' y1')
regmodel(data$x2, data$y2, 'x2', ' y2')
regmodel(data$x3, data$y3, 'x3', ' y3')
regmodel(data$x4, data$y4, 'x4', ' y4')

par(mfrow=c(1,1))

