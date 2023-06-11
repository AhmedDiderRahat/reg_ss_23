##########################################################################################
#   Regression (Quiz-02)                                                                 #
#   Date: 25th May 2023                                                                  #
##########################################################################################


# Preparations
rm(list = ls(all.names = TRUE))

# load data
data <- read.csv('Dataset/Simple-32209.csv')

# head of the data
head(data)

attach(data)

# Estimate a simple linear regression to explain Y by X.
ll_reg <- lm(Y ~ X)

# Draw a scatterplot and add the estimated regression line.
plot(data)
abline(ll_reg, col = 'seagreen', lwd=2)

# sample size
n <- length(X); n # = 42

# intercept
coef(ll_reg)

# intercept: 0.1005218 
# slope: -1.4509552

# coefficient of determination
summary(ll_reg)
r2 <- summary(ll_reg)$r.squared; r2 # 0.401859

# RSS
RSS <- sum(residuals(ll_reg)^2); RSS # 41.55212

# predict X = 0.4
y_hat <- predict(ll_reg, data.frame(X = 0.9)); y_hat # = -1.205338 

