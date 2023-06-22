###############################################################################################
#   Regression (Exercise-04)                                                                  #
#   Date: 12th June 2023                                                                      #
###############################################################################################


# Preparations
rm(list = ls(all.names = TRUE))


# Exercise 3

# load dataset
data <- read.csv("Dataset/AnscombeQuartet.csv")

# view first 6 rows
head(data)

regmodel <- function(X, Y, x_lab, y_lab) {
  
  # defining the linear model object
  lobj <- lm(Y ~ X)
  
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
