## Ex 4/5
#################################################################
## R code contributed by 	Robert Wienroeder


########## Exercise 5 ##########
########## (a) & (b)  ##########

# Clear the working environment
rm(list = ls(all.names = TRUE))

###                       ###
### --- Create Values --- ###
###                       ###

# Create a sequence of values for x-axis
x <- seq(-6, 6, length.out = 100)

# Calculate PDF & CDF values for standard logistic distribution
pdf_logistic <- dlogis(x)
cdf_logistic <- plogis(x)

# Calculate PDF & CDF values for standard normal distribution
pdf_normal <- dnorm(x)
cdf_normal <- pnorm(x)


###                       ###
### --- Create Plots ---  ###
###                       ###

# Set up the plot layout
par(mfrow = c(2, 2))

################### PDF ###################

# Plot PDF for standard logistic distribution
plot(x, pdf_logistic, type = "l", lwd = 2,
     xlab = "x", ylab = "Density",
     main = "PDF Comparison: Standard Logistic vs Standard Normal",
     ylim = c(0,0.4))

# Add PDF curve for standard normal distribution
lines(x, pdf_normal, col = "red", lwd = 2)

# Add legend
legend("topleft", legend = c("Standard Logistic", "Standard Normal"),
       col = c("black", "red"), lwd = 2, bty = "n")

################### CDF ###################

# Plot CDF for standard logistic distribution
plot(x, cdf_logistic, type = "l", lwd = 2,
     xlab = "x", ylab = "Probability",
     main = "CDF Comparison: Standard Logistic vs Standard Normal")

# Add CDF curve for standard normal distribution
lines(x, cdf_normal, col = "red", lwd = 2)

# Add legend
legend("topleft", legend = c("Standard Logistic", "Standard Normal"),
       col = c("black", "red"), lwd = 2, bty = "n")



########## (c) ##########

###                       ###
### --- Create Values --- ###
###                       ###

# Calculate standard deviation of standard logistic distribution (given from task 4)
pi/sqrt(3) # result is 1.81

# Calculate PDF values for adjusted normal distribution
pdf_normal_adj <- dnorm(x, mean = 0, sd = 1.61) # yields better result than sd=1.81
cdf_normal_adj <- pnorm(x, mean = 0, sd = 1.71) # yields better result than sd=1.81


###                       ###
### --- Create Plots ---  ###
###                       ###

################### PDF ###################

# Plot PDF for standard logistic distribution
plot(x, pdf_logistic, type = "l", lwd = 2,
     xlab = "x", ylab = "Density",
     main = "PDF Comparison: Standard Logistic vs Adjusted Normal",
     ylim = c(0,0.4))

# Add PDF curve for adjusted normal distribution
lines(x, pdf_normal_adj, col = "red", lwd = 2)

# Add legend
legend("topleft", legend = c("Standard Logistic", "Adjusted Normal \n(sd=1.61)"),
       col = c("black", "red"), lwd = 2, bty = "n")

################### CDF ###################

# Plot CDF for standard logistic distribution
plot(x, cdf_logistic, type = "l", lwd = 2,
     xlab = "x", ylab = "Probability",
     main = "CDF Comparison: Standard Logistic vs Adjusted Normal")

# Add CDF curve for adjusted normal distribution
lines(x, cdf_normal_adj, col = "red", lwd = 2)

# Add legend
legend("topleft", legend = c("Standard Logistic", "Adjusted Normal \n(sd=1.71)"),
       col = c("black", "red"), lwd = 2, bty = "n")

