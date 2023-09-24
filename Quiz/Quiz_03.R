##########################################################################################
#   Regression (Quiz-03)                                                                 #
#   Date: 29th Jun 2023                                                                  #
##########################################################################################


# Preparations
rm(list = ls(all.names = TRUE))

# read the dataset
df <- read.csv('Dataset/Rent-17807.csv')

# head of the data
head(df)

# attached the variables
attach(df)

# sample size 
n <- nrow(df); n
# 99

# Models to estimate
# Model m1 : Explain netrent by space.
# Model m2 : Explain netrent by space and space^2.
# Model m3 : Explain netrent by rooms and year and space .
# Model m4 : Explain netrent by space and space^2 and year and kitchen.

m1 <- lm(netrent ~ space)
m2 <- lm(netrent ~ space + I(space^2), data = df)
m3 <- lm(netrent ~ rooms + year + space, data = df)



plot(space, netrent)
abline(m1)


b2_hat <- m2$coefficients; b2_hat


curve(b2_hat[1] + b2_hat[2]*x +b2_hat[3]*x^2, 
      col="darkblue",
      lwd = 3, add=TRUE)

summary(m3)

y3_hat <- predict(m3)

rss3 <- sum((y3_hat - netrent)^2)

sigma3 <- sqrt(rss3/95); sigma3
