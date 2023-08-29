##########################################################################################
#   Regression (Quiz-04)                                                                 #
#   Date: 6th July 2023                                                                  #
##########################################################################################


# Preparations
rm(list = ls(all.names = TRUE))

# load the dataset
df <- read.csv('Dataset/Rent-17807.csv')

attach(df)

head(df)

# Model m1 : Explain netrent by space.
# Model m2 : Explain netrent by space and space^2.
# Model m3 : Explain netrent by rooms and year and space .
# Model m4 : Explain netrent by space and space^2 and year and kitchen.

m1 <- lm(netrent ~ space)
m2 <- lm(netrent ~ space + I(space^2))
m3 <- lm(netrent ~ rooms + year + space)
m4 <- lm(netrent ~ space + I(space^2) + year + kitchen)



# Sol (a):
n <- nrow(df); n # n = 99

# Sol (b):
rss <- sum(residuals(m3)^2)

# sigma_hat = rss / (n-p-1) | here, p+1 is the total number of coefficients
sigma_hat <- sqrt(rss / (n - length(coef(m3))))
sigma_hat # 183.8335

# Sol(c):

# Using model m4, predict the net rent for an appartment of 60 square metres that was built in 1975 and has no upscale kitchen:
new_data <- data.frame(space=c(60), year=c(1975), kitchen=c("no")); new_data

predict(m4, newdata = new_data)
# 539.9545 


# Sol(d):
AIC(m1, m2, m3, m4)

# df      AIC
# m1  3 1337.161
# m2  4 1338.366
# m3  5 1319.245
# m4  6 1328.290

# answer: m3


# Sol(e):
summary(m3)

# sol (f):

png("Resource/quiz_4.png")

plot(space, netrent)
abline(m1, lwd="2", col="seagreen")

b2_hat <- coef(m2)

curve(b2_hat[1] + b2_hat[2]*x +b2_hat[3]*x^2, 
      col="darkblue",
      lwd = 3, add=TRUE)

dev.off()

# sol (g):
new_data2 <- data.frame(space=c(60,60), year=c(1975, 1975), kitchen=c("yes", "no")); new_data2

predict(m4, newdata = new_data2)
#        1(yes)       2(no) 
#        729.5043     539.9545 
# Upscale kitchen increase the netrent if we predict m4 using same variables

# sol (h):
# use anova for f-test
anova(m1, m2) # m1 is best, as no significance found at 10%

anova(m1, m3) # m3 is best

anova(m1, m4) # m4 is best

anova(m2, m3) # m3 is best

anova(m2, m4) # m4 is best

anova(m3, m4) # m3 is best, as no significance found at 10%

