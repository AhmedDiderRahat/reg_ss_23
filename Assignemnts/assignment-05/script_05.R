###############################################################################################
#   Regression Exercise-05                                                                    #
#   Date: 1st July 2023                                                                       #
###############################################################################################

# Preparations
rm(list = ls(all.names = TRUE))

# load the dataset
library(AER)
data("Affairs")

# summary if the data
str(Affairs)

# attach the variables
attach(Affairs)

# check the variable affairs
table(affairs)

# make new variable Y
Affairs$Y <- as.numeric(Affairs$affairs > 0)
attach(Affairs)

table(Y)


# (a) Explore graphically the effect of single explanatory variables on Y (use for example: spineplot, barplot, or mosaicplot).

mosaicplot(table(Y, gender), col=c("red","blue"))
mosaicplot(table(Y, children), col=(c("orange","green")))
mosaicplot(table(Y, rating), col=rainbow(5))


mosaicplot()

head(Affairs)
