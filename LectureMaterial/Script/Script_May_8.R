#######################################################################################################
#   Statistical Computing Lecture-04                                                                  #
#   Date: 12st Jan 2022                                                                               #
#######################################################################################################

# Preparations
rm(list = ls(all.names = TRUE))

setwd("/Users/zeotaplaptop/Desktop/BHT-SS23/Regression/reg_ss_23")
getwd()

data <- read.csv2('Dataset/Marketing_Example.R')

attach(data)

summary(data)
str(data)
