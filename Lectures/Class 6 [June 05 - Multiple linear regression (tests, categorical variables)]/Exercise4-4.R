x <- c("a", "a", "a", "b", "b", "b", "b", "c", "c", "c") ## 3 categories = 3 levels
y <- c(5, 7, 5, 4, 5, 5, 6, 4, 4, 3)

lm1 <- lm(y ~ x)  ## x is automatically transformed into a factor in lm
summary(lm1)
coef(lm1)

str(x)
x <- as.factor(x) ## transform into a factor
levels(x)

## design matrix of lm1
model.matrix(lm1)

xx <- relevel(x, ref="c")
x
xx

lm2 <- lm(y ~ xx)
summary(lm2)
model.matrix(lm2)
