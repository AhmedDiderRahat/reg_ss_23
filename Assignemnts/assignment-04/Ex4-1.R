## Ex 4/1
##############################################################
## R code contributed by Tim Keno Bischoff + titles added (MM)

par(mfrow=c(1,1))
cols <- rainbow(10)
x <- seq(from=-5, to=5, by=0.01)

# a)
plot.new()
plot(
  x, dchisq(x, df=1), col=cols[1], type="l", xlim=c(-0.5,5), ylim=c(0, 1), main="Chi^2"
)
for (i in 2:5){
  lines(x, dchisq(x, df=i), col=cols[i])
}
legend(
  "topright",
  legend=c("df=1", "df=2", "df=3", "df=4", "df=5"),
  col=cols[1:5],
  lty=c(1,1,1,1,1)
)

# b)
plot.new()
plot(
  x, dt(x, 1), type="l", col=cols[1],
  ylim=c(0, 0.4), main="t"
)
for (i in 2:5){
  lines(x, dt(x, i), col=cols[i])
}
lines(
  x, dnorm(x), col="blue"
)
legend(
  2.5, 0.4,
  legend=c("df=1", "df=2", "df=3", "df=4", "df=5","normal"),
  col=c(cols[1:5],"blue"),  ## normal added to legend (MM)
  lty=c(1,1,1,1,1)
)

# c)
chi_sq_data_1 <- rchisq(100000, 1)
chi_sq_data_3 <- rchisq(100000, 3)
chi_sq_data_6 <- rchisq(100000, 6)
chi_sq_data_9 <- rchisq(100000, 40)

r_data_1 <- rt(100000, 1)
r_data_3 <- rt(100000, 3)
r_data_6 <- rt(100000, 6)
r_data_9 <- rt(100000, 40)

chi_sq_means <- c(mean(chi_sq_data_1), mean(chi_sq_data_3), mean(chi_sq_data_6), mean(chi_sq_data_9))
chi_sq_vars <- c(var(chi_sq_data_1), var(chi_sq_data_3), var(chi_sq_data_6), var(chi_sq_data_9))
t_means <- c(mean(r_data_1), mean(r_data_3), mean(r_data_6), mean(r_data_9))
t_vars <- c(var(r_data_1), var(r_data_3), var(r_data_6), var(r_data_9))

data.frame(
  "Chi squared means"=chi_sq_means,
  "Chi squared vars"=chi_sq_vars,
  "T means"=t_means,
  "T vars"=t_vars,
  row.names=c(1, 3, 6, 9)
)
