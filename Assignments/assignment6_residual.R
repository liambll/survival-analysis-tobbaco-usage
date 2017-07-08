## Question 17 - Chapter 5
y <- c(0:10)
yhat_list <- c(2)
yhat_list <- c(1, 0.5, 0.25, 0.1)
for (yhat in yhat_list) {
  pear <- (y-yhat)/sqrt(yhat)
  pp <- 1-ppois(y, yhat)
  pn <- 1-pnorm(pear)
  print(paste("yhat: ", yhat))
  print(cbind(y, pear, pp, pn))
}

