# Question 9:
# Wald CI
w <- 79
n <- 80
alpha <- 0.05
pi.hat <- w/n
var.wald <- pi.hat * (1 - pi.hat)/n
lower <- pi.hat - qnorm(p = 1 - alpha/2)*sqrt(var.wald)
upper <- pi.hat + qnorm(p = 1 - alpha/2)*sqrt(var.wald)
round(data.frame(lower,upper), 4)

# Wilson CI
p.tilde <- (w + qnorm(p = 1 - alpha/2)^2/2) / (n + qnorm(p = 1- alpha/2)^2)
round(p.tilde + qnorm(p = c(alpha /2, 1- alpha/2))*sqrt(n)/(n + qnorm(p = 1- alpha/2)^2)*sqrt(pi.hat*(1 - pi.hat) + qnorm(p=1- alpha/2)^2/(4*n)), 4)

# Question 15:
# Caculate expected length
expected_length <- function(n, alpha, pi) {
  w <- 0:n
  pi.hat <- w/n
  var.wald <- pi.hat * (1 - pi.hat)/n
  lower <- pi.hat - qnorm(p = 1 - alpha/2)*sqrt(var.wald)
  upper <- pi.hat + qnorm(p = 1 - alpha/2)*sqrt(var.wald)
  length = upper - lower
  length.expected = length*choose(n,w)*(pi^w)*((1-pi)^(n-w))
  return(sum(length.expected)) }
n <- 40
alpha <- 0.05
pi = 0.16
length = expected_length(n, alpha, pi)
round(length, 4)

# Caculate expected length using binom
library(binom)
n <- 40
alpha <- 0.05
wald.expected = binom.length(pi, n, conf.level = 1-alpha, method = "asymptotic")
round(wald.expected$length, 4)

# Plot
n <- 40
alpha <- 0.05
# All pi's
pi.seq<-seq(from = 0.001, to = 0.999, by = 0.0005)
# Save expected length in a matrix
save.length.expected<-matrix(data = NA, nrow = length(pi.seq), ncol = 5)
# Create counter for the loop
counter<-1
# Loop over each pi that the expected length is calculated on
for(pi in pi.seq) {
  wald<-binom.length(pi, n, conf.level = 1-alpha, method = "asymptotic")$length
  AC<-binom.length(pi, n, conf.level = 1-alpha, method = "agresti-coull")$length
  wilson<-binom.length(pi, n, conf.level = 1-alpha, method = "wilson")$length
  CP<-binom.length(pi, n, conf.level = 1-alpha, method = "exact")$length
  save.length.expected[counter,]<-c(pi, wald, AC, wilson, CP)
  counter<-counter+1
}

x11(width = 7, height = 6, pointsize = 12)
plot(x = save.length.expected[,1], y = save.length.expected[,2], main = "Expected Length", xlab = expression(pi), ylab = "Expected Length", type = "l", ylim = c(0,0.35))
lines(x = save.length.expected[,1], y = save.length.expected[,3], xlab = expression(pi),
      ylab = "Expected Length", type = "l", ylim = c(0,0.35), col='blue')
lines(x = save.length.expected[,1], y = save.length.expected[,4], xlab = expression(pi),
      ylab = "Expected Length", type = "l", ylim = c(0,0.35), col='red')
lines(x = save.length.expected[,1], y = save.length.expected[,5], xlab = expression(pi),
      ylab = "Expected Length", type = "l", ylim = c(0,0.35), col='green')
legend("center", c("Wald", "Agresti-Coull", "Wilson", "Clopper-Pearson"),
       lty=c(1,1,1,1), lwd=c(2.5,2.5,2.5,2.5),col=c('black','blue','red','green'))

# Simulation:
n = 40
alpha = 0.05
pi = 0.16
numb.bin.samples <- 10000 # Binomial samples of size n
w <- rbinom(n = numb.bin.samples, size = n, prob = pi)
pi.hat <- w/n
var.wald <- pi.hat*(1 - pi.hat)/n
lower <- pi.hat - qnorm(p = 1- alpha/2) * sqrt(var.wald)
upper <- pi.hat + qnorm(p = 1- alpha/2) * sqrt(var.wald)
length = upper - lower
expected.length = mean(length)
round(expected.length, 4)
