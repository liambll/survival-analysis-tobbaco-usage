## Question 4 - Chapter 4
stoplight <- read.csv(file = "stoplight.csv")
head(stoplight)

# Poisson Distribution
mu.hat <- mean(stoplight$vehicles)
threshold = 9
p_threshold = ppois(9, lambda=mu.hat, lower=FALSE)
p_threshold

# Binomial Distribution
N = 60
pbinom(1, size=N, prob=p_threshold, lower=FALSE)