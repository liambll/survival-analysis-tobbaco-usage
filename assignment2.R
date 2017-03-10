# Question 18
## Part a:
c.table <- array(data = c(135, 15, 434, 9), dim=c(2,2), 
                 dimnames = list(Condom=c("Never", "Ever"), HIV=c("Positive", "Negative")))
pi.hat.table <- c.table/rowSums(c.table)
alpha <- 0.05
pi.hat1 = pi.hat.table[1,1]
pi.hat2 = pi.hat.table[2,1]

# Wald interval
var.wald <- pi.hat1*(1 - pi.hat1)/sum(c.table[1,]) +
  pi.hat2*(1 - pi.hat2)/sum(c.table[2,])
pi.hat1 - pi.hat2 + qnorm(p = c(alpha/2, 1-alpha/2))*sqrt(var.wald)

# Agresti- Caffo interval
pi.tilde1 <- (c.table[1,1] + 1)/(sum(c.table[1,]) + 2)
pi.tilde2 <- (c.table[2,1] + 1)/(sum(c.table[2,]) + 2)
var.AC <- pi.tilde1*(1 - pi.tilde1)/(sum(c.table[1,]) + 2) +
  pi.tilde2*(1 - pi.tilde2)/(sum(c.table[2,]) + 2)
pi.tilde1 - pi.tilde2 + qnorm(p = c(alpha/2, 1- alpha/2))*sqrt(var.AC)

## Part b
# Score Test and Pearson Chi-square
prop.test (x = c.table, conf.level = 1-alpha, correct = FALSE)

# LRT
pi.bar <- colSums(c.table)[1]/sum(c.table)
log.Lambda <- c.table[1,1]*log(pi.bar/pi.hat.table[1,1]) +
  c.table[1,2]*log((1-pi.bar)/(1-pi.hat.table[1,1])) +
  c.table[2,1]*log(pi.bar/pi.hat.table[2,1]) +
  c.table[2,2]*log((1-pi.bar)/(1-pi.hat.table[2,1]))
test.stat <- -2*log.Lambda
crit.val <- qchisq(p = 1-alpha, df = 1)
p.val <- 1 - pchisq(q = test.stat, df = 1)
round(data.frame(pi.bar, test.stat, crit.val, p.val, row.names = NULL), 4)

## Part c
# Odds Ratio
OR.hat <- c.table[1,1]*c.table[2,2]/(c.table[2,1]*c.table[1,2])
round(OR.hat,4)

var.log.or <- 1/c.table[1,1] + 1/c.table[1,2] + 1/c.table[2 ,1] + 1/c.table [2,2]
OR.CI <- exp(log(OR.hat) + qnorm(p=c(alpha/2, 1-alpha/2))*sqrt(var.log.or))
round (OR.CI , 2)




