## Question 4 - Chapter 6
# Fisher Test
c.table <- array(data = c(51, 74, 8146, 8124), dim=c(2,2), 
                 dimnames = list(Treatment=c("Vaccine", "Placebo"), Response=c("HIV", "No_HIV")))
c.table
fisher.test(x = c.table)

# Permutation Test
set.seed(3)
save.p <- chisq.test(x = c.table, correct = FALSE, simulate.p.value = TRUE, B = 100000)
save.p
binom.confint(x = round(save.p$p.value*100000,0), n = 100000, conf.level = 1-0.05, methods = "wilson")