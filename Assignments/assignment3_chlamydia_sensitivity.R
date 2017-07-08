## Chlamydia - Sensitivity
mod.fit <- glm(formula=Sensitivity ~ Gender + Specimen + Symptoms_Status, 
               family = binomial(link = logit), data = chlamydia, weights=Sample_size)
summary(mod.fit)

# Plot
chlamydia$gender.numb <- ifelse( test = chlamydia$Gender == "Female",
                              yes = 0, no = 1)
plot(x = chlamydia$gender.numb, y = chlamydia$sensitivity, ylim=c(0.9, 1.0), xlim=c(0, 1),
     xlab = "Gender", ylab = "Sensitivity", type = "n", xaxt = "n")
axis( side = 1, at = c(0,1), labels = c("Female", "Male"))
Swab.Symptomatic <- chlamydia$Specimen == "Swab" &
  chlamydia$Symptoms_Status == "Symptomatic"
lines(x = chlamydia$gender.numb[Swab.Symptomatic], y = chlamydia$Sensitivity[Swab.Symptomatic],
      type = "o", lty = "solid", col = "black")

Swab.Asymptomatic <- chlamydia$Specimen == "Swab" &
  chlamydia$Symptoms_Status == "Asymptomatic"
lines(x = chlamydia$gender.numb[Swab.Asymptomatic], y = chlamydia$Sensitivity[Swab.Asymptomatic],
      type = "o", lty = "solid", col = "blue")

Urine.Symptomatic <- chlamydia$Specimen == "Urine" &
  chlamydia$Symptoms_Status == "Symptomatic"
lines(x = chlamydia$gender.numb[Urine.Symptomatic], y = chlamydia$Sensitivity[Urine.Symptomatic],
      type = "o", lty = "solid", col = "green")

Urine.Asymptomatic <- chlamydia$Specimen == "Urine" &
  chlamydia$Symptoms_Status == "Asymptomatic"
lines(x = chlamydia$gender.numb[Urine.Asymptomatic], y = chlamydia$Sensitivity[Urine.Asymptomatic],
      type = "o", lty = "solid", col = "red")

legend("bottom", c("Swab.Symptomatic", "Swab.Asymptomatic", "Urine.Symptomatic", "Urine.Asymptomatic"),
       lty=c(1,1,1,1), lwd=c(2.5,2.5,2.5,2.5),col=c('black','blue','green','red'))

# Logistics Regression Model with Gender, Specimen, Symptoms_Status
# and interaction term Gender*Specimen
mod.fit.inter <- glm(formula=Sensitivity ~ Gender + Specimen + Symptoms_Status + Gender:Specimen, 
               family = binomial(link = logit), data = chlamydia, weights=Sample_size)
summary(mod.fit.inter)

library(package = car)
Anova(mod.fit.inter)

# Confidence Interval
g = 8
alpha <- 0.05/g
predict.data = chlamydia[, c('Gender','Specimen','Symptoms_Status')]
linear.pred <- predict( object = mod.fit.inter, type = "link", newdata = predict.data, se = TRUE )
CI.lin.pred.lower <- linear.pred$fit + qnorm(p = alpha/2)*linear.pred$se
CI.lin.pred.upper <- linear.pred$fit + qnorm(p = 1-alpha/2)*linear.pred$se
CI.pi.upper <- exp(CI.lin.pred.upper)/(1+exp(CI.lin.pred.upper))
CI.pi.lower <- exp(CI.lin.pred.lower)/(1+exp(CI.lin.pred.lower))
ci.pi = data.frame(predict.data, lower = CI.pi.lower, upper = CI.pi.upper)
