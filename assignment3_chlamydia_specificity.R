## Chlamydia - Specificity
mod.fit <- glm(formula=Specificity ~ Gender + Specimen + Symptoms_Status, 
               family = binomial(link = logit), data = chlamydia, weights=Sample_size)
summary(mod.fit)

# Plot
chlamydia$gender.numb <- ifelse( test = chlamydia$Gender == "Female",
                                 yes = 0, no = 1)
plot(x = chlamydia$gender.numb, y = chlamydia$Specificity, ylim=c(0.9, 1.0), xlim=c(0, 1),
     xlab = "Gender", ylab = "Specificity", type = "n", xaxt = "n")
axis( side = 1, at = c(0,1), labels = c("Female", "Male"))
Swab.Symptomatic <- chlamydia$Specimen == "Swab" &
  chlamydia$Symptoms_Status == "Symptomatic"
lines(x = chlamydia$gender.numb[Swab.Symptomatic], y = chlamydia$Specificity[Swab.Symptomatic],
      type = "o", lty = "solid", col = "black")

Swab.Asymptomatic <- chlamydia$Specimen == "Swab" &
  chlamydia$Symptoms_Status == "Asymptomatic"
lines(x = chlamydia$gender.numb[Swab.Asymptomatic], y = chlamydia$Specificity[Swab.Asymptomatic],
      type = "o", lty = "solid", col = "blue")

Urine.Symptomatic <- chlamydia$Specimen == "Urine" &
  chlamydia$Symptoms_Status == "Symptomatic"
lines(x = chlamydia$gender.numb[Urine.Symptomatic], y = chlamydia$Specificity[Urine.Symptomatic],
      type = "o", lty = "solid", col = "green")

Urine.Asymptomatic <- chlamydia$Specimen == "Urine" &
  chlamydia$Symptoms_Status == "Asymptomatic"
lines(x = chlamydia$gender.numb[Urine.Asymptomatic], y = chlamydia$Specificity[Urine.Asymptomatic],
      type = "o", lty = "solid", col = "red")

legend("bottom", c("Swab.Symptomatic", "Swab.Asymptomatic", "Urine.Symptomatic", "Urine.Asymptomatic"),
       lty=c(1,1,1,1), lwd=c(2.5,2.5,2.5,2.5),col=c('black','blue','green','red'))

# Logistics Regression Model with Gender, Specimen, Symptoms_Status
# and interaction term Gender*Specimen
mod.fit.inter <- glm(formula=Specificity ~ Gender + Specimen + Symptoms_Status + Gender:Specimen:Symptoms_Status, 
                family = binomial(link = logit), data = chlamydia, weights=Sample_size)
summary(mod.fit.inter)

library(package = car)
Anova(mod.fit.inter)