## gonorrhea - Specificity
mod.fit <- glm(formula=Specificity ~ Gender + Specimen + Symptoms_Status, 
               family = binomial(link = logit), data = gonorrhea, weights=Sample_size)
summary(mod.fit)

# Plot
gonorrhea$gender.numb <- ifelse( test = gonorrhea$Gender == "Female",
                                 yes = 0, no = 1)
plot(x = gonorrhea$gender.numb, y = gonorrhea$Specificity, ylim=c(0.9, 1.0), xlim=c(0, 1),
     xlab = "Gender", ylab = "Specificity", type = "n", xaxt = "n")
axis( side = 1, at = c(0,1), labels = c("Female", "Male"))
Swab.Symptomatic <- gonorrhea$Specimen == "Swab" &
  gonorrhea$Symptoms_Status == "Symptomatic"
lines(x = gonorrhea$gender.numb[Swab.Symptomatic], y = gonorrhea$Specificity[Swab.Symptomatic],
      type = "o", lty = "solid", col = "black")

Swab.Asymptomatic <- gonorrhea$Specimen == "Swab" &
  gonorrhea$Symptoms_Status == "Asymptomatic"
lines(x = gonorrhea$gender.numb[Swab.Asymptomatic], y = gonorrhea$Specificity[Swab.Asymptomatic],
      type = "o", lty = "solid", col = "blue")

Urine.Symptomatic <- gonorrhea$Specimen == "Urine" &
  gonorrhea$Symptoms_Status == "Symptomatic"
lines(x = gonorrhea$gender.numb[Urine.Symptomatic], y = gonorrhea$Specificity[Urine.Symptomatic],
      type = "o", lty = "solid", col = "green")

Urine.Asymptomatic <- gonorrhea$Specimen == "Urine" &
  gonorrhea$Symptoms_Status == "Asymptomatic"
lines(x = gonorrhea$gender.numb[Urine.Asymptomatic], y = gonorrhea$Specificity[Urine.Asymptomatic],
      type = "o", lty = "solid", col = "red")

legend("bottom", c("Swab.Symptomatic", "Swab.Asymptomatic", "Urine.Symptomatic", "Urine.Asymptomatic"),
       lty=c(1,1,1,1), lwd=c(2.5,2.5,2.5,2.5),col=c('black','blue','green','red'))

# Logistics Regression Model with Gender, Specimen, Symptoms_Status
# and interaction term Gender*Specimen
mod.fit.inter <- glm(formula=Specificity ~ Gender + Specimen + Symptoms_Status + Specimen:Symptoms_Status, 
                family = binomial(link = logit), data = gonorrhea, weights=Sample_size)
summary(mod.fit.inter)

library(package = car)
Anova(mod.fit.inter)