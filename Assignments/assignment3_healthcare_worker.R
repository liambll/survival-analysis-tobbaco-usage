## Health Worker
health.table <- read.table( file = "D:/Training/Source_Code/R/Generalized-Linear-Model/healthcare_worker.csv",
                      header = TRUE, sep = "," )
health.table$No_Hepatitis = health.table$Size - health.table$Hepatitis

c.table <- array(data = c(health.table$Hepatitis, health.table$No_Hepatitis), dim=c(5,2), 
                 dimnames = list(Occup.group=c("Exposure prone", "Fluid contact",
                "Lab staff", "Patient contact", "No patient contact"), 
                Hepatitis=c("Hepatiti", "No_Hepatitis")))

library(package = vcd)
assocstats(x = c.table)

# Logistics Regression
mod.fit <- glm(formula=Hepatitis/Size ~ Occup.group, 
               family = binomial(link = logit), data = health.table, weights=Size)
summary(mod.fit)