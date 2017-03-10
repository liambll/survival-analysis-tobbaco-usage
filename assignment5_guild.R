## Question 30 - Chapter 4
c.table <- array(data = c(8, 69, 2, 25, 20, 7, 100,
                          39, 139, 39, 57, 48, 20, 177,
                          48, 131, 50, 115, 16, 19, 190), dim=c(7,3), 
                 dimnames = list(Guild=c("Air-Arth", "Leaf_Arth", "Wood-Arth", "Ground-Arth", "Ftui", "Seeds", "Nectar"), 
                                 Degradation=c("High", "Moderate", "Low")))
c.table

df = as.data.frame(as.table(c.table))
model.fit <- glm(formula = Freq ~ Guild*Degradation, family = poisson(link="log"), data = df)
summary(model.fit)
