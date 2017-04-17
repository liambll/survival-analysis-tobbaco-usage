## Question 30 - Chapter 4
c.table <- array(data = c(8, 69, 2, 25, 20, 7, 100,
                          39, 139, 39, 57, 48, 20, 177,
                          48, 131, 50, 115, 16, 19, 190), dim=c(7,3), 
                 dimnames = list(Guild=c("Air-Arth", "Leaf_Arth", "Wood-Arth", "Ground-Arth", "Ftui", "Seeds", "Nectar"), 
                                 Degradation=c("High", "Moderate", "Low")))
c.table

df = as.data.frame(as.table(c.table))
df$Degradation =factor(df$Degradation,levels = c('Low','Moderate','High'),order=FALSE)
model.fit <- glm(formula = Freq ~ Degradation*Guild, family = poisson(link="log"), data = df)
summary(model.fit)

# Confidence Interval for odds
# High vs Low
library(mcprofile)
K = model.matrix(model.fit)
for (i in 1:7) K[i,] = K[i,]-K[i+14,]
linear_comb = mcprofile(glm_fit1,CM=K)
conf = confint(linear_comb,adjust='none')
high_low = cbind.data.frame(compare = 'High vs Low', Guild = dimnames(c.table)$Guild, est= exp(conf$est)[1:7,],exp(conf$confint)[1:7,])
high_low

# Moderate vs Low
K = model.matrix(model.fit)
for (i in 8:14) K[i,] = K[i,]-K[i+7,]
linear_comb = mcprofile(glm_fit1,CM=K)
conf = confint(linear_comb,adjust='none')
moderate_low = cbind.data.frame(compare = 'Moderate vs Low', Guild = dimnames(c.table)$Guild, est= exp(conf$est)[8:14,],exp(conf$confint)[8:14,])
moderate_low

# High vs Moderate
K = model.matrix(model.fit)
for (i in 1:7) K[i,] = K[i,]-K[i+7,]
linear_comb = mcprofile(glm_fit1,CM=K)
conf = confint(linear_comb,adjust='none')
high_moderate = cbind.data.frame(compare = 'High vs Moderate', Guild  = data[1:7,3],est= exp(conf$est)[1:7,],exp(conf$confint)[1:7,])
high_moderate
