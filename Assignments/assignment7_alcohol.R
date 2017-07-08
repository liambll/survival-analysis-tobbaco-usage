## Question 19 - Chapter 5
data <- read.csv(file = "DeHartSimplified.csv")
head(data, 10)
data<-na.omit(data)
data$dayweek = factor(data$dayweek)

## Mixed Effect Model
library(lme4)
k_list <- c(1,2,5,10,15,20)
for (k in k_list) {
  mod.fit = glmer(formula = numall ~ nrel + prel + negevent + posevent + gender + rosn + age + desired + state + dayweek + (1|id),
                   nAGQ = k, data = data, family = poisson( link = "log"))
  print(paste("nAGQ: ", k))
  print(summary(mod.fit)$varcor)
  print(summary(mod.fit)$AIC)
}

fixef(mod.fit)
ranef(mod.fit)

# Variance component
mod.glm <- glm(formula = numall ~ nrel + prel + negevent + posevent + gender + rosn + age + desired + state + dayweek,
               data = data, family = poisson(link = "log"))
LRstat.vc <- deviance(mod.glm) - deviance(mod.fit)
(1 - pchisq(LRstat.vc, df = 1))/2

# Fixed Effect
lrt <- drop1(mod.fit, test = "Chisq")
lrt

## Marginal Model
data <- data[order(data$id),]

library(geepack)
mod.gee.full <- geeglm(formula = numall ~ nrel + prel + negevent + posevent + gender + rosn + age + desired + state + dayweek,
                    id = id, data = data, scale.fix = TRUE, family = poisson(link = "log"),
                    corstr = "exchangeable")
summary(mod.gee.full)
anova(mod.gee.full)

mod.gee.full2 <- geeglm(formula = numall ~ dayweek + state + desired + age + rosn + gender + posevent + negevent + prel + nrel,
                    id = id, data = data, scale.fix = TRUE, family = poisson(link = "log"),
                    corstr = "exchangeable")
summary(mod.gee.full2)
anova(mod.gee.full2)

mod.gee.partial <- geeglm(formula = numall ~ nrel + prel + negevent + posevent + gender + rosn + age + desired + state,
                    id = id, data = data, scale.fix = TRUE, family = poisson(link = "log"),
                    corstr = "exchangeable")
anova(mod.gee.full, mod.gee.partial)



