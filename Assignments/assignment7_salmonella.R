## Question 19 - Chapter 5
library(aod)
data = data.frame(salmonella)

# Plot
library(ggplot2)
data$logdose = log10(data$dose+1)
qplot(dose, y, data=data)
qplot(logdose, y, data=data)

## Poisson Regression
mod.fit1 = glm(formula = y ~ dose, family = poisson(link=log), data = data)
summary(mod.fit1)

mod.fit2 = glm(formula = y ~ logdose, family = poisson(link=log), data = data)
summary(mod.fit2)

data$catdose = factor(data$dose)
mod.fit3 = glm(formula = y ~ catdose, family = poisson(link=log), data = data)
summary(mod.fit3)

# Deviance/DF statistics:
round(mod.fit1$deviance/mod.fit1$df.residual, 3)
round(1+3*sqrt(2/(mod.fit1$df.residual)), 3)

round(mod.fit2$deviance/mod.fit2$df.residual, 3)
round(1+3*sqrt(2/(mod.fit2$df.residual)), 3)

round(mod.fit3$deviance/mod.fit3$df.residual, 3)
round(1+3*sqrt(2/(mod.fit3$df.residual)), 3)

## Plot residual
fitted_means1 <- mod.fit1$fitted.values
s.res1 = rstandard(mod.fit1, type="pearson")
qplot(fitted_means1, s.res1, main="Model with dose as explanatory variable", 
      xlab="Fitted Values" , ylab="Standardized Residual")

fitted_means2 <- mod.fit2$fitted.values
s.res2 = rstandard(mod.fit2, type="pearson")
qplot(fitted_means2, s.res2, main="Model with log-dose as explanatory variable", 
      xlab="Fitted Values" , ylab="Standardized Residual")

fitted_means3 <- mod.fit3$fitted.values
s.res3 = rstandard(mod.fit3, type="pearson")
qplot(fitted_means3, s.res3, main="Model with categorical dose as explanatory variable", 
      xlab="Fitted Values" , ylab="Standardized Residual")

## QuasiPoisson vs Negative Binomial
res.sq <- residuals(object = mod.fit3, type = "response")^2
set1 <- data.frame(res.sq, mu.hat = mod.fit3$fitted.values)
fit.lin <- lm(formula = res.sq ~ mu.hat, data = set1)
fit.quad <- lm(formula = res.sq ~ mu.hat + I(mu.hat ^2), data = set1)
summary(fit.lin)
summary(fit.quad)
anova(fit.quad)


plot(x = set1$mu.hat, y = set1$res.sq, xlab = "Predicted count", ylab = "Squared Residual")
curve(expr = predict(object = fit.lin , newdata = data.frame(mu.hat = x), type = "response"),
        col = "blue", add=TRUE, lty = "solid")
curve(expr = predict(object = fit.quad, newdata = data.frame(mu.hat = x), type = "response"),
        col = "red", add =TRUE, lty = "dashed")
legend(x = 50, y = 1000, legend = c("Linear", "Quadratic"),
       col= c("red", "blue") , lty = c("solid", "dashed") , bty = "n")

# QuasiPoisson model:
Mqp <- glm(formula = y ~ catdose, family = quasipoisson(link = "log"), data = data)
summary(Mqp)
round(Mqp$deviance/Mqp$df.residual, 3)
round(1+3*sqrt(2/(Mqp$df.residual)), 3)

fitted_means_Mqp <- Mqp$fitted.values
s.res_Mqp = rstandard(Mqp, type="pearson")
qplot(fitted_means_Mqp, s.res_Mqp, main="Quasi-Poisson Model", 
      xlab="Fitted Values" , ylab="Standardized Residual")