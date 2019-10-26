# Read Data:
tu <- read.csv(file = "tu.csv")
df <- subset(tu, select = c("age", "race", "sex", "ms", "hisp", "educ", "pob",
                            "adjinc", "esr", "histatus", 
                            "stater",
                            "smokstat",
                            "inddea", "follow"))
df <- df[complete.cases(df),]

## Explore Data
summary(df$age)
summary(df$smokstat)
summary(df$follow)
hist(df$follow)
summary(df$inddea)
hist(df$DWHFDAYS)

## Data Pre-Processing
df$age_group = cut(df$age, breaks=c(10, 20, 30, 40, 50, 60, 70, 80, 90))
df$inddea = factor(df$inddea)
df$race = factor(df$race)
df$sex = factor(df$sex)
df$ms = factor(df$ms)
df$hisp = factor(df$hisp)
df$educ = factor(df$educ)
df$pob = factor(df$pob)
df$esr = factor(df$esr)
df$histatus = factor(df$histatus)
df$stater = factor(df$stater)
df$eversmoke <- ifelse((df$smokstat==1), yes=0, no=1)
head(df)

# Summary Statistics:
summary(df$eversmoke)
summary(df$age)
library(reshape2)
smoke.agegroup <- dcast(df, age_group ~ ., function(eversmoke) mean(eversmoke))
barplot(100*smoke.agegroup$., main="Percentage of people smoking", xlab="Age", ylab="%",
        names.arg=smoke.agegroup$age_group)

## Contingency Table Anlysis
df <- df[df$age > 50 & df$age <= 70,]
c.table <- xtabs(formula = ~ eversmoke + inddea, data = df)
c.table
OR.hat <- 1.0*c.table[1,1]*c.table[2,2]/(c.table[2,1]*c.table[1,2])
paste("sample OR:", round(OR.hat,4))
var.log.or <- 1/c.table[1,1] + 1/c.table[1,2] + 1/c.table[2 ,1] + 1/c.table [2,2]
alpha = 0.05
OR.CI <- exp(log(OR.hat) + qnorm(p=c(alpha/2, 1-alpha/2))*sqrt(var.log.or))
paste("CI OR:", round (OR.CI , 4))

### Logistics Regression Model
mod.fit <- glm(formula = inddea ~ esr + age + as.factor(df$smokstat) + sex + 
                 ms + hisp + adjinc + .*as.factor(df$smokstat), family = binomial(link = "logit"), data = df)
summary(mod.fit)

# Step-wise Variable Selection
empty.mod = glm(formula = inddea ~ 1, family = binomial(link="logit"), data = df)
# with binary smoking status (Yes/No)
full.mod = glm(formula = inddea ~ age + race + sex + ms + hisp + educ + pob +
                 esr + adjinc + histatus + 
                 eversmoke, family = binomial(link="logit"), data = df)
step.sel <- step(object = empty.mod, scope = list(upper = full.mod), 
                 k = log(nrow(df)), trace = TRUE)
summary(step.sel)

# with ordinal smoking status (1 < 4 < 3 < 2)
df$smokstat_order <- ifelse((df$smokstat==1), yes=1, 
                            no=ifelse((df$smokstat==4), yes=2, 
                            no=ifelse((df$smokstat==3), yes=3, no=2)))
full.mod = glm(formula = inddea ~ age + race + sex + ms + hisp + educ + pob +
                 esr + adjinc + histatus + 
                 smokstat_order, family = binomial(link="logit"), data = df)
step.sel <- step(object = empty.mod, scope = list(upper = full.mod), 
                 k = log(nrow(df)), trace = TRUE)
summary(step.sel)

# with nominal smoking status (1, 2, 3, 4)
full.mod = glm(formula = inddea ~ age + race + sex + ms + hisp + educ + pob +
                 esr + adjinc + histatus + 
                 as.factor(df$smokstat), family = binomial(link="logit"), data = df)
step.sel <- step(object = empty.mod, scope = list(upper = full.mod), 
                 k = log(nrow(df)), trace = TRUE)
summary(step.sel)

# Model Checking
s.res1 = rstandard(step.sel, type="pearson")
(length(s.res1[s.res1>2])+length(s.res1[s.res1<-2]))
length(s.res1)

source("AllGOFTests.R")
HL <- HLTest(obj=step.sel, g = 10)
HL
HL <- HLTest(obj=step.sel, g = 100)
HL

paste("Deviance/df statistics: ", round(step.sel$deviance/step.sel$df.residual, 3))
round(1+2*sqrt(2/(step.sel$df.residual)), 3)

# Inference
OR.hat <- exp(summary(step.sel)$coefficients[9][1])
var.log.or <- summary(step.sel)$coefficients[26][1]
OR.CI <- exp(log(OR.hat) + qnorm(p=c(alpha/2, 1-alpha/2))*(var.log.or))
paste("sample OR:", round(OR.hat,4))
paste("CI OR:", round (OR.CI , 4))

OR.hat <- exp(summary(step.sel)$coefficients[8][1])
var.log.or <- summary(step.sel)$coefficients[25][1]
OR.CI <- exp(log(OR.hat) + qnorm(p=c(alpha/2, 1-alpha/2))*(var.log.or))
paste("sample OR:", round(OR.hat,4))
paste("CI OR:", round (OR.CI , 4))

OR.hat <- exp(summary(step.sel)$coefficients[7][1])
var.log.or <- summary(step.sel)$coefficients[24][1]
OR.CI <- exp(log(OR.hat) + qnorm(p=c(alpha/2, 1-alpha/2))*(var.log.or))
paste("sample OR:", round(OR.hat,4))
paste("CI OR:", round (OR.CI , 4))

## Mixed Model
library(lme4)
k_list <- c(1,2,5,10,20,30)
for (k in k_list) {
  mod.glmm <- glmer(formula = inddea ~ age + esr + as.factor(smokstat) + sex + ms + hisp + 
                      adjinc + (1|stater), nAGQ = k, family = binomial(link = "logit"), data = df)
  print(paste("nAGQ: ", k))
  print(summary(mod.glmm)$varcor)
}
mod.glmm <- glmer(formula = inddea ~ age + esr + as.factor(smokstat) + sex + ms + hisp + 
                    adjinc + (1|stater), nAGQ = 5, family = binomial(link = "logit"), data = df)
summary(mod.glmm)

# http://www.ssc.wisc.edu/sscc/pubs/MM/MM_DiagInfer.html

plot(mod.glmm)
library(ggplot2)
ggplot(data.frame(eta=predict(mod.glmm,type="link"),pearson=s.res1),
       aes(x=eta,y=pearson)) +
  geom_point() +
  theme_bw()
s.res1 = resid(mod.glmm, type="pearson")
(length(s.res1[s.res1>3])+length(s.res1[s.res1<-2]))
length(s.res1)

# Random Effect
ranef(mod.glmm)$stater
LRstat.vc <- deviance(step.sel) - deviance(mod.glmm)
(1 - pchisq(LRstat.vc, df = 1))/2

# Fixed Effect
fixef(mod.glmm)
lrt <- drop1(mod.glmm, test = "Chisq")
lrt

## Survival Analysis
# http://rstudio-pubs-static.s3.amazonaws.com/5896_8f0fed2ccbbd42489276e554a05af87e.html
library(survival)
df$SurvObj <- Surv(df$follow, df$inddea==1)

# Kaplan-Meier estimator
km.by.smokstat <- survfit(SurvObj ~ as.factor(smokstat), data = df, conf.type = "log-log")
km.by.smokstat
plot(km.by.smokstat, conf.int=TRUE , ylim=c(0.9, 1),
     col=c('black','red', 'blue', 'green'), main="Survival Rate", xlab='Time (days)')
legend(1,0.95, legend=c("Never smoke", "Everyday smoker", "Someday smoker", "Former smoker"), 
       lty=c(1,1,1,1) ,col=c('black','red', 'blue', 'green'))

# Log-Rank Test
survdiff(SurvObj ~ as.factor(smokstat),data=df)

# Cox Regression
res.cox1 <- coxph(SurvObj ~ age + sex + esr + as.factor(smokstat), data = df)
res.cox1

## Check for violation of proportional hazard (constant HR over time)
(res.zph1 <- cox.zph(res.cox1))
plot(res.zph1)





## Plot residual
model = step.sel
s.res1 = rstandard(model, type="pearson")
fitted_values <- model$fitted.values
linear_predictors <- model$linear.predictors

plot(df$ms, s.res1, main="Selected Model", 
     xlab="X" , ylab="Standardized Residual")

plot(fitted_values, s.res1, main="Standardized Residual vs pi.hat", 
     xlab="pi.hat" , ylab="Standardized Residual")

plot(linear_predictors, s.res1, main="Standardized Residual vs Linear Predictor", 
     xlab="Linear Predictor" , ylab="Standardized Residual")
