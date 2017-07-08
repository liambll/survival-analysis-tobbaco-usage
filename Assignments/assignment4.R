## Data
survey_data <- read.table( file = "D:/Training/Source_Code/R/Generalized-Linear-Model/PolIdeolData.csv",
                         header = TRUE, sep = "," )
survey_data$ideol = factor(survey_data$ideol, levels=c("VL", "SL", "M", "SC", "VC"))
levels(survey_data$ideol)

c.table <- xtabs(formula = count ~ party + ideol + gender , data = survey_data)
ftable(x = c.table, row.vars = c("gender", "party"), col.vars = "ideol")

## Multinomial Model
library(package = nnet)
mod.fit= multinom(formula = ideol ~ party + gender + party:gender, data=survey_data, weights=count)
summary(mod.fit)

library(package = car)
Anova(mod.fit)

# Estimate odds
library(plyr)
data_combination <- ddply(survey_data, c("gender", "party"), summarize, totalcount = sum(count))
pi.hat <- predict(object = mod.fit, newdata = data_combination, type = "probs")
prediction = data.frame(data_combination, pi.hat)
estimated_probs = prediction[c("gender", "party", "VL", "SL", "M", "SC", "VC")]

# Estimated counts
library(reshape2)
prediction = melt(prediction, id=c("gender", "party", "totalcount"), variable.name="ideol", value.name="prob")
prediction$count = round(prediction$prob * prediction$totalcount)
estimated_counts = prediction[c("gender", "party", "ideol", "count")]
counts.table <- xtabs(formula = count ~ party + ideol + gender , data = estimated_counts)
ftable(x = counts.table, row.vars = c("gender", "party"), col.vars = "ideol")

# Odd Ratio
changes = exp(coefficients(mod.fit)[,2:3])

## Proportional Odds Model
library(package = MASS)
mod.fit2 = polr(formula = ideol ~ party + gender + party:gender, data=survey_data, weights=count, method="logistic")
summary(mod.fit2)
Anova(mod.fit2)

# Estimate odds
pi.hat2 <- predict(object = mod.fit2, newdata = data_combination, type = "probs")
prediction2 = data.frame(data_combination, pi.hat2)
estimated_probs2 = prediction2[c("gender", "party", "VL", "SL", "M", "SC", "VC")]

# Estimated counts
prediction2 = melt(prediction2, id=c("gender", "party", "totalcount"), variable.name="ideol", value.name="prob")
prediction2$count = round(prediction2$prob * prediction2$totalcount)
estimated_counts2 = prediction2[c("gender", "party", "ideol", "count")]
counts.table2 <- xtabs(formula = count ~ party + ideol + gender , data = estimated_counts2)
ftable(x = counts.table2, row.vars = c("gender", "party"), col.vars = "ideol")

# Odd Ratio
changes = exp(coefficients(mod.fit))
coefficients(mod.fit2)
