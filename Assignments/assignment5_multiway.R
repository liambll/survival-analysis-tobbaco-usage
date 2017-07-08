## Data
survey_data <- read.table( file = "D:/Training/Source_Code/R/Generalized-Linear-Model/PolIdeolData.csv",
                         header = TRUE, sep = "," )

## Marginal Odd ratio of Democratics vs Republican between males and females
marginal.table <- xtabs(formula = count ~ gender + party, data = survey_data)
marginal.odd.ratio <- marginal.table[1,1]*marginal.table[2,2]/(marginal.table[1,2]*marginal.table[2,1])
marginal.table
cat("Marginal Odds Ratio:", marginal.odd.ratio)

## Conditional Odd ratio of Democratics vs Republican between males and females
c.table <- xtabs(formula = count ~ gender + party + ideol, data = survey_data)
ideol <- c("VL", "SL", "M", "SC", "VC")
odd.ratios <- c()
for (k in c(1,2,3,4,5)) {
  conditional.odd.ratio = c.table[1,1,k]*c.table[2,2,k]/(c.table[1,2,k]*c.table[2,1,k])
  odd.ratios <- c(odd.ratios, conditional.odd.ratio)
}
result <- data.frame(ideol, odd.ratios )
result

# Cohran-Mantel-Haenszel test
mantelhaen.test(c.table)

# Breslow-Day test
source('breslowday.test.R')
breslowday.test(c.table)