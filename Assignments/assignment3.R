## Sensitivity and Specificity
aptima <- read.table( file = "D:/Training/Source_Code/R/Generalized-Linear-Model/aptima_combo.csv",
                         header = TRUE, sep = "," )
aptima$Positive = aptima$True_positive + aptima$False_negative
aptima$Sensitivity = aptima$True_positive / aptima$Positive

aptima$Negative = aptima$True_negative + aptima$False_positive
aptima$Specificity = aptima$True_negative / aptima$Negative

chlamydia <- subset(aptima, aptima$Disease=="Chlamydia") # Chlamydia
gonorrhea <- subset(aptima, aptima$Disease=="Gonorrhea") # Gonorrhea