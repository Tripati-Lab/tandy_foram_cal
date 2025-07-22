library(bayclumpr)
library(readxl)
library(here)

# Read in the dataset
mt <- read_excel(here("data", "Test 1 - All benthics, epifaunal and infaunal.xlsx"))


# Need to rename column names
colnames(mt)[2] <- "Temperature"
colnames(mt)[3] <- "TempError"
colnames(mt)[5] <- "D47error"
colnames(mt)[9] <- "IonError"
mt$Material <- 1

#Check with a simple regression
lm(mt$D47 ~ mt$Temperature)


#With the ion (no error on ion)
ionmodel <- cal.ion.bayesian(calibrationData = mt, useIonError = FALSE)
ionmodel

# mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
# alpha   0.05    0.00 0.03   0.00   0.03   0.04   0.06   0.10  3006    1
# beta    0.00    0.00 0.00  -0.01   0.00   0.00   0.00   0.00  3040    1
# gamma   0.00    0.00 0.00   0.00   0.00   0.00   0.00   0.00  2538    1
# sigma   0.01    0.00 0.00   0.00   0.01   0.01   0.01   0.01  3407    1
# lp__  146.94    0.13 4.74 136.64 143.85 147.25 150.33 155.38  1287    1


#Regressions without the ion
regmodel <- cal.bayesian(calibrationData = mt)
regmodel

