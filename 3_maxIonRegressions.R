library(bayclumpr)
library(readxl)
library(here)

# Read in the dataset
mt <- read_excel(here("data", "Test 1 - All benthics, epifaunal and infaunal.xlsx"))


# Need to rename column names
colnames(mt)[2] <- "Temperature"
colnames(mt)[3] <- "TempError"
colnames(mt)[6] <- "D47error"
colnames(mt)[9] <- "IonError"
mt$Material <- 1

#Check with a simple regression
lm(mt$D47 ~ mt$Temperature)
plot(mt$D47 ~ mt$Temperature)


#With the ion (no error on ion)
ionmodel <- cal.ion.bayesian(calibrationData = mt, useIonError = FALSE)
ionmodel

# mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
# alpha   0.15    0.00 0.05   0.05   0.12   0.15   0.18   0.26  2694    1
# beta    0.04    0.00 0.00   0.03   0.04   0.04   0.04   0.05  2726    1
# gamma   0.00    0.00 0.00   0.00   0.00   0.00   0.00   0.00  2707    1
# sigma   0.01    0.00 0.00   0.01   0.01   0.01   0.01   0.02  3351    1
# lp__  119.43    0.15 4.68 109.52 116.58 119.73 122.53 128.11   967    1


#Regressions without the ion
regmodel <- cal.bayesian(calibrationData = mt)
regmodel

# Model 1
# mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
# alpha         0.20    0.00 0.03   0.14   0.18   0.20   0.22   0.26  4345    1
# beta          0.04    0.00 0.00   0.03   0.03   0.04   0.04   0.04  4307    1
# sigma         0.01    0.00 0.00   0.01   0.01   0.01   0.01   0.02  4119    1
# Model 2
# mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
# alpha         0.20    0.00 0.03   0.13   0.18   0.19   0.22   0.25  1233    1
# beta          0.04    0.00 0.00   0.03   0.03   0.04   0.04   0.04  1234    1
# sigma         0.01    0.00 0.00   0.01   0.01   0.01   0.01   0.02  1188    1
# Model 3
# mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
# alpha[1]      0.20    0.00 0.03   0.14   0.18   0.20   0.22   0.26   858    1
# beta[1]       0.04    0.00 0.00   0.03   0.03   0.04   0.04   0.04   871    1
# sigma         0.01    0.00 0.00   0.01   0.01   0.01   0.01   0.02   833    1

