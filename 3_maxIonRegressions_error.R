library(readxl)
library(here)
library(bayclumpr)

Table1 <- read_excel(here("data", "Test 1 - All benthics, epifaunal and infaunal.xlsx"))
Table1$Material <- rep(1, nrow(Table1))
colnames(Table1)[c(5, 6, 2, 3, 8)] <- c("D47", "D47error", "Temperature", "TempError", "Ion")

# Check basic regression model
lm(Table1$D47 ~ Table1$Temperature)

# Fit Bayesian regression
ionmodel <- cal.ion.bayesian(calibrationData = Table1, 
                             IonError = Table1$`Ion error`[1])
parameters <- data.frame(rstan::summary(ionmodel[[1]])$summary)
write.csv(parameters, here("results", "parameters_ion.csv"))

# Perform reconstructions
ionRec <- read.csv(here("data", "reconstructions_ion.csv"))
PredsBay <- rec.bayesian(calModel = ionmodel[[1]],
                             recData = ionRec,
                             iter = 1000,
                             postcalsamples = 100, MC = FALSE)

write.csv(PredsBay, here("results", "reconstructed_ion.csv"))

