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


#With the ion (no error on ion)
ionmodel <- cal.ion.bayesian(calibrationData = mt, useIonError = FALSE)
ionmodel

#Regressions without the ion
regmodel <- cal.bayesian(calibrationData = mt)
regmodel

# Perform reconstructions using the model that ignores ion error
rec <- read_excel(here("data", "Test 1 - All benthics, epifaunal and infaunal.xlsx"), sheet = 2)

colnames(rec)[3] <- "D47error"
colnames(rec)[4] <- "Ion"
colnames(rec)[5] <- "IonError"


PredsBay <- rec.ion.bayesian(calModel = ionmodel[[1]],
                             recData = rec,
                             iter = 1000,
                             postcalsamples = 100, MC = FALSE)

write.csv(PredsBay, here("data", "reconstructions_ion.csv"))



