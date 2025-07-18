library(bayclumpr)
library(readxl)
library(here)

# Read in the dataset
mt <- read_excel(here("data", "Test 1 - All benthics, epifaunal and infaunal.xlsx"))


# Need to rename column names
colnames(mt)[2] <- "Temperature"
colnames(mt)[3] <- "TempError"
colnames(mt)[5] <- "D47error"
colnames(mt)[6] <- "TempError"
colnames(mt)[9] <- "IonError"

ionmodel <- cal.ion.bayesian(calibrationData = mt)
ionmodel
