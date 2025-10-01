library(readxl)
library(here)
library(bayclumpr)

Table1 <- read_excel(here("data", "Test 1 - All benthics, epifaunal and infaunal.xlsx"))
Table1$Material <- rep(1, nrow(Table1))

colnames(Table1)[c(6, 5, 2, 3, 8)] <- c("D47", "D47error", "Temperature", "TempError", "Ion")

ionmodel <- cal.ion.bayesian(calibrationData = Table1, 
                             IonError = Table1$`Ion error`[1])

model <- cal.bayesian(calibrationData = Table1)


Table1$D47
Table1$Temperature
Table1$D47error
Table1$TempError

plot(Table1$Temperature, Table1$D47)



