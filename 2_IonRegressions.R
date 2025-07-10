library(bayclumpr)
library(readxl)
library(here)

# Read in the dataset
mt <- read_excel(here("data", "Meckler and Taylor dataset for correction carbonate ion effect.xlsx"))

# Remove last row (empty)?
mt <- mt[-nrow(mt),]

# Rename columns to match expectations in bayclumpr
colnames(mt)[c(10, 12, 13, 14, 33)] <- c("D47", "D47error", "Temperature", "TempError", "Ion")

# Include normally distribuited errors for 
mt$IonError <- rnorm(nrow(mt), 2)

#Fit the regression model that account for the Ion (column) and it's error
ionmodel <- cal.ion.bayesian(calibrationData = mt)

# Perform reconstructions in a sample dataset based on the calibration model
recData <- data.frame(Sample = paste("Sample", 1:9),
                      D47 = rep(c(0.6, 0.7, 0.8), 3),
                      D47error = c(rep(0.005,3), rep(0.01,3), rep(0.02,3)),
                      Ion = rep(c(0.6, 0.7, 0.8), 3),
                      IonError = c(rep(0.005,3), rep(0.01,3), rep(0.02,3)),
                      N = rep(2, 9),
                      Material = rep(1, 9))

PredsBay <- rec.ion.bayesian(calModel = ionmodel[[1]],
                             recData = recData,
                             iter = 1000,
                             postcalsamples = 100, MC = FALSE)
