library(readxl)
library(here)
library(deming)

############################################################
#Script focused on comparing groups based on Figs 3 and 4###
############################################################

# Read in datasets
d <- read_excel(here("data", "forregressiontest_meinicke_piaseckiETHchange_latest.xlsx"))
s4 <- read.csv(here("data", "Table S4.csv"))

## Rename columns in S4
colnames(s4)[6] <- "D47ICDES"
colnames(s4)[8] <- "calcTiso"
colnames(s4)[7] <- "SE47"
colnames(s4)[9] <- "Terror"

## Categories from Figure 4
ben <- subset(d, Habitat=="Benthic, epifaunal"|Habitat=="Benthic, infaunal"|Habitat=="Benthic"|Habitat=="Benthic, epifaunal to shallow infaunal")
plan <- subset(d, Habitat!="Benthic, epifaunal"&Habitat!="Benthic, infaunal"&Habitat!="Benthic"&Habitat!="Benthic, epifaunal to shallow infaunal"&`Sample Name`!="CH75-18-16 G MENARDII") #plan
mixplan <- subset(d, Habitat=="Mixed-layer")
mixben <- subset(d, Species=="Mixed benthic species")
epiben <- subset(d, Habitat!="Benthic, epifaunal")
infraben <- subset(d, Habitat=="Benthic, infaunal")

## Additional categories from Figure 3
UCLA <- subset(d, Dataset=="UCLA"&`Sample Name`!="CH75-18-16 G MENARDII")
synthetic <- s4
allforams <- rbind(UCLA, ben, plan, mixplan, mixben, epiben, infraben)

## Create a list with all the datasets
data_list <- list(UCLA = UCLA, 
                  ben = ben, 
                  plan = plan,
                  mixplan = mixplan, 
                  mixben = mixben,
                  epiben = epiben,
                  infraben = infraben,
                  allforams = allforams
                  #synthetic = synthetic #Some missing data?? Negative errors?
)

## Fit Deming regressions
fit_list <- lapply(data_list, function(df) {
  deming(D47ICDES ~ calcTiso, data = df, xstd = Terror, ystd = SE47)
})

  
## Calculate prediction interval statistics
pred_interval_stats <- lapply(seq_along(fit_list), function(i) {
  fit <- fit_list[[i]]
  df <- data_list[[i]]
  
  # Extract coefficients and variance components
  slope <- coef(fit)[2]
  intercept <- coef(fit)[1]
  sigma2 <- fit$sigma^2
  
  # Extract variance-covariance matrix from the deming object
  # The deming object stores this in fit$variance
  vcov_mat <- fit$variance
  
  df_resid <- fit$n - 2
  
  # Use the range of observed X values for prediction
  x_range <- range(df$calcTiso, na.rm = TRUE)
  x_mean <- mean(df$calcTiso, na.rm = TRUE)
  
  # Calculate prediction interval width at mean X value
  X_mean <- c(1, x_mean)
  var_fit_mean <- as.numeric(t(X_mean) %*% vcov_mat %*% X_mean)
  var_pred_mean <- var_fit_mean + sigma2
  se_pred_mean <- sqrt(var_pred_mean)
  
  # t-critical value for 95% PI
  t_crit <- qt(0.975, df_resid)
  
  # Prediction interval half-width and full width at mean
  pi_halfwidth_mean <- t_crit * se_pred_mean
  pi_width_mean <- 2 * pi_halfwidth_mean
  
  # Also calculate average PI width across the observed data range
  x_seq <- seq(x_range[1], x_range[2], length.out = 100)
  se_pred_all <- sapply(x_seq, function(x) {
    X <- c(1, x)
    var_fit <- as.numeric(t(X) %*% vcov_mat %*% X)
    var_pred <- var_fit + sigma2
    sqrt(var_pred)
  })
  
  pi_halfwidth_avg <- mean(t_crit * se_pred_all)
  pi_width_avg <- 2 * pi_halfwidth_avg
  
  data.frame(
    group = names(data_list)[i],
    n = fit$n,
    slope = slope,
    intercept = intercept,
    residual_sd = sqrt(sigma2),
    PI_width_at_mean = pi_width_mean,
    PI_avg_width = pi_width_avg,
    stringsAsFactors = FALSE
  )
})

## Summary table
summary_table <- do.call(rbind, pred_interval_stats)
rownames(summary_table) <- NULL

write.csv(summary_table, here("results", "Deming_PI_Table3.csv"))




