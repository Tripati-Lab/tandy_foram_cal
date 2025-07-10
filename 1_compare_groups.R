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

# Perform comparisons

## Fit Deming regressions
fit_list <- lapply(data_list, function(df) {
  deming(D47ICDES ~ calcTiso, data = df, xstd = Terror, ystd = SE47)
})

## Extract regression parameters
extract_params <- function(fit) {
  list(coef = coef(fit), se = fit$se)
}
param_list <- lapply(fit_list, extract_params)

## Perform pairwise comparisons
group_names <- names(data_list)
pairs <- combn(group_names, 2, simplify = FALSE)

compare_pairs <- function(pair, param_list) {
  a <- param_list[[pair[1]]]
  b <- param_list[[pair[2]]]
  
  # Slope comparisons
  z_slope <- (a$coef[2] - b$coef[2]) / sqrt(a$se[2]^2 + b$se[2]^2)
  p_slope <- 2 * (1 - pnorm(abs(z_slope)))
  
  # Intercept comparisons
  z_int <- (a$coef[1] - b$coef[1]) / sqrt(a$se[1]^2 + b$se[1]^2)
  p_int <- 2 * (1 - pnorm(abs(z_int)))
  
  data.frame(
    group1 = pair[1],
    group2 = pair[2],
    z_slope = z_slope,
    p_slope = p_slope,
    z_intercept = z_int,
    p_intercept = p_int
  )
}

results_raw <- do.call(rbind, lapply(pairs, compare_pairs, param_list = param_list))
results_raw$p_slope_adj <- p.adjust(results_raw$p_slope, method = "bonferroni")
results_raw$p_intercept_adj <- p.adjust(results_raw$p_intercept, method = "bonferroni")

## Results (with adjusted p-values)
results_raw
write.csv(results_raw, here("results", "pairwise.comparisons.csv"))

# Create plot showing differences in slopes/intercept between groups
groups <- sort(unique(c(results_raw$group1, results_raw$group2)))
n <- length(groups)
# 0 = no difference, 1 = slope only, 2 = intercept only, 3 = both
diff_matrix <- matrix(0, nrow = n, ncol = n, dimnames = list(groups, groups))

# Create matrix summaring differences
for (i in seq_len(nrow(results_raw))) {
  row <- results_raw[i, ]
  g1 <- row$group1
  g2 <- row$group2
  
  # Determine the type of difference (slope or intercept)
  code <- 0
  if (row$p_slope_adj < 0.05) code <- code + 1
  if (row$p_intercept_adj < 0.05) code <- code + 2
  
  diff_matrix[g2, g1] <- code
}

colors <- c("grey", "red", "blue", "purple")
image_matrix <- diff_matrix
image_matrix[upper.tri(image_matrix, diag = TRUE)] <- NA  # remove upper triangle

## Plot heatmap
pdf(here("results", "heatmap.pdf"))
image(1:n, 1:n, t(image_matrix)[, n:1],
      col = colors, zlim = c(0, 3), 
      axes = FALSE, xlab = "", 
      ylab = "", main = "Pairwise differences")
axis(1, at = 1:n, labels = groups, las = 2)
axis(2, at = 1:n, labels = rev(groups), las = 2)
legend("topright",
       legend = c("No difference", "Slope only", "Intercept only", "Both"),
       fill = colors, cex = 0.8)
dev.off()


