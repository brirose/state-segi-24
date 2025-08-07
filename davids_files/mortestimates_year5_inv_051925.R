### In this code
### model fit for probability of mature sequoia dying from fire
### figure visualizing that fit, etc. 



# Load libraries
library(readxl)
library(sf)
library(dplyr)
library(terra)
library(purrr)
library(lubridate)
library(brms)
library(here)


### NOW MODEL USING TREES SURVEYED 1-5 YEARS POSTIFRE

shive_large_trees_visited_years5 <- read.csv(here("davids_files/shive_large_trees_visited_years5.csv"))

# Inversing the 0s and 1s for postfire status to invert the curve
# so that it represents probability of segi dying ~ burn severity

shive_large_trees_visited_years5$postfire_status_inv <- ifelse(
  shive_large_trees_visited_years5$postfire_status == 0, 
  1, 
  0
)

# model fit
# (we'll ignore this though and instead just read in the model object)

# model_years5_inv <- brm(
#   postfire_status_inv ~ s(rdnbr),  # Formula: status_0_1 as a function of rdnbr
#   data = shive_large_trees_visited_years5,  # Dataset containing the variables
#   family = bernoulli(),  # Bernoulli distribution for binary outcome
#   chains = 4,            # Number of Markov chains for MCMC
#   iter = 2000,           # Number of iterations per chain
#   warmup = 1000,         # Number of warm-up iterations
#   control = list(adapt_delta = 0.95),  # Increase adapt_delta
#   set.seed(666)
# )

# save model object
# saveRDS(model_years5_inv, file = "model_years5_inv.rds")

# read in model object
model_years5_inv <- readRDS(here("davids_files/model_years5_inv.rds"))

#calculate the 90%CI of the smoothing parameter
# I'm just doing this for the statistical methods write up
# Shive et al can ignore this for coding the figure

{
  
  # Extract posterior samples for the smooth term 'rdnbr'
  posterior_samples <- posterior_samples(model_years5_inv)
  
  # Look at the names of the parameters to understand the smooth terms
  names(posterior_samples)
  
  # Extract the specific samples for the smooth term (usually named `b_s(rdnbr)` in `brms`)
  rdnbr_samples <- posterior_samples[,2]
  
  # Calculate the 90% credible interval (905CI)
  ci_90 <- quantile(rdnbr_samples, probs = c(0.05, 0.95))
  
  # Print the 90% credible interval
  print(ci_90)
  # -1.2085465 -0.8878356 
  
  ###########
  
  # Extract the specific samples for the smooth term (usually named `b_s(rdnbr)` in `brms`)
  rdnbr_samples <- posterior_samples[,1]
  
  # Calculate the 90% credible interval (905CI)
  ci_90 <- quantile(rdnbr_samples, probs = c(0.05, 0.95))
  
  # Print the 90% credible interval
  print(ci_90)
  # 14.37019 33.78563
}

# Extract fitted values from the cleaned model
fitted_values_clean <- fitted(model_years5_inv)

rdnbr_seq <- seq(min(shive_large_trees_visited_years5$rdnbr, na.rm = TRUE), 
                 max(shive_large_trees_visited_years5$rdnbr, na.rm = TRUE), 
                 length.out = 1415)

# Define categories and colors
breaks_burn <- c(-Inf, 69, 316, 640, Inf)
labels_burn <- c("Unch", "Low", "Mod", "High")
colors <- c("darkgrey", "yellow", "orange", "red")

# Add rdnbr_category to the dataset
new_data <- data.frame(rdnbr = rdnbr_seq)

new_data$rdnbr_category <- cut(new_data$rdnbr, 
                               breaks = breaks_burn, 
                               labels = labels_burn, 
                               include.lowest = TRUE)

# Predict fitted values 
fitted_values <- fitted(model_years5_inv, newdata = new_data, probs = c(0.05, 0.95))
new_data$fitted_seq <- fitted_values[, "Estimate"]
new_data$lower_ci <- fitted_values[, "Q5"]
new_data$upper_ci <- fitted_values[, "Q95"]


# pdf("Figure_mort_logit_040225.pdf", width = 10, height = 8, useDingbats = FALSE)
# tiff("Figure_mort_logit_051925.tiff", width = 10, height = 10, units = "in", res = 300, compression = "lzw")

png(here("outputs/figures_for_manuscript/Figure_mort_logit_051925.png"), width = 10, height = 8, units = "in", res = 300)


par(mar = c(6,6,1,1))


# Plot observed data
plot(postfire_status_inv ~ rdnbr, data = shive_large_trees_visited_years5,  
     xlim = c(-100, 1300), 
     ylim = c(0, 1),
     main = "",
     # main = "Mature SEGI Probability Mortality vs. Fire Severity",
     # xlab = "RdNBR",
     xlab = "", 
     ylab = "",
     xaxt = "n", 
     yaxt = "n",
     # ylab = "Live/Dead",
     pch = 19, cex = 1.3,
     col = "grey")  # Single color for data points

mtext("Mortality probability", side = 2, line = 3.2, cex = 1.5)
mtext("RdNBR", side = 1, line = 3.2, cex = 1.5)
# mtext("Mature Giant Sequoia", side = 3, line = 3, cex = 3)
# mtext("Mortality Probability Following Wildfire", side = 3, line = 0.7, cex = 2.5)

axis(1, at = c(0, 200, 400, 600, 800, 1000, 1200), 
     labels = FALSE)  # No labels
mtext(c("0", "200", "400", "600", "800", "1000", "1200"), 
      side = 1, line = 1, at = c(0, 200, 400, 600, 800, 1000, 1200), cex = 1.3)

axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
     labels = FALSE)  # No labels
mtext(c("0", "0.2", "0.4", "0.6", "0.8", "1"), 
      side = 2, line = 1, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex = 1.3, las = 2)

# Add gridlines
# abline(h = seq(0, 1, by = 0.1), col = "gray90", lty = 2)
# abline(v = seq(-100, 1300, by = 100), col = "gray90", lty = 2)

# Add predicted lines by category
for (i in seq_along(labels_burn)) {
  category <- labels_burn[i]
  color <- colors[i]
  
  # Filter the data based on the current category
  category_data <- new_data[new_data$rdnbr_category == category, ]
  
  # Add the main fitted line for the category
  lines(category_data$rdnbr, category_data$fitted_seq, col = color, lwd = 6)
  
  # Add credible interval lines for the category
  lines(category_data$rdnbr, category_data$lower_ci, col = "black", lty = 2, lwd = 2)
  lines(category_data$rdnbr, category_data$upper_ci, col = "black", lty = 2, lwd = 2)
}

rect(-113, 0, 69, 1, col = rgb(0.7, 0.7, 0.7, 0.5), border = NA)   # Unchanged
rect(70, 0, 316, 1, col = rgb(1, 1, 0, 0.3), border = NA)          # Low severity
rect(317, 0, 640, 1, col = rgb(1, 0.65, 0, 0.3), border = NA)       # Moderate severity
rect(641, 0, 1301, 1, col = rgb(1, 0, 0, 0.3), border = NA)         # High severity

legend("topleft", inset = c(0.045, 0.065), 
       cex = 0.9, 
       # bty = "n",
       bg = "white",
       box.col = "white",
       legend = c(
         "Unchanged", 
         "Low severity", 
         "Moderate severity", 
         "High severity", 
         "90% Credible interval", 
         "Raw data - live/dead", 
         "Raw data - mean \u00B1 binom. CI *"
       ),
       col = c("darkgrey", "yellow", "orange", "red", "black", "darkgrey", "black"),
       pt.cex = 2,
       y.intersp = 1.2,
       pch = c(NA, NA, NA, NA, NA, 20, 20),
       lty = c(1, 1, 1, 1, 3, NA, NA),  # Line types for legend
       lwd = c(6, 6, 6, 6, 4, NA, NA))  # Line widths




# add binned raw data means and std.errors

{
  
  ## ADD IN POINTS FOR RAW DATA MEAN AND STD.ERROR
  ## of 200 rdnbr window around plotted location
  
  large_0 <- shive_large_trees_visited_years5[shive_large_trees_visited_years5$rdnbr < 1,]
  large_200 <- shive_large_trees_visited_years5[shive_large_trees_visited_years5$rdnbr < 201 &
                                                  shive_large_trees_visited_years5$rdnbr > 0  ,]
  large_400 <- shive_large_trees_visited_years5[shive_large_trees_visited_years5$rdnbr > 200 & 
                                                  shive_large_trees_visited_years5$rdnbr < 401,]
  large_600 <- shive_large_trees_visited_years5[shive_large_trees_visited_years5$rdnbr > 400 & 
                                                  shive_large_trees_visited_years5$rdnbr < 601,]
  large_800 <- shive_large_trees_visited_years5[shive_large_trees_visited_years5$rdnbr > 600 & 
                                                  shive_large_trees_visited_years5$rdnbr < 801,]
  large_1000 <- shive_large_trees_visited_years5[shive_large_trees_visited_years5$rdnbr > 800 & 
                                                   shive_large_trees_visited_years5$rdnbr < 1001,]
  large_1200 <- shive_large_trees_visited_years5[shive_large_trees_visited_years5$rdnbr > 1000 & 
                                                   shive_large_trees_visited_years5$rdnbr < 1201,]
  large_1400 <- shive_large_trees_visited_years5[shive_large_trees_visited_years5$rdnbr > 1200 & 
                                                   shive_large_trees_visited_years5$rdnbr < 1401,]
  
  ###########
  ###########
  ###########
  
  # table(large_0$postfire_status)
  # 
  # 
  # 
  # mean(large_0$postfire_status_inv, na.rm = T)
  # # 
  # std.error(large_0$postfire_status_inv, na.rm = T)
  # # 
  # 
  # x <- -100
  # y <- mean(large_0$postfire_status_inv, na.rm = T)
  # y_offset <- std.error(large_0$postfire_status_inv, na.rm = T)
  # 
  # # Plot the point
  # points(x, y, cex = 3, pch = 21, lwd = 2)
  # points(x, y, cex = 3, pch = 20, lwd = 2)
  # 
  # 
  # # Add the vertical line (segment)
  # segments(x, y - y_offset, x, y + y_offset, col = "black", lwd = 2)
  
  # Calculate the proportion and 95% binomial confidence interval
  successes <- sum(large_0$postfire_status_inv, na.rm = TRUE)
  n <- sum(!is.na(large_0$postfire_status_inv))
  binom_result <- binom.test(successes, n)
  
  # Extract values
  y <- as.numeric(binom_result$estimate)          # Proportion
  ci_lower <- binom_result$conf.int[1]            # Lower bound of CI
  ci_upper <- binom_result$conf.int[2]            # Upper bound of CI
  
  # x-position for plotting
  x <- -100
  
  # Plot the point (mean proportion)
  points(x, y, cex = 2.3, pch = 21, lwd = 2)
  points(x, y, cex = 2, pch = 20, lwd = 2)
  
  # Add vertical line for binomial CI
  segments(x, ci_lower, x, ci_upper, col = "black", lwd = 2)
  
  
  #####################
  #####################
  
  
  # table(large_200$postfire_status_inv)
  # 
  # 
  # 
  # mean(large_200$postfire_status_inv, na.rm = T)
  # # 
  # std.error(large_200$postfire_status_inv, na.rm = T)
  # # 
  # 
  # x <- 100
  # y <- mean(large_200$postfire_status_inv, na.rm = T)
  # y_offset <- std.error(large_200$postfire_status_inv, na.rm = T)
  # 
  # # Plot the point
  # points(x, y, cex = 3, pch = 21, lwd = 2)
  # points(x, y, cex = 3, pch = 20, lwd = 2)
  # 
  # 
  # # Add the vertical line (segment)
  # segments(x, y - y_offset, x, y + y_offset, col = "black", lwd = 2)
  
  # Calculate the proportion and 95% binomial confidence interval
  successes <- sum(large_200$postfire_status_inv, na.rm = TRUE)
  n <- sum(!is.na(large_200$postfire_status_inv))
  binom_result <- binom.test(successes, n)
  
  # Extract values
  y <- as.numeric(binom_result$estimate)          # Proportion
  ci_lower <- binom_result$conf.int[1]            # Lower bound of CI
  ci_upper <- binom_result$conf.int[2]            # Upper bound of CI
  
  # x-position for plotting
  x <- 100
  
  # Plot the point (mean proportion)
  points(x, y, cex = 2.3, pch = 21, lwd = 2)
  points(x, y, cex = 2, pch = 20, lwd = 2)
  
  # Add vertical line for binomial CI
  segments(x, ci_lower, x, ci_upper, col = "black", lwd = 2)
  
  
  #####################
  #####################
  
  # table(large_400$postfire_status_inv)
  # 
  # 
  # mean(large_400$postfire_status_inv, na.rm = T)
  # # 
  # std.error(large_400$postfire_status_inv)
  # # 
  # 
  # x <- 300
  # y <- mean(large_400$postfire_status_inv, na.rm = T)
  # y_offset <- std.error(large_400$postfire_status_inv)
  # 
  # # Plot the point
  # points(x, y, cex = 3, pch = 21, lwd = 2)
  # points(x, y, cex = 3, pch = 20, lwd = 2)
  # 
  # # Add the vertical line (segment)
  # segments(x, y - y_offset, x, y + y_offset, col = "black", lwd = 2)
  
  # Calculate the proportion and 95% binomial confidence interval
  successes <- sum(large_400$postfire_status_inv, na.rm = TRUE)
  n <- sum(!is.na(large_400$postfire_status_inv))
  binom_result <- binom.test(successes, n)
  
  # Extract values
  y <- as.numeric(binom_result$estimate)          # Proportion
  ci_lower <- binom_result$conf.int[1]            # Lower bound of CI
  ci_upper <- binom_result$conf.int[2]            # Upper bound of CI
  
  # x-position for plotting
  x <- 300
  
  # Plot the point (mean proportion)
  points(x, y, cex = 2.3, pch = 21, lwd = 2)
  points(x, y, cex = 2, pch = 20, lwd = 2)
  
  # Add vertical line for binomial CI
  segments(x, ci_lower, x, ci_upper, col = "black", lwd = 2)
  
  
  #####################
  #####################
  
  # table(large_600$postfire_status_inv)
  # 
  # 
  # mean(large_600$postfire_status_inv, na.rm = T)
  # # 
  # std.error(large_600$postfire_status_inv)
  # # 
  # 
  # x <- 500
  # y <- mean(large_600$postfire_status_inv, na.rm = T)
  # y_offset <- std.error(large_600$postfire_status_inv)
  # 
  # # Plot the point
  # points(x, y, cex = 3, pch = 21, lwd = 2)
  # points(x, y, cex = 3, pch = 20, lwd = 2)
  # 
  # # Add the vertical line (segment)
  # segments(x, y - y_offset, x, y + y_offset, col = "black", lwd = 2)
  
  # Calculate the proportion and 95% binomial confidence interval
  successes <- sum(large_600$postfire_status_inv, na.rm = TRUE)
  n <- sum(!is.na(large_600$postfire_status_inv))
  binom_result <- binom.test(successes, n)
  
  # Extract values
  y <- as.numeric(binom_result$estimate)          # Proportion
  ci_lower <- binom_result$conf.int[1]            # Lower bound of CI
  ci_upper <- binom_result$conf.int[2]            # Upper bound of CI
  
  # x-position for plotting
  x <- 500
  
  # Plot the point (mean proportion)
  points(x, y, cex = 2.3, pch = 21, lwd = 2)
  points(x, y, cex = 2, pch = 20, lwd = 2)
  
  # Add vertical line for binomial CI
  segments(x, ci_lower, x, ci_upper, col = "black", lwd = 2)
  
  
  #####################
  #####################
  
  # table(large_800$postfire_status_inv)
  # 
  # 
  # mean(large_800$postfire_status_inv, na.rm = T)
  # # 
  # std.error(large_800$postfire_status_inv)
  # # 
  # 
  # x <- 700
  # y <- mean(large_800$postfire_status_inv, na.rm = T)
  # y_offset <- std.error(large_800$postfire_status_inv)
  # 
  # # Plot the point
  # points(x, y, cex = 3, pch = 21, lwd = 2)
  # points(x, y, cex = 3, pch = 20, lwd = 2)
  # 
  # # Add the vertical line (segment)
  # segments(x, y - y_offset, x, y + y_offset, col = "black", lwd = 2)
  
  # Calculate the proportion and 95% binomial confidence interval
  successes <- sum(large_800$postfire_status_inv, na.rm = TRUE)
  n <- sum(!is.na(large_800$postfire_status_inv))
  binom_result <- binom.test(successes, n)
  
  # Extract values
  y <- as.numeric(binom_result$estimate)          # Proportion
  ci_lower <- binom_result$conf.int[1]            # Lower bound of CI
  ci_upper <- binom_result$conf.int[2]            # Upper bound of CI
  
  # x-position for plotting
  x <- 700
  
  # Plot the point (mean proportion)
  points(x, y, cex = 2.3, pch = 21, lwd = 2)
  points(x, y, cex = 2, pch = 20, lwd = 2)
  
  # Add vertical line for binomial CI
  segments(x, ci_lower, x, ci_upper, col = "black", lwd = 2)
  
  
  #####################
  #####################  
  
  # table(large_1000$postfire_status_inv)
  # 
  # 
  # mean(large_1000$postfire_status_inv, na.rm = T)
  # # 
  # std.error(large_1000$postfire_status_inv)
  # # 
  # 
  # x <- 900
  # y <- mean(large_1000$postfire_status_inv, na.rm = T)
  # y_offset <- std.error(large_1000$postfire_status_inv)
  # 
  # # Plot the point
  # points(x, y, cex = 3, pch = 21, lwd = 2)
  # points(x, y, cex = 3, pch = 20, lwd = 2)
  # 
  # # Add the vertical line (segment)
  # segments(x, y - y_offset, x, y + y_offset, col = "black", lwd = 2)
  
  # Calculate the proportion and 95% binomial confidence interval
  successes <- sum(large_1000$postfire_status_inv, na.rm = TRUE)
  n <- sum(!is.na(large_1000$postfire_status_inv))
  binom_result <- binom.test(successes, n)
  
  # Extract values
  y <- as.numeric(binom_result$estimate)          # Proportion
  ci_lower <- binom_result$conf.int[1]            # Lower bound of CI
  ci_upper <- binom_result$conf.int[2]            # Upper bound of CI
  
  # x-position for plotting
  x <- 900
  
  # Plot the point (mean proportion)
  points(x, y, cex = 2.3, pch = 21, lwd = 2)
  points(x, y, cex = 2, pch = 20, lwd = 2)
  
  # Add vertical line for binomial CI
  segments(x, ci_lower, x, ci_upper, col = "black", lwd = 2)
  
  
  
  #####################
  #####################   
  
  # table(large_1200$postfire_status_inv)
  # 
  # 
  # mean(large_1200$postfire_status_inv, na.rm = T)
  # # 
  # std.error(large_1200$postfire_status_inv)
  # # 
  # 
  # x <- 1100
  # y <- mean(large_1200$postfire_status_inv, na.rm = T)
  # y_offset <- std.error(large_1200$postfire_status_inv)
  # 
  # # Plot the point
  # points(x, y, cex = 3, pch = 21, lwd = 2)
  # points(x, y, cex = 3, pch = 20, lwd = 2)
  # 
  # # Add the vertical line (segment)
  # segments(x, y - y_offset, x, y + y_offset, col = "black", lwd = 2)
  
  # Calculate the proportion and 95% binomial confidence interval
  successes <- sum(large_1200$postfire_status_inv, na.rm = TRUE)
  n <- sum(!is.na(large_1200$postfire_status_inv))
  binom_result <- binom.test(successes, n)
  
  # Extract values
  y <- as.numeric(binom_result$estimate)          # Proportion
  ci_lower <- binom_result$conf.int[1]            # Lower bound of CI
  ci_upper <- binom_result$conf.int[2]            # Upper bound of CI
  
  # x-position for plotting
  x <- 1100
  
  # Plot the point (mean proportion)
  points(x, y, cex = 2.3, pch = 21, lwd = 2)
  points(x, y, cex = 2, pch = 20, lwd = 2)
  
  # Add vertical line for binomial CI
  segments(x, ci_lower, x, ci_upper, col = "black", lwd = 2)
  
  
  #####################
  #####################   
  
  # table(large_1400$postfire_status_inv)
  # 
  # 
  # x <- 1300
  # y <- mean(large_1400$postfire_status_inv, na.rm = T)
  # y_offset <- std.error(large_1200$postfire_status_inv)
  # 
  # # Plot the point
  # points(x, y, cex = 3, pch = 21, lwd = 2)
  # points(x, y, cex = 3, pch = 20, lwd = 2)
  # 
  # # Add the vertical line (segment)
  # segments(x, y - y_offset, x, y + y_offset, col = "black", lwd = 2)
  
  # Calculate the proportion and 95% binomial confidence interval
  successes <- sum(large_1400$postfire_status_inv, na.rm = TRUE)
  n <- sum(!is.na(large_1400$postfire_status_inv))
  binom_result <- binom.test(successes, n)
  
  # Extract values
  y <- as.numeric(binom_result$estimate)          # Proportion
  ci_lower <- binom_result$conf.int[1]            # Lower bound of CI
  ci_upper <- binom_result$conf.int[2]            # Upper bound of CI
  
  # x-position for plotting
  x <- 1300
  
  # Plot the point (mean proportion)
  points(x, y, cex = 2.3, pch = 21, lwd = 2)
  points(x, y, cex = 2, pch = 20, lwd = 2)
  
  # Add vertical line for binomial CI
  segments(x, ci_lower, x, ci_upper, col = "black", lwd = 2)
  
  
  #####################
  #####################   
  # 
  # large_100 <- shive_large_trees_visited[shive_large_trees_visited$rdnbr < 101,]
  # 
  # mean(large_100$postfire_status, na.rm = T)
  # # 
  # std.error(large_100$postfire_status)
  # # 
  # 
  # x <- 0
  # y <- mean(large_100$postfire_status, na.rm = T)
  # y_offset <- std.error(large_100$postfire_status)
  # 
  # # Plot the point
  # points(x, y, cex = 3, pch = 21, lwd = 2)
  # points(x, y, cex = 3, pch = 20, lwd = 2)
  # 
  # # Add the vertical line (segment)
  # segments(x, y - y_offset, x, y + y_offset, col = "black", lwd = 2)
  # 
  # ######################
  # ######################
  # 
  # large_300 <- shive_large_trees_visited[shive_large_trees_visited$rdnbr < 301,]
  # 
  # mean(large_300$postfire_status, na.rm = T)
  # # 
  # std.error(large_300$postfire_status)
  # # 
  # 
  # x <- 200
  # y <- mean(large_300$postfire_status, na.rm = T)
  # y_offset <- std.error(large_300$postfire_status)
  # 
  # # Plot the point
  # points(x, y, cex = 3, pch = 21, lwd = 2)
  # points(x, y, cex = 3, pch = 20, lwd = 2)
  # 
  # # Add the vertical line (segment)
  # segments(x, y - y_offset, x, y + y_offset, col = "black", lwd = 2)
  # 
  # ######################
  # ######################
  # 
  # large_500 <- shive_large_trees_visited[shive_large_trees_visited$rdnbr > 300 & shive_large_trees_visited$rdnbr < 501, ]
  # 
  # mean(large_500$postfire_status, na.rm = T)
  # # 
  # std.error(large_500$postfire_status)
  # # 
  # 
  # x <- 400
  # y <- mean(large_500$postfire_status, na.rm = T)
  # y_offset <- std.error(large_500$postfire_status)
  # 
  # # Plot the point
  # points(x, y, cex = 3, pch = 21, lwd = 2)
  # points(x, y, cex = 3, pch = 20, lwd = 2)
  # 
  # # Add the vertical line (segment)
  # segments(x, y - y_offset, x, y + y_offset, col = "black", lwd = 2)
  # 
  # ######################
  # ######################
  # 
  # large_700 <- shive_large_trees_visited[shive_large_trees_visited$rdnbr > 500 & shive_large_trees_visited$rdnbr < 701, ]
  # 
  # mean(large_700$postfire_status, na.rm = T)
  # # 
  # std.error(large_700$postfire_status)
  # # 
  # 
  # x <- 600
  # y <- mean(large_700$postfire_status, na.rm = T)
  # y_offset <- std.error(large_700$postfire_status)
  # 
  # # Plot the point
  # points(x, y, cex = 3, pch = 21, lwd = 2)
  # points(x, y, cex = 3, pch = 20, lwd = 2)
  # 
  # # Add the vertical line (segment)
  # segments(x, y - y_offset, x, y + y_offset, col = "black", lwd = 2)
  # 
  # ######################
  # ######################
  # 
  # large_900 <- shive_large_trees_visited[shive_large_trees_visited$rdnbr > 700 & shive_large_trees_visited$rdnbr < 901, ]
  # 
  # mean(large_900$postfire_status, na.rm = T)
  # # 
  # std.error(large_900$postfire_status)
  # # 
  # 
  # x <- 800
  # y <- mean(large_900$postfire_status, na.rm = T)
  # y_offset <- std.error(large_900$postfire_status)
  # 
  # # Plot the point
  # points(x, y, cex = 3, pch = 21, lwd = 2)
  # points(x, y, cex = 3, pch = 20, lwd = 2)
  # 
  # # Add the vertical line (segment)
  # segments(x, y - y_offset, x, y + y_offset, col = "black", lwd = 2)
  # 
  # ######################
  # ######################
  # 
  # large_1100 <- shive_large_trees_visited[shive_large_trees_visited$rdnbr > 900 & shive_large_trees_visited$rdnbr < 1101, ]
  # 
  # mean(large_1100$postfire_status, na.rm = T)
  # # 
  # std.error(large_1100$postfire_status)
  # # 
  # 
  # x <- 1000
  # y <- mean(large_1100$postfire_status, na.rm = T)
  # y_offset <- std.error(large_1100$postfire_status)
  # 
  # # Plot the point
  # points(x, y, cex = 3, pch = 21, lwd = 2)
  # points(x, y, cex = 3, pch = 20, lwd = 2)
  # 
  # # Add the vertical line (segment)
  # segments(x, y - y_offset, x, y + y_offset, col = "black", lwd = 2)
  # 
  # ######################
  # ######################
  # 
  # large_1300 <- shive_large_trees_visited[shive_large_trees_visited$rdnbr > 1100 & shive_large_trees_visited$rdnbr < 1301, ]
  # 
  # mean(large_1300$postfire_status, na.rm = T)
  # # 
  # std.error(large_1300$postfire_status)
  # # 
  # 
  # x <- 1200
  # y <- mean(large_1300$postfire_status, na.rm = T)
  # y_offset <- std.error(large_1300$postfire_status)
  # 
  # # Plot the point
  # points(x, y, cex = 3, pch = 21, lwd = 2)
  # points(x, y, cex = 3, pch = 20, lwd = 2)
  # 
  # # Add the vertical line (segment)
  # segments(x, y - y_offset, x, y + y_offset, col = "black", lwd = 2)
  # 
  ######################
  ######################
  
}

# Close the TIFF device
dev.off()


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### Mortality estimates at RdNBR thresholds
### allows me to make ranges of mortality for a given burn severity category of interest
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

{
  
  # RDNBR 69
  
  {
    # Create a new data frame with rdnbr = 69
    new_data_69 <- data.frame(rdnbr = 69)
    
    # Get fitted values (median and credible intervals)
    fitted_69 <- fitted(model_years5_inv, newdata = new_data_69, summary = TRUE, probs = c(0.5, 0.05, 0.95))
    
    # Extract values
    median_mortality_69 <- fitted_69[1, "Q50"] * 100  # Median (50th percentile)
    ci_90_lower <- fitted_69[1, "Q5"] * 100  # 5th percentile (lower bound)
    ci_90_upper <- fitted_69[1, "Q95"] * 100  # 95th percentile (upper bound)
    
    # Print results
    list(
      median_mortality = median_mortality_69,
      ci_90_lower = ci_90_lower,
      ci_90_upper = ci_90_upper
    )
    
    # $median_mortality
    # Q50 
    # 1.522117 
    # 
    # $ci_90_lower
    # Q5 
    # 0.9643211 
    # 
    # $ci_90_upper
    # Q95 
    # 2.264126 
    
  }
  
  
  # RDNBR 316
  
  {
    # Create a new data frame with rdnbr = 316
    new_data_316 <- data.frame(rdnbr = 316)
    
    # Get fitted values (median and credible intervals)
    fitted_316 <- fitted(model_years5_inv, newdata = new_data_316, summary = TRUE, probs = c(0.5, 0.05, 0.95))
    
    # Extract values
    median_mortality_316 <- fitted_316[1, "Q50"] * 100  # Median (50th percentile)
    ci_90_lower <- fitted_316[1, "Q5"] * 100  # 5th percentile (lower bound)
    ci_90_upper <- fitted_316[1, "Q95"] * 100  # 95th percentile (upper bound)
    
    # Print results
    list(
      median_mortality = median_mortality_316,
      ci_90_lower = ci_90_lower,
      ci_90_upper = ci_90_upper
    )
    
    # $median_mortality
    # Q50 
    # 6.126935 
    # 
    # $ci_90_lower
    # Q5 
    # 4.965209 
    # 
    # $ci_90_upper
    # Q95 
    # 7.483591
    
    
  }
  
  # RDNBR 640
  
  {
    # Create a new data frame with rdnbr = 640
    new_data_640 <- data.frame(rdnbr = 640)
    
    # Get fitted values (median and credible intervals)
    fitted_640 <- fitted(model_years5_inv, newdata = new_data_640, summary = TRUE, probs = c(0.5, 0.05, 0.95))
    
    # Extract values
    median_mortality_640 <- fitted_640[1, "Q50"] * 100  # Median (50th percentile)
    ci_90_lower <- fitted_640[1, "Q5"] * 100  # 5th percentile (lower bound)
    ci_90_upper <- fitted_640[1, "Q95"] * 100  # 95th percentile (upper bound)
    
    # Print results
    list(
      median_mortality = median_mortality_640,
      ci_90_lower = ci_90_lower,
      ci_90_upper = ci_90_upper
    )
    
    # $median_mortality
    # Q50 
    # 32.88953 
    # 
    # $ci_90_lower
    # Q5 
    # 29.85085 
    # 
    # $ci_90_upper
    # Q95 
    # 36.09612 
    
    
  }
  
  # RDNBR 800
  
  {
    # Create a new data frame with rdnbr = 800
    new_data_800 <- data.frame(rdnbr = 800)
    
    # Get fitted values (median and credible intervals)
    fitted_800 <- fitted(model_years5_inv, newdata = new_data_800, summary = TRUE, probs = c(0.5, 0.05, 0.95))
    
    # Extract values
    median_mortality_800 <- fitted_800[1, "Q50"] * 100  # Median (50th percentile)
    ci_90_lower <- fitted_800[1, "Q5"] * 100  # 5th percentile (lower bound)
    ci_90_upper <- fitted_800[1, "Q95"] * 100  # 95th percentile (upper bound)
    
    # Print results
    list(
      median_mortality = median_mortality_800,
      ci_90_lower = ci_90_lower,
      ci_90_upper = ci_90_upper
    )
    
    # $median_mortality
    # Q50 
    # 63.08341 
    # 
    # $ci_90_lower
    # Q5 
    # 59.10216 
    # 
    # $ci_90_upper
    # Q95 
    # 66.70833 
    
  }
  
  # RDNBR 900
  
  {
    # Create a new data frame with rdnbr = 900
    new_data_900 <- data.frame(rdnbr = 900)
    
    # Get fitted values (median and credible intervals)
    fitted_900 <- fitted(model_years5_inv, newdata = new_data_900, summary = TRUE, probs = c(0.5, 0.05, 0.95))
    
    # Extract values
    median_mortality_900 <- fitted_900[1, "Q50"] * 100  # Median (50th percentile)
    ci_90_lower <- fitted_900[1, "Q5"] * 100  # 5th percentile (lower bound)
    ci_90_upper <- fitted_900[1, "Q95"] * 100  # 95th percentile (upper bound)
    
    # Print results
    list(
      median_mortality = median_mortality_900,
      ci_90_lower = ci_90_lower,
      ci_90_upper = ci_90_upper
    )
    
    # $median_mortality
    # Q50 
    # 81.97223 
    # 
    # $ci_90_lower
    # Q5 
    # 78.93916 
    # 
    # $ci_90_upper
    # Q95 
    # 84.61508 
    
  }
  
  # RDNBR 1000
  
  {
    
    # Create a new data frame with rdnbr = 1000
    new_data_1000 <- data.frame(rdnbr = 1000)
    
    # Get fitted values (median and credible intervals)
    fitted_1000 <- fitted(model_years5_inv, newdata = new_data_1000, summary = TRUE, probs = c(0.5, 0.05, 0.95))
    
    # Extract values
    median_mortality_1000 <- fitted_1000[1, "Q50"] * 100  # Median (50th percentile)
    ci_90_lower <- fitted_1000[1, "Q5"] * 100  # 5th percentile (lower bound)
    ci_90_upper <- fitted_1000[1, "Q95"] * 100  # 95th percentile (upper bound)
    
    # Print results
    list(
      median_mortality = median_mortality_1000,
      ci_90_lower = ci_90_lower,
      ci_90_upper = ci_90_upper
    )
    
    # $median_mortality
    # Q50 
    # 93.44118 
    # 
    # $ci_90_lower
    # Q5 
    # 91.66338 
    # 
    # $ci_90_upper
    # Q95 
    # 95.03745 
    
  }
  
  # RDNBR 1100
  
  {
    
    # Create a new data frame with rdnbr = 1100
    new_data_1100 <- data.frame(rdnbr = 1100)
    
    # Get fitted values (median and credible intervals)
    fitted_1100 <- fitted(model_years5_inv, newdata = new_data_1100, summary = TRUE, probs = c(0.5, 0.05, 0.95))
    
    # Extract values
    median_mortality_1100 <- fitted_1100[1, "Q50"] * 100  # Median (50th percentile)
    ci_90_lower <- fitted_1100[1, "Q5"] * 100  # 5th percentile (lower bound)
    ci_90_upper <- fitted_1100[1, "Q95"] * 100  # 95th percentile (upper bound)
    
    # Print results
    list(
      median_mortality = median_mortality_1100,
      ci_90_lower = ci_90_lower,
      ci_90_upper = ci_90_upper
    )
    
    # $median_mortality
    # Q50 
    # 97.97426 
    # 
    # $ci_90_lower
    # Q5 
    # 96.86035 
    # 
    # $ci_90_upper
    # Q95 
    # 98.80011 
    
  }
  
}
