

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# CODE TO CREATE SEQUOIA MORTALITY OVER TIME FIGURE
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###


# load necessary packages
library(plotrix)
library(terra)
library(sf)
library(readxl)
library(dplyr)
library(raster)
library(brms)
library(tidyr)
library(here)

# SET WORKING DIRECTORY TO WHEREVER DATASET IS 
setwd(here("davids_files"))

# mort_df <- read_excel("sequoia_mortestimates_03262025.xlsx", sheet = "mortest_Years5")
mort_df <- read.csv("mortest_Year5_06052025.csv", header = T)


### MAKE PLOT

# tiff("sequoia_mortality_annual_071525.tiff", width = 10, height = 7, units = "in", res = 300, compression = "lzw")
png("sequoia_mortality_annual_071525.png", width = 10, height = 7, units = "in", res = 600)


# Ensure 'year' column is numeric
mort_df$year <- as.numeric(as.character(mort_df$year))

# Calculate the scaling factor for the right axis
scaling_factor <- 0.1 / 7018.6  # Right y-axis value 0.1 should correspond to cum_mort_est_50 == 7018.6

# Open a plotting window with two y-axes
par(mar = c(7, 7, 2, 7))  # Adjust margins to fit right-side axis

# Base R plot (Primary Y-Axis: cum_mort_est_50)
plot(mort_df$year - 0.5, mort_df$cum_mort_est_50,
     type = "s", col = "black", lwd = 3,
     xlab = "", 
     ylim = c(0, 14000),  # Set y-axis max limit to 14000 for left side
     las = 2,
     ylab = "",
     main = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 1)

mtext("Year", side = 1, line = 4, cex = 1.2)
mtext("Number of large sequoia", side = 2, line = 4, cex = 1.3)

##fix left axis to have commas
left_ticks_lbl <- c("0", "2,000", "4,000", "6,000", "8,000", "10,000", "12,000", "14,000")
left_ticks_actual <- c(0,2000,4000,6000,8000,10000,12000,14000)
# left_ticks <- right_ticks_count.scale * 70186  # Scale to left y-axis count values

axis(2, at = left_ticks_actual, labels = left_ticks_lbl, las = 2, cex.axis = 1)
mtext("Proportion of total large sequoia population", side = 4, line = 4, cex = 1.3)


# Add x-axis ticks every 1 year
axis(1, at = seq(1985, 2024, by = 1), labels = seq(1985, 2024, by = 1), las = 2, cex.axis = 1)

# # Add main estimate line with adjusted step positions
# lines(mort_df$year - 0.5, mort_df$cum_mort_est_50, col = "black", lwd = 3, lty = 1, type = "s")
# 
# # Add dashed lines for confidence intervals with adjusted step positions
# lines(mort_df$year - 0.5, mort_df$cum_mort_est_05, col = "darkgrey", lwd = 3, lty = 3, type = "s")
# lines(mort_df$year - 0.5, mort_df$cum_mort_est_095, col = "darkgrey", lwd = 3, lty = 3, type = "s")

# Extend step lines to 2024.5


lines(c(mort_df$year - 0.5, 2024.5), 
      c(mort_df$cum_mort_est_05, tail(mort_df$cum_mort_est_05, 1)), 
      col = "darkgrey", lwd = 2, lty = 1, type = "s")

lines(c(mort_df$year - 0.5, 2024.5), 
      c(mort_df$cum_mort_est_095, tail(mort_df$cum_mort_est_095, 1)), 
      col = "darkgrey", lwd = 2, lty = 1, type = "s")

lines(c(mort_df$year - 0.5, 2024.5),
      c(mort_df$cum_mort_est_50, tail(mort_df$cum_mort_est_50, 1)),
      col = "black", lwd = 3, lty = 1, type = "s")


# Add horizontal dashed abline at 70816
abline(h = 70186, lty = 2, col = "darkgrey", lwd = 1.25)

# Add right-side axis for percpop.scal
right_ticks_count <- formatC(c(0, 0.02, 0.04, 0.06, 0.08, 0.10, 0.12, 0.14, 0.16, 0.18, 0.20), digits = 2, format = "f")
right_ticks_count.scale <- c(0, 0.02, 0.04, 0.06, 0.08, 0.10, 0.12, 0.14, 0.16, 0.18, 0.20)
right_ticks_scaled <- right_ticks_count.scale * 70186  # Scale to left y-axis count values

axis(4, at = right_ticks_scaled, labels = right_ticks_count, las = 2, cex.axis = 1)
mtext("Proportion of total large sequoia population", side = 4, line = 4, cex = 1.3)

# Add horizontal dashed lines for percentage markers
abline(h = 70186 * 0.20, lty = 2, col = "darkgrey", lwd = 1.0)
abline(h = 70186 * 0.18, lty = 2, col = "darkgrey", lwd = 1.0)
abline(h = 70186 * 0.16, lty = 2, col = "darkgrey", lwd = 1.0)
abline(h = 70186 * 0.14, lty = 2, col = "darkgrey", lwd = 1.0)
abline(h = 70186 * 0.12, lty = 2, col = "darkgrey", lwd = 1.0)
abline(h = 70186 * 0.10, lty = 2, col = "darkgrey", lwd = 1.0)
abline(h = 70186 * 0.08, lty = 2, col = "darkgrey", lwd = 1.0)
abline(h = 70186 * 0.06, lty = 2, col = "darkgrey", lwd = 1.0)
abline(h = 70186 * 0.04, lty = 2, col = "darkgrey", lwd = 1.0)
abline(h = 70186 * 0.02, lty = 2, col = "darkgrey", lwd = 1.0)
abline(h = 70186 * 0.0, lty = 1, col = "black", lwd = 1.2)


# Add legend
legend("topleft", 
       legend = c("Mean estimate", 
                  "90% Credible interval", 
                  "Large sequioa mortality"),
       col = c("black", "darkgrey", "black"),
       pt.bg = c(NA, NA, "red"),
       pt.cex = c(NA,NA,2),
       lty = c(1, 1, NA), 
       pch = c(NA,NA,22),
       lwd = c(3,3,NA),
       # bty = "n",
       cex = 1.0,
       box.col = "black",                 # Black outline for the legend box
       bg = "white",                      # White background for the legend
       inset = c(0.05, 0.05),
       y.intersp = 1.5)



###################
################### BARPLOT ADDITION
###################

all_years <- seq(min(mort_df$year), max(mort_df$year))

# Aggregate and fill missing years with 0s
mort_summary2 <- mort_df %>%
  group_by(year) %>%
  summarize(
    mort_est_50 = sum(mort_est_50, na.rm = TRUE),
    mort_est_05 = sum(mort_est_05, na.rm = TRUE),
    mort_est_095 = sum(mort_est_095, na.rm = TRUE),
    mort_est_025 = sum(mort_est_025, na.rm = TRUE),
    mort_est_975 = sum(mort_est_975, na.rm = TRUE)
  ) %>%
  right_join(data.frame(year = all_years), by = "year") %>%
  replace_na(list(mort_est_50 = 0, 
                  mort_est_05 = 0, 
                  mort_est_095 = 0,
                  mort_est_025 = 0,
                  mort_est_975 = 0))

# Compute bar widths dynamically (assuming evenly spaced years)
bar_width <- 0.5  # Adjust this value to change bar spacing

# Overlay bars using `rect()`
for (i in seq_along(mort_summary2$year)) {
  rect(
    xleft = as.numeric(mort_summary2$year[i]) - bar_width, 
    xright = as.numeric(mort_summary2$year[i]) + bar_width,
    ybottom = 0, 
    ytop = mort_summary2$mort_est_50[i], 
    col = adjustcolor("red", alpha.f = 0.6), 
    border = "black"
  )
}



#################
################# POLYGONS
#################

{

# Set the y-values for the polygon at x = 2014.5 and 2016
top_y <- max(mort_df$cum_mort_est_095[mort_df$year == 2015])  # Top of the polygon
bottom_y <- max(mort_df$cum_mort_est_05[mort_df$year == 2015])  # Bottom of the polygon

# Define the x and y coordinates for the polygon
polygon_x <- c(2014.5, 2015.5, 2015.5, 2014.5)
polygon_y <- c(bottom_y, bottom_y, top_y, top_y)

# Plot the grey polygon
polygon(polygon_x, polygon_y, col = adjustcolor("grey", alpha.f = 0.5), border = NA)

####
####
####

# Set the y-values for the polygon at x = 2014.5 and 2016
top_y <- max(mort_df$cum_mort_est_095[mort_df$year == 2016])  # Top of the polygon
bottom_y <- max(mort_df$cum_mort_est_05[mort_df$year == 2016])  # Bottom of the polygon

# Define the x and y coordinates for the polygon
polygon_x <- c(2015.5, 2016.5, 2016.5, 2015.5)
polygon_y <- c(bottom_y, bottom_y, top_y, top_y)

# Plot the grey polygon
polygon(polygon_x, polygon_y, col = adjustcolor("grey", alpha.f = 0.5), border = NA)

####
####
####

# Set the y-values for the polygon at x = 2014.5 and 2016
top_y <- max(mort_df$cum_mort_est_095[mort_df$year == 2017])  # Top of the polygon
bottom_y <- max(mort_df$cum_mort_est_05[mort_df$year == 2017])  # Bottom of the polygon

# Define the x and y coordinates for the polygon
polygon_x <- c(2016.5, 2017.5, 2017.5, 2016.5)
polygon_y <- c(bottom_y, bottom_y, top_y, top_y)

# Plot the grey polygon
polygon(polygon_x, polygon_y, col = adjustcolor("grey", alpha.f = 0.5), border = NA)

####
####
####

# Set the y-values for the polygon at x = 2014.5 and 2016
top_y <- max(mort_df$cum_mort_est_095[mort_df$year == 2019])  # Top of the polygon
bottom_y <- max(mort_df$cum_mort_est_05[mort_df$year == 2019])  # Bottom of the polygon

# Define the x and y coordinates for the polygon
polygon_x <- c(2017.5, 2019.5, 2019.5, 2017.5)
polygon_y <- c(bottom_y, bottom_y, top_y, top_y)

# Plot the grey polygon
polygon(polygon_x, polygon_y, col = adjustcolor("grey", alpha.f = 0.5), border = NA)

####
####
####


# Set the y-values for the polygon at x = 2014.5 and 2016
top_y <- max(mort_df$cum_mort_est_095[mort_df$year == 2020])  # Top of the polygon
bottom_y <- max(mort_df$cum_mort_est_05[mort_df$year == 2020])  # Bottom of the polygon

# Define the x and y coordinates for the polygon
polygon_x <- c(2019.5, 2020.5, 2020.5, 2019.5)
polygon_y <- c(bottom_y, bottom_y, top_y, top_y)

# Plot the grey polygon
polygon(polygon_x, polygon_y, col = adjustcolor("grey", alpha.f = 0.5), border = NA)

####
####
####


# Set the y-values for the polygon at x = 2014.5 and 2016
top_y <- max(mort_df$cum_mort_est_095[mort_df$year == 2023])  # Top of the polygon
bottom_y <- max(mort_df$cum_mort_est_05[mort_df$year == 2023])  # Bottom of the polygon

# Define the x and y coordinates for the polygon
polygon_x <- c(2020.5, 2023.5, 2023.5, 2020.5)
polygon_y <- c(bottom_y, bottom_y, top_y, top_y)

# Plot the grey polygon
polygon(polygon_x, polygon_y, col = adjustcolor("grey", alpha.f = 0.5), border = NA)


####
####
####


# Set the y-values for the polygon at x = 2014.5 and 2016
top_y <- max(mort_df$cum_mort_est_095[mort_df$year == 2024])  # Top of the polygon
bottom_y <- max(mort_df$cum_mort_est_05[mort_df$year == 2024])  # Bottom of the polygon

# Define the x and y coordinates for the polygon
polygon_x <- c(2023.5, 2024.5, 2024.5, 2023.5)
polygon_y <- c(bottom_y, bottom_y, top_y, top_y)

# Plot the grey polygon
polygon(polygon_x, polygon_y, col = adjustcolor("grey", alpha.f = 0.5), border = NA)

}

###################
################### BARPLOT ADDITION
###################

# Aggregate and fill missing years with 0s
mort_summary2 <- mort_df %>%
  group_by(year) %>%
  summarize(
    mort_est_50 = sum(mort_est_50, na.rm = TRUE),
    mort_est_05 = sum(mort_est_05, na.rm = TRUE),
    mort_est_095 = sum(mort_est_095, na.rm = TRUE),
    mort_est_025 = sum(mort_est_025, na.rm = TRUE),
    mort_est_975 = sum(mort_est_975, na.rm = TRUE)
  ) %>%
  right_join(data.frame(year = all_years), by = "year") %>%
  replace_na(list(mort_est_50 = 0, 
                  mort_est_05 = 0, 
                  mort_est_095 = 0,
                  mort_est_025 = 0,
                  mort_est_975 = 0))

# Compute bar widths dynamically (assuming evenly spaced years)
bar_width <- 0.5  # Adjust this value to change bar spacing

# Overlay bars using `rect()`
for (i in seq_along(mort_summary2$year)) {
  rect(
    xleft = as.numeric(mort_summary2$year[i]) - bar_width, 
    xright = as.numeric(mort_summary2$year[i]) + bar_width,
    ybottom = 0, 
    ytop = mort_summary2$mort_est_50[i], 
    col = adjustcolor("red", alpha.f = 0.6), 
    border = "black"
  )
}

# Add error bars for bars
segments(
  x0 = as.numeric(mort_summary2$year), 
  y0 = mort_summary2$mort_est_05, 
  x1 = as.numeric(mort_summary2$year), 
  y1 = mort_summary2$mort_est_095, 
  col = "black", 
  lwd = 2
)




#################
################# ADD LINES BACK OVER BARPLOTS
#################

# Add dashed lines for confidence intervals with step shift
# lines(mort_df$year - 0.5, mort_df$cum_mort_est_05, col = "darkgrey", lwd = 3, lty = 3, type = "s")
# lines(mort_df$year - 0.5, mort_df$cum_mort_est_095, col = "darkgrey", lwd = 3, lty = 3, type = "s")

# Add main estimate line with step shift
lines(mort_df$year - 0.5, mort_df$cum_mort_est_50, col = "black", lwd = 3, lty = 1, type = "s")







dev.off()
