# The purpose of this script is to show how the Euclidean Norm Minus One (ENMO) 
# is calculated from the raw x-, y-, and z-axis acceleration values. It is also 
# meant to show that calculating ENMO in gravitational units (g) or milli-
# gravitational units (mg) show equivalent plots, aside from the difference in 
# y-axis (ENMO) scale. Lastly, it shows that the conversion to mg can occur 
# either before or after the ENMO calculation is made, as the results match 
# either way. This follows from the distributive property of multiplication.

# Repository: https://github.com/yutamasuda/heatillness/
# License: MIT - See the LICENSE file for more details

# ------
# Setup
# ------

# Clear workspace of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) { 
    res <- suppressWarnings(
        lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
               detach, character.only=TRUE, unload=TRUE, force=TRUE)) 
}

# Install pacman if needed
my_repo <- 'http://cran.r-project.org'
if (!require("pacman")) {install.packages("pacman", repos = my_repo)}

# Load the other packages, installing as needed
pacman::p_load(GGIR, dplyr, tidyr, ggplot2)

# --------------
# Configutation
# --------------

noquestioner <- '010101'

# ------------
# Import Data
# ------------

# Import first 3000 observations
raw_data <- g.cwaread(
    file.path('data', 'Axivity', paste0(noquestioner, '.cwa')), 0, 3000)

# ------------------
# Convert Timestamp
# ------------------

# Convert timestamp from character to POSIX Date-Time format
timestamp <- as.POSIXct(raw_data$data$time, 
                        origin = '1970-01-01', 
                        tz = Sys.getenv('TZ'))

# Confirm that the first timestamp matches the start time in the header
raw_data$header$start
timestamp[1]

# ---------------
# Calculate ENMO
# ---------------

# See: https://github.com/wadpac/GGIR/blob/master/R/g.applymetrics.R

# Calculate from raw x, y, z axis data in units of g
g <- sqrt(raw_data$data$x ^ 2 + 
          raw_data$data$y ^ 2 + 
          raw_data$data$z ^ 2) - 1
g[which(g < 0)] <- 0

# Calculate from raw x, y, z axis data converted to units of mg
mg <- sqrt((1000 * raw_data$data$x) ^ 2 + 
           (1000 * raw_data$data$y) ^ 2 + 
           (1000 * raw_data$data$z) ^ 2) - 1000
mg[which(mg < 0)] <- 0

# Create a data.frame to facilitate averaging in a 1-second epoch
# and also perform a post-calculation conversion from g to mg
df <- data.frame(timestamp, g, mg)
df <- df %>% group_by(timestamp = cut(timestamp, breaks = '1 sec')) %>%
             summarize(g = mean(g), mg = mean(mg)) %>% 
             mutate(gx1000 = g * 1000)

# ------------------
# Compare ENMO (mg)
# ------------------

# Is there a difference between converting from g to mg before or 
# after calculation of ENMO? Round to ignore minor differences in
# insignificant digits due to floating-point round-off errors.
setdiff(round(df$mg, 8), round(df$gx1000, 8))

# --------
# Reshape
# --------

# Reshape data frame from wide to long format to facilitate plot facet
df.long <- df %>% gather(units, ENMO, c(g, mg, gx1000)) %>% 
    mutate(units = recode(units, gx1000 = 'g*1000'))

# -------------
# Plot Results
# -------------

# Create plot with ggplot using a 1x3 facet arrangement
plt <- suppressWarnings(
       ggplot(data=df.long, aes(x = timestamp, y = ENMO)) + 
              geom_point(size = 0.1) + 
              facet_wrap( ~ units, ncol = 1, scales = 'free') + 
              ggtitle(paste('ENMO in g, g*1000, and mg for', noquestioner)) + 
              theme(axis.ticks.x = element_blank(), 
                    axis.text.x = element_blank(), 
                    axis.title.x = element_blank()))

# Show the plot
suppressWarnings(print(plt))
