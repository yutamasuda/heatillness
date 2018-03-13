# Visualize sensor data with plots for each "noquestioner" and variable.

# Repository: https://github.com/yutamasuda/heatillness/
# License: MIT - See the LICENSE file for more details

# ------
# Setup
# ------

# Clear workspace of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
    res <- suppressWarnings(lapply(
        paste('package:', names(sessionInfo()$otherPkgs), sep = ""),
        detach,
        character.only = TRUE,
        unload = TRUE,
        force = TRUE
    ))
}

# Install pacman if needed
my_repo <- 'http://cran.r-project.org'
if (!require("pacman")) {
    install.packages("pacman", repos = my_repo)
}

# Load the other packages, installing as needed
pacman::p_load(readr, hms, stringr, dplyr, ggplot2, gridExtra)

# ----------
# Load data
# ----------

sensor_data <- read_csv('output_data/sensor_data.csv')

noqs <- read_csv('data/start and end time.csv')
noqs <- unique(as.character(as.vector(noqs$noquestioner)))
noqs <- str_pad(noqs, 6, pad = '0')

# -----------------
# Plot sensor data
# -----------------

# Create a custom theme
my_theme <- theme_bw() + theme(legend.title = element_blank())

# For each "noquestioner", create three x-y (point) plots, one for each of the 
# three dependent variables, plotting both the raw observations and also the 
# rolling median.
for (noq in noqs) {
    df <- sensor_data %>% filter(noquestioner == noq)
    
    g1 <- ggplot(df, aes(time)) + scale_x_time() +
        geom_point(aes(y = ENMO_raw, color = 'raw'), 
                   size = 0.75) +
        geom_point(aes(y = ENMO, color = 'median'), 
                   size = 1, alpha = 0.03) +
        ylab('ENMO') + ggtitle(paste('ENMO for', noq)) + my_theme
    
    g2 <- ggplot(df, aes(time)) +
        geom_point(aes(y = heartrate_raw, colour = 'raw'), 
                   size = 0.75) +
        geom_point(aes(y = heartrate, color = 'median'), 
                   size = 1, alpha = 0.03) +
        ylab('heartrate') + ggtitle(paste('Heart Rate for', noq)) + my_theme
    
    g3 <- ggplot(df, aes(time)) +
        geom_point(aes(y = eartemp_raw, color = 'raw'), 
                   size = 0.75) +
        geom_point(aes(y = eartemp, colour = 'median'), 
                   size = 1, alpha = 0.03) +
        ylab('eartemp') + ggtitle(paste('Ear Temp for', noq)) + my_theme
    
    png(file.path('output_data', 'png', paste(noq, '.png', sep = '')))
    grid.arrange(g1, g2, g3, ncol = 1)
    dev.off()
}
