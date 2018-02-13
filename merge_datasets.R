# Merge all sensor datasets

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
pacman::p_load(data.table, stringr, chron, lubridate)

# Set timezone
# Timezone: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones#List
# CC   Coordinates   Timezone        Offset   DST Offset
# ID   −0507+11924   Asia/Makassar   +08:00       +08:00
sampling_tz <- 'Asia/Makassar'

# -----------------------------------
# Import active, start and end times
# -----------------------------------

col_classes = c('character', 'numeric', 'character', 'character')
start_end <- fread('data/start and end time.csv', colClasses = col_classes)
start_end$noquestioner <- str_pad(start_end$noquestioner, 6, pad = '0')
start_end$starttime = times(start_end$starttime_str)
start_end$endtime = times(start_end$endtime_str)
start_end[, c('starttime_str', 'endtime_str')] <- NULL

# --------------
# Read datasets
# --------------

axivity_data <- fread('output_data/Axivity/axivity_data.csv')
wahoo_data <- fread('output_data/Heart rate/Wahoo/wahoo_data.csv')
polar_data <- fread('output_data/Heart rate/Polar/polar_data.csv')
questemp_data <- fread('output_data/Questemp/questemp_data.csv')

# --------------------
# Create key variable
# --------------------

# To merge the datasets we will need to create a common ten second time 
# interval variable. This will be combined with the "noquestioner" ID to create
# key for merging in a later step.

# To create the common ten second time interval variable:
#
# For timestsamps with a 1 second interval, find the end time of the ten second 
# interval (00, 10, 20, 30, 40, 50) that would include it. That is, for a time 
# of 00:01 to 00:10, use an interval end time of 00:10, or for a time of 00:51 
# to 00:59 use an interval end time of 01:00. For timestamps with a ten second 
# interval, round to the nearest ten second interval (00, 10, 20, 30, 40, 50).
# The latter case occurs with Questemp data only, and the only values in our 
# dataset to be rounded will be 09, 19, 29, 39, 49, and 59. For example, see:
#   table(format(questemp_data$timestamp, "%S"))

str_to_datetime <- function(timestamp_str) {
    as.POSIXct(strptime(
        timestamp_str, '%Y-%m-%d %H:%M:%S', tz = sampling_tz))
}

round_to_interval <- function(timestamp) {
    format(strptime('1970-01-01', '%Y-%m-%d', tz = 'UTC') + 
               round(as.numeric(timestamp)/10)*10, '%H:%M:%S', tz = sampling_tz)
}

ceiling_to_interval <- function(timestamp) {
    format(strptime('1970-01-01', '%Y-%m-%d', tz = 'UTC') + 
               ceiling(as.numeric(timestamp)/10)*10, '%H:%M:%S', tz = sampling_tz)
}

axivity_data$timestamp <- str_to_datetime(axivity_data$timestamp)
axivity_data$interval <- ceiling_to_interval(axivity_data$timestamp)

wahoo_data$timestamp <- str_to_datetime(wahoo_data$timestamp)
wahoo_data$interval <- ceiling_to_interval(wahoo_data$timestamp)
wahoo_data$hr_sensor <- 'Wahoo'

polar_data$timestamp <- str_to_datetime(polar_data$timestamp)
polar_data$interval <- ceiling_to_interval(polar_data$timestamp)
polar_data$hr_sensor <- 'Polar'

questemp_data$timestamp <- str_to_datetime(questemp_data$timestamp)
questemp_data$interval <- round_to_interval(questemp_data$timestamp)

# ------
# Merge
# ------

# Merge datasets on a key of "noquestioner" ID variable and a time variable
sensor_data <- merge(axivity_data, rbindlist(list(wahoo_data, polar_data)), 
                     by=c('noquestioner', 'timestamp', 'interval'), all = TRUE)
sensor_data <- merge(sensor_data, questemp_data[, -c('timestamp')], 
                     by = c('noquestioner', 'interval'), all = TRUE)

# -----
# Trim
# -----

# Trim off data for observations which occur earlier than 5 minutes before
# the reported "starttime" and also those which occured later than 5 minutes 
# after the reported "endtime".

# Create time variable from timestamp and fill time NAs with interval value
sensor_data$time <- times(format(sensor_data$timestamp, "%H:%M:%S"))
sensor_data$interval <- times(sensor_data$interval)
sensor_data[is.na(time), time := interval]

# Merge sensor data with starttime and endtime values to use for subsetting
sensor_data <- merge(sensor_data, 
                     start_end[, c('starttime', 'endtime', 'noquestioner')], 
                     by='noquestioner')

# Subset by comparing time variable with starttime and endtime and removing 
# any observations not falling between those, allowing for 5 minute padding
sensor_data <- sensor_data[time >= starttime - times('00:05:00') 
                           & time <= endtime + times('00:05:00')]

# Remove extra columns
sensor_data <- sensor_data[, -c('timestamp', 'starttime', 'endtime')]

# Write this single data table to a single CSV file
output_data_path <- 'output_data'
write.csv(sensor_data, file.path(output_data_path, 'sensor_data.csv'), 
          row.names = FALSE)
