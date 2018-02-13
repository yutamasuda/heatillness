# Import heart rate (Wahoo and Polar) data

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
pacman::p_load(data.table, chron, lubridate)

# Set timezone
# Timezone: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones#List
# CC   Coordinates   Timezone        Offset   DST Offset
# ID   −0507+11924   Asia/Makassar   +08:00       +08:00
sampling_tz <- 'Asia/Makassar'

# ------------------
# Import Wahoo data
# ------------------

# We will just import the CSVs as provided to us, converting the timestamp
# format, omiting extra columns and then write this to a separate CSV for each 
# input CSV, plus a combined CSV. We will also write out a plot file for each.

# There are two formats for Wahoo files, a primary and an alternate format.
# The primary format is just a standard CSV format with 1 header line.
# For the alternate format, the observations start on line 21 with a time of 
# "1", lines 1 through 19 are metadata and line 20 a header, the start date and
# start time are in the second line. The observations have a time starting at 
# "1" and increment by one second per obseration. To create a proper timestamp, 
# we add the start time (-1 second) to these times and combine with the date.

# Set paths
data_path <- 'data/Heart rate/Wahoo'
output_data_path <- 'output_data/Heart rate/Wahoo'

# Create output folder(s) if necessary
dir.create(file.path(output_data_path), 
           showWarnings=FALSE, recursive=TRUE)
dir.create(file.path(output_data_path, 'csv'), 
           showWarnings=FALSE, recursive=TRUE)
dir.create(file.path(output_data_path, 'png'), 
           showWarnings=FALSE, recursive=TRUE)

# Get list of input filenames
file_names <- dir(data_path, pattern ='.csv')

# Define a function to return the file name if it is of the alternate format
is_alt_format <- function(file_name) {
    file_path <- file.path(data_path, file_name)
    
    # Read the first line (the file header) as a string
    con  <- file(file_path, open = "r")
    oneline <- readLines(con, n = 1, warn = FALSE)
    close(con)
    
    # Check to see if the first 6 characters in the string is 'Device'
    teststr <- substr(oneline, 0, 6)
    if (teststr == 'Device') { return(file_name) }
}

# Get list of files of the alternate format
alt_names <- as.vector(unlist(sapply(file_names, function(x) is_alt_format(x))))

# Define a function to read a Wahoo CSV file to get heart rate data
get_wahoo_data <- function(file_name) {
    # Set file paths
    file_path <- file.path(data_path, file_name)
    csv_file_path <- file.path(output_data_path, 'csv', file_name)
    png_file_path <- file.path(output_data_path, 'png', 
                               gsub('\\.csv$', '\\.png', file_name))
    
    # Import raw data file
    df <- fread(file_path, 
                colClasses = c('Heartrate'='numeric', 'Timestamp'='character'))
    
    # Clean up data
    names(df) <- tolower(names(df))
    df <- df[, c('timestamp', 'heartrate')]
    
    # Convert datestamp from character to POSIX format
    df$timestamp <- as.POSIXct(as.numeric(as.character(df$timestamp))/1000, 
                               origin="1970-01-01", tz=sampling_tz)
    
    # Plot
    png(filename=png_file_path)
    plot(df, type='l', 
         main=paste('Heart Rate [Wahoo]', file_name), 
         xlab='Timestamp', ylab='Heart Rate (bpm)')
    dev.off()
    
    # Write to CSV
    write.csv(df, csv_file_path, row.names = FALSE)
    
    # Add column for 'noquestioner' and combine into single data frame
    df$noquestioner <- as.character(gsub('\\.csv$', '', file_name))
    df
}

# Remove file names of alternate-format files from vector of file names
file_names <- setdiff(file_names, alt_names)

# Read, clean, write, and combine Wahoo data into a single data table
wahoo_data <- rbindlist(as.data.table(
    sapply(file_names, function(x) get_wahoo_data(x))))

# Define a function to read an alternate-format Wahoo CSV file
get_alt_wahoo_data <- function(file_name) {
    # Set file paths
    file_path <- file.path(data_path, file_name)
    csv_file_path <- file.path(output_data_path, 'csv', file_name)
    png_file_path <- file.path(output_data_path, 'png', 
                               gsub('\\.csv$', '\\.png', file_name))
    
    # Import raw data file
    start_duration <- read.table(file_path, nrows = 1, skip = 1, sep=',', 
                                 stringsAsFactors = FALSE)[seq(2, 12, 2)]
    df <- fread(file_path, skip = 19)[, c(1, 12)]
    
    # Clean up data
    start_time <- with(start_duration, paste(
        paste(V2, V4, V6, sep='-'), paste(V8, V10, V12, sep=':')))
    start_duration <- data.frame(starttime=as.POSIXct(
        strptime(start_time, "%Y-%m-%d %H:%M:%S", tz = sampling_tz)),
        stringsAsFactors = FALSE)
    
    names(df) <- c('time', 'heartrate')
    id <- as.character(gsub('\\.csv$', '', file_name))
    df$timestamp <- df$time - 1 + start_duration$starttime
    df <- df[, c('timestamp', 'heartrate')]
    
    # Plot
    png(filename=png_file_path)
    plot(df, type='l', 
         main=paste('Heart Rate [Wahoo]', file_name), 
         xlab='Timestamp', ylab='Heart Rate (bpm)')
    dev.off()
    
    # Write to CSV
    write.csv(df, csv_file_path, row.names = FALSE)
    
    # Add column for 'noquestioner' and combine into single data frame
    df$noquestioner <- id
    df
}

# Process alternate-format Wahoo files, if any
if (length(alt_names) > 0) {
    # Read, clean, write, and combine alternate Wahoo data
    wahoo_alt_data <- rbindlist(as.data.table(
        sapply(alt_names, function(x) get_alt_wahoo_data(x))))
    
    # Combine all wahoo data
    wahoo_data <- rbindlist(list(wahoo_data, wahoo_alt_data))
}

# Do final clean-up on this single Wahoo data table
names(wahoo_data) <- c('timestamp', 'heartrate', 'noquestioner')

# Write this single Wahoo data table to a single CSV file
write.csv(wahoo_data, file.path(output_data_path, 'wahoo_data.csv'), 
          row.names = FALSE)

# ------------------
# Import Polar data
# ------------------

# We will just import the CSVs as provided to us, converting the timestamp
# format, omiting extra columns and then write this to a separate CSV for each 
# input CSV, plus a combined CSV. We will also write out a plot file for each.

# For these files, the observations start on line 4 with a time of "0", 
# lines 1 and 3 are headers, and line 2 contains meta-data, including date,
# start time and duration. The observations have a time starting at "0" and 
# increment by one second per obseration. We will add the start time to these
# times and combine with the date and timezone to create a proper timestamp.

# Set paths
data_path <- 'data/Heart rate/Polar'
output_data_path <- 'output_data/Heart rate/Polar'

# Create output folder(s) if necessary
dir.create(file.path(output_data_path), 
           showWarnings=FALSE, recursive=TRUE)
dir.create(file.path(output_data_path, 'csv'), 
           showWarnings=FALSE, recursive=TRUE)
dir.create(file.path(output_data_path, 'png'), 
           showWarnings=FALSE, recursive=TRUE)

# Get list of input filenames
file_names <- dir(data_path, pattern ='.csv')

# Define a function to read a Polar CSV file to get heart rate data
get_polar_data <- function(file_name) {
    # Set file paths
    file_path <- file.path(data_path, file_name)
    csv_file_path <- file.path(output_data_path, 'csv', file_name)
    png_file_path <- file.path(output_data_path, 'png', 
                               gsub('\\.csv$', '\\.png', file_name))
    
    # Import raw data file
    start_duration <- read.table(file_path, nrows = 1, skip = 1, sep=',',
                                 stringsAsFactors = FALSE)[3:5]
    df <- fread(file_path, skip = 2)[, 2:3]
    
    # Clean up data
    names(start_duration) <- c('date', 'starttime', 'duration')
    names(df) <- c('time', 'heartrate')
    id <- as.character(gsub('\\.csv$', '', file_name))
    df$time <- df$time + times(start_duration$starttime)
    df$timestamp <- as.POSIXct(strptime(
        paste0(start_duration$date, 'T', df$time, sep=''), 
        "%d-%m-%YT%H:%M:%S", tz = sampling_tz))
    df <- df[, c('timestamp', 'heartrate')]
    
    # Plot
    png(filename=png_file_path)
    plot(df, type='l', 
         main=paste('Heart Rate [Polar]', file_name), 
         xlab='Timestamp', ylab='Heart Rate (bpm)')
    dev.off()
    
    # Write to CSV
    write.csv(df, csv_file_path, row.names = FALSE)
    
    # Add column for 'noquestioner' and combine into single data frame
    df$noquestioner <- id
    df
}

# Read, clean, write, and combine Polar data into a single data table
polar_data <- rbindlist(as.data.table(sapply(file_names, 
                                             function(x) get_polar_data(x))))

# Do final clean-up on this single Polar data table
names(polar_data) <- c('timestamp', 'heartrate', 'noquestioner')

# Write this single Polar data table to a single CSV file
write.csv(polar_data, file.path(output_data_path, 'polar_data.csv'), 
          row.names = FALSE)
