# Import core body temperature (Questemp) data

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
pacman::p_load(data.table, stringr, chron, lubridate, gdata)

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

# ---------------------
# Import Questemp data
# ---------------------

# We will import the XLS files as provided to us, converting the timestamp
# format, omiting extra columns and then write this to a separate CSV for each 
# input CSV, plus a combined CSV. We will also write out a plot file for each.

# There are several different formats for the input files due to variations
# in the number of columns and presence or absence of a title row above the 
# column headings row. Also, there may be values reported for eartemp which 
# are ten times higher than they should be due to a missing decimal point. 
# Further, the the eartemp values may have a comma instead of the decimal 
# point, so we make this substition if necessary. Lastly, the reported 
# timestamps are not trusted, so alternate timestamps will be generated using
# a separate file containing start times, end times, and durations. The last
# observation will be assumed to occur at the correct end time as reported in 
# the separate file and all other timestamps will be based on this end time 
# and a 10 second interval between observations assumed to be chronologically 
# sequential. Thus, if the end time is t, then observation n occurs at time t
# and observation n-1 occurs at time t-10s, n-2 at t-20s, n-3 at t-30s, etc.

# Set paths
data_path <- 'data/Questemp'
output_data_path <- 'output_data/Questemp'

# Create output folder(s) if necessary
dir.create(file.path(output_data_path), 
           showWarnings=FALSE, recursive=TRUE)
dir.create(file.path(output_data_path, 'csv'), 
           showWarnings=FALSE, recursive=TRUE)
dir.create(file.path(output_data_path, 'png'), 
           showWarnings=FALSE, recursive=TRUE)

# Get list of input filenames
file_names <- dir(data_path, pattern ='.xls')

# Define a function to read a Questemp XLS file to get core body temp data
get_questemp_data <- function(file_name) {
    # Set file paths
    file_path <- file.path(data_path, file_name)
    csv_file_path <- file.path(output_data_path, 'csv', 
                               gsub('\\.xls$', '\\.csv', file_name))
    png_file_path <- file.path(output_data_path, 'png', 
                               gsub('\\.xls$', '\\.png', file_name))
    
    # Import raw data file, checking first 3 rows to determine table format
    df = read.xls(file_path, sheet = 1, colClasses = 'character',
                  header = FALSE, stringsAsFactors=FALSE, nrows=3)
    if (df[1, 1] == 'Logged Data Chart') {
        df = read.xls(file_path, sheet = 1, colClasses = 'character',
                      header = FALSE, stringsAsFactors=FALSE, skip=3)[1:2]
    } else if (df[1, 1] == 'Timestamp') {
        df = read.xls(file_path, sheet = 1, colClasses = 'character',
                      header = FALSE, stringsAsFactors=FALSE, skip=1)[1:2]
    } else if (df[2, 1] == 'Village Id') {
        df = read.xls(file_path, sheet = 1, colClasses = 'character',
                      header = FALSE, stringsAsFactors=FALSE, skip=3)[4:5]
    } else {
        df = read.xls(file_path, sheet = 1, colClasses = 'character',
                      header = FALSE, stringsAsFactors=FALSE, skip=1)[4:5]
    }
    
    # Replace variable names
    names(df) <- c('timestamp', 'eartemp')

    # Use the filename to find the "noquestioner" id    
    id <- as.character(gsub('\\.xls$', '', file_name))
    
    # Use the first oberservation's timestamp to find the sampling date
    questdate <- as.Date(df$timestamp, '%m/%d/%Y %H:%M:%S')[1]
    
    # Use the end time as reported in the "start and end time.csv" file
    endtime <- start_end[noquestioner == id, endtime]
    
    # Calculate number of seconds until the "end time" for each observation
    # assuming a chronological sequence of observations at 10 second interval
    time_offset <- seq(nrow(df)*10-10, 0, -10) 
    
    # Create POSIX timestamp based on sampling date and reported end time
    df$timestamp <- as.POSIXct(
        strptime(paste(questdate, endtime), 
                 "%Y-%m-%d %H:%M:%S", tz = sampling_tz) - time_offset)
    
    # Replace commas in "eartemp" with decimal points and convert to number
    df$eartemp <- as.numeric(gsub(',', '\\.', df$eartemp))
    
    # Correct for temperatures off by a factor of 10, ignoring NA values
    df$eartemp <- sapply(df$eartemp, function(x)  {
        if (!is.na(x) & x > 100) x/10 else x })

    # Plot
    png(filename=png_file_path)
    plot(df, type='l', 
         main=paste('Core Body Temp [Questemp]', file_name), 
         xlab='Timestamp', ylab='Core Body Temp (°C)')
    dev.off()
    
    # Write to CSV
    write.csv(df, csv_file_path, row.names = FALSE)
    
    # Add column for 'noquestioner' and combine into single data frame
    df$noquestioner <- id
    df
}

# Read, clean, write, and combine Questemp data into a single data table
questemp_data <- rbindlist(as.data.table(
    sapply(file_names, function(x) get_questemp_data(x))))

# Do final clean-up on this single Questemp data table
names(questemp_data) <- c('timestamp', 'eartemp', 'noquestioner')

# Write this single Questemp data table to a single CSV file
write.csv(questemp_data, file.path(output_data_path, 'questemp_data.csv'), 
          row.names = FALSE)

