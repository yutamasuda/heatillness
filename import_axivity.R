# Import activity (Axivity) data

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
pacman::p_load(GGIR, zoo, data.table)

# Set timezone
# Timezone: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones#List
# CC   Coordinates   Timezone        Offset   DST Offset
# ID   −0507+11924   Asia/Makassar   +08:00       +08:00
sampling_tz <- 'Asia/Makassar'

# ---------------------------------------------------
# Import data using g.shell.GGIR to generate reports
# ---------------------------------------------------

# See: https://cran.r-project.org/web/packages/GGIR/vignettes/GGIR.html

# Set paths
#data_path <- 'data/Axivity'
#output_data_path <- 'reports'

# Create output folder if it does not already exist
#dir.create(file.path(output_data_path), showWarnings=FALSE, recursive=TRUE)

# Perform automated preliminary analysis, with a 1 second "short epoch length"
#shell_result <- g.shell.GGIR(datadir = data_path, outputdir = output_data_path, 
#                             desiredtz = sampling_tz, maxdur = 1, strategy = 3, 
#                             ndayswindow = 1, windowsizes = c(1, 300, 1200))
# Note: Defaults are windowsizes=c(5, 900, 3600))

# --------------------
# Import Axivity data
# --------------------

# We will just import the timestamp and ENMO for a 1 second short epoch length"
# and write this to a separate CSV for each input CWA, plus a combined CSV.

# Load the other packages, installing as needed
pacman::p_load(GGIR, data.table)

# Set paths
data_path <- 'data/Axivity'
output_data_path <- 'output_data/Axivity'

# Create output folder(s) if necessary
dir.create(file.path(output_data_path), 
           showWarnings=FALSE, recursive=TRUE)
dir.create(file.path(output_data_path, 'csv'), 
           showWarnings=FALSE, recursive=TRUE)
dir.create(file.path(output_data_path, 'png'), 
           showWarnings=FALSE, recursive=TRUE)

# Get list of input filenames
file_names <- dir(data_path, pattern ='.cwa')

# Define a function to read an Axivity CWA file to get "short epoch" metadata
get_axivity_data <- function(file_name) {
    # Set file paths
    file_path <- file.path(data_path, file_name)
    csv_file_path <- file.path(output_data_path, 'csv',
                               gsub('\\.cwa$', '\\.csv', file_name))
    png_file_path <- file.path(output_data_path, 'png', 
                               gsub('\\.cwa$', '\\.png', file_name))
    
    # Get meta-data, with a 1 second "short epoch length"
    meta_data <- g.getmeta(file_path, desiredtz = sampling_tz, 
                           ndayswindow = 1, windowsizes = c(1, 300, 1200))
    
    # Inspect data file
    insp_data <- g.inspectfile(file_path)
    
    # Autocalibrate data file
    cali_data <- g.calibrate(file_path, windowsizes = c(1, 300, 1200))
    
    # Impute
    impu_data <- g.impute(meta_data, insp_data, maxdur = 1, strategy = 3, 
                          ndayswindow = 1, desiredtz = sampling_tz)

    # Import data (timestamp and ENMO) into data frame
    df <- data.frame(impu_data['metashort'])
    names(df) <- gsub('metashort\\.', '', names(df))
    
    # Convert datestamp from character to POSIX format
    df$timestamp <- as.POSIXct(strptime(df$timestamp, 
                                        "%Y-%m-%dT%H:%M:%S%z",
                                        tz = sampling_tz))
    
    # Turn off scientific notation for ENMO column
    df$ENMO <-format(df$ENMO, scientific = FALSE) 

    # Plot
    png(filename=png_file_path)
    #g.plot(impu_data, meta_data, insp_data, durplot=1)
    plot(df, type='l', 
         main=paste('Activity (ENMO) [Axivity]', file_name), 
         xlab='Timestamp', ylab='Activity (ENMO)')
    dev.off()
    
    # Write to CSV
    write.csv(df, csv_file_path, row.names = FALSE)
    
    # Add column for 'noquestioner' and combine into single data frame
    df$noquestioner <- as.character(gsub('\\.cwa$', '', file_name))
    df
}

# Read, clean, write, and combine Axivity data into a single data table
axivity_data <- rbindlist(as.data.table(sapply(file_names, 
                                 function(x) get_axivity_data(x))))

# Do final clean-up on this single Axivity data table
names(axivity_data) <- c('timestamp', 'ENMO', 'noquestioner')
#axivity_data$ENMO <- as.numeric(axivity_data$ENMO)

# Write this single Axivity data table to a single CSV file
write.csv(axivity_data, file.path(output_data_path, 'axivity_data.csv'), 
          row.names = FALSE)

