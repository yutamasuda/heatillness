# Calculate the rolling median for the sensor datasets

# Repository: https://github.com/yutamasuda/heatillness/
# License: MIT - See the LICENSE file for more details

# Before executing this script, make sure that the import_*.R scripts  
# have already been executed without error.

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
pacman::p_load(tidyr, zoo, tidyquant)

# --------------------------------------------------------------------------
# Functions
# --------------------------------------------------------------------------

# The zoo::rollmedian function will fail if there are any sequences of values 
# containing less than k consecutive non-NA values, where k is the window 
# size used with the rollmedian function, or if any data sequences start 
# or end with any NA values. To prevent the function from returning an error, 
# we have defined the label_span_groups() and flag_na_on_ends() functions. They 
# label and flag the data so that you can filter and group to prevent these 
# errors. Two additional convenience functions, prep_rolling_median() and 
# calc_rolling_median(), are used as "wrappers" for the above two functions 
# to automate the processing of multiple input files for better code reuse.

# --------------------
# label_span_groups()
# --------------------
# Label the groups of long-running sequences of NAs and groups of the 
# non-NA values between them, where "long-running" means the length of an 
# uninterrupted sequence of NAs is greater than the value of "maxgap". 
# Flag those sequences by storing TRUE in a column "longspan". Similarly, 
# flag sequences of non-NA values shorter than "k" (the rollmedian window 
# size), by storing TRUE in a column "shortspan". "k" should be an odd integer 
# greater than 2 and "maxgap" should be a positive integer less than "k".

label_span_groups <- function(df, obsvar, maxgap, k) {
    # Find lengths of runs of NAs and non-NAs
    is.na.rle <- rle(is.na(df[[obsvar]]))
    
    # Find long spans (> maxgap) of NAs and short spans (< k) of non-NAs 
    is.na.rle$longspan <- is.na.rle$values & is.na.rle$lengths > maxgap
    is.na.rle$shortspan <- !is.na.rle$values & is.na.rle$lengths < k
    
    # Initialize loop flags and counters
    is.prev.span.long <- 0
    is.prev.span.short <- 0
    grpnum <- 0
    rownum <- 0
    
    # Label and flag groups of long, short, and usable spans
    for (i in 1:length(is.na.rle$lengths)) {
        # Start a new group if this is the first group or if we 
        # have transitioned into or out of a long or short span
        if (grpnum == 0 | 
            is.na.rle$longspan[i] != is.prev.span.long | 
            is.na.rle$shortspan[i] != is.prev.span.short) {

            # Start a new span group by incrementing group counter
            grpnum <- grpnum + 1
        }
        
        # Determine if this span group should be flagged as long or short
        is.prev.span.long <- is.na.rle$longspan[i]
        is.prev.span.short <- is.na.rle$shortspan[i]
        
        # Determine the row number sequence for this span group
        rownums <- (rownum + 1):(rownum + is.na.rle$lengths[i])
        
        # Label this span group and flag if this is a long or short span
        df[rownums, 'grpnum'] <- grpnum
        df[rownums, 'longspan'] <- is.na.rle$longspan[i]
        df[rownums, 'shortspan'] <- is.na.rle$shortspan[i]
        
        # Increment row counter to the last row number of this span group
        rownum <- rownum + is.na.rle$lengths[i]
    }
    df
}

# ------------------
# flag_na_on_ends()
# ------------------
# Flag with TRUE all NA observations at the beginning or end of a span group

flag_na_on_ends <- function(df, obsvar) {
    # For each span group, flag the leading or trailing NAs, if any
    for (grp in unique(df$grpnum)) {
        # Copy the observations to a vector
        v <- as.vector(df[df$grpnum == grp,][[obsvar]])
        
        # Compare the length of this vector to the NA-trimmed length
        lt <- length(v) - length(na.trim(v, 'left'))
        rt <- length(v) - length(na.trim(v, 'right'))
        
        # Use the length differences to flag the NAs with TRUE
        if (lt) df[df$grpnum == grp,][['natrim']][0:lt] <- TRUE
        if (rt) df[df$grpnum == grp,][['natrim']][
            (length(v) - rt + 1):length(v)] <- TRUE
    }
    df
}

# ----------------------
# prep_rolling_median()
# ----------------------
# A wrapper function for performing the labelling and flagging operations

prep_rolling_median <- function(df, idvar, obsvar, maxgap, k) {
    # Add new columns to the dataframe to store group labels and boolean flags
    df <- df %>% 
        mutate(grpnum = as.integer(NA), 
               longspan = as.logical(FALSE),
               shortspan = as.logical(FALSE),
               natrim = as.logical(FALSE))
    
    # Label span groups based on usable sequences and flag NAs on span edges
    for (id in unique(df[[idvar]])) {
        id_rows <- df[[idvar]] == id
        df[id_rows,] <- label_span_groups(df[id_rows,], obsvar, maxgap, k)
        df[id_rows,] <- flag_na_on_ends(df[id_rows,], obsvar)
    }
    df
}

# ----------------------
# calc_rolling_median()
# ----------------------
# A wrapper function for performing the rolling median calculation

calc_rolling_median <- function(infile, outfile, obsvar, medvar, rawvar, 
                                maxgap, k, lwr_lim=NA, upr_lim=NA) {
    # Load data
    df <- read_csv(infile)

    # Set out-of-range values of obsvar to NA if limits have been provided
    if (!is.na(lwr_lim)) {
        # Set values of obsvar which are lower than lower limit to NA
        df[!is.na(df[[obsvar]]) & df[[obsvar]] < lwr_lim, obsvar] <- NA
    }
    if (!is.na(upr_lim)) {
        # Set values of obsvar which are higher than upper limit to NA
        df[!is.na(df[[obsvar]]) & df[[obsvar]] > upr_lim, obsvar] <- NA
    }
    
    # Label and flag sequences by idvar, grouping for use with rollmedian
    df <- prep_rolling_median(df, idvar = 'noquestioner', obsvar = obsvar, 
                              maxgap = maxgap, k = k) 
    
    # Save data that we will not be able to use for calculating medians
    nomedian <- df %>%
        filter(longspan == TRUE | shortspan == TRUE | natrim == TRUE)
    
    # Calculate the rolling median, grouping by the idvar and span group
    df <- df %>% 
        filter(longspan == FALSE & shortspan == FALSE & natrim == FALSE) %>% 
        group_by(noquestioner, grpnum) %>% 
        tq_mutate(select=obsvar, mutate_fun=rollmedian, k=k, fill=c(NA),
                  align=c("center"), col_rename = medvar)
    
    # Add the excluded data back into the dataset and sort by timestamp
    df <- bind_rows(list(ungroup(df), nomedian)) %>% 
        arrange(noquestioner, timestamp)
    
    # Clean up the dataset by removing span label and flag variables
    df <- df %>% select(-grpnum, -longspan, -shortspan, -natrim)
    
    # Rename the orig. variable to rawvar and the smoothed variable to obsvar
    names(df)[names(df) == obsvar] <- rawvar
    names(df)[names(df) == medvar] <- obsvar
    
    # Save the results to a CSV file
    write.csv(df, outfile, row.names = FALSE)
}

# --------------------------------------------------------------------------
# Main Routine
# --------------------------------------------------------------------------

# For all datasets, the time window used for the rolling median is 3 minutes, 
# so k is set to the number of observations which would span three minutes. 

# If a run of NAs is one minute or more, then a rolling median will not be 
# calculated for that span. Therefore, the maxgap variable is set to one 
# observation less than the number of observations which span one minute.

# Enter the top-level data folder
setwd('output_data')

# Process axivity data
calc_rolling_median(infile  = 'Axivity/axivity_data.csv',
                    outfile = 'Axivity/axivity_data_rollmedian.csv',
                    obsvar  = 'ENMO',
                    medvar  = 'ENMO_rm',
                    rawvar  = 'ENMO_raw',
                    maxgap  = 59,
                    k       = 181
)

# Process wahoo data
calc_rolling_median(infile  = 'Heart rate/Wahoo/wahoo_data.csv',
                    outfile = 'Heart rate/Wahoo/wahoo_data_rollmedian.csv',
                    obsvar  = 'heartrate',
                    medvar  = 'heartrate_rm',
                    rawvar  = 'heartrate_raw',
                    maxgap  = 59,
                    k       = 181
)

# Process polar data
calc_rolling_median(infile  = 'Heart rate/Polar/polar_data.csv',
                    outfile = 'Heart rate/Polar/polar_data_rollmedian.csv',
                    obsvar  = 'heartrate',
                    medvar  = 'heartrate_rm',
                    rawvar  = 'heartrate_raw',
                    maxgap  = 59,
                    k       = 181
)

# Process questemp data
calc_rolling_median(infile  = 'Questemp/questemp_data.csv',
                    outfile = 'Questemp/questemp_data_rollmedian.csv',
                    obsvar  = 'eartemp',
                    medvar  = 'eartemp_rm',
                    rawvar  = 'eartemp_raw',
                    maxgap  = 5,
                    k       = 19,
                    lwr_lim = 32,
                    upr_lim = 40
)

# Leave the top-level data folder
setwd('..')
