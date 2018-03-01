# Calculate the rolling median for the sensor datasets

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
pacman::p_load(tidyr, zoo, tidyquant)

# --------------------------------------------------------------------------
# Functions
# --------------------------------------------------------------------------

# The zoo::rollmedian function will fail if there are any sequences of values 
# containing less than k consecutive non-NA values, where k is the window 
# size used with the rollmedian function, or if any data sequences start 
# or end with any NA values. To prevent the function from returning an error, 
# we have defined the label_span_groups and flag_na_on_ends functions. They 
# label and flag the data so that you can filter and group to prevent these 
# errors.

# --------------------
# label_span_groups()
# --------------------
# Label the groups of long-running sequences of NAs and groups of the 
# non-NA values between them, where "long-running" means the length of an 
# uninterrupted sequence of NAs is greater than the value of "maxgap". 
# Flag those sequences by storing TRUE in a column "longspan". Similarly, 
# flag sequences of non-NA values shorter than "k" (the rollmedian window 
# size), by storing TRUE in a column "shortspan". "k" should be an odd 
# integer and "maxgap" should be less than or equal to "k".

label_span_groups <- function(df, obsvar, maxgap=3, k=3) {
    is.na.rle <- rle(is.na(df[[obsvar]]))
    is.na.rle$longspan <- is.na.rle$values & is.na.rle$lengths >= maxgap
    is.na.rle$shortspan <- !is.na.rle$values & is.na.rle$lengths < k
    
    is.prev.span.long <- 0
    is.prev.span.short <- 0
    grpnum <- 0
    rownum <- 0
    for (i in 1:length(is.na.rle$lengths)) {
        if (grpnum == 0 | 
            is.na.rle$longspan[i] != is.prev.span.long | 
            is.na.rle$shortspan[i] != is.prev.span.short) {
            grpnum <- grpnum + 1
        }
        is.prev.span.long <- is.na.rle$longspan[i]
        is.prev.span.short <- is.na.rle$shortspan[i]
        
        rownums <- (rownum + 1):(rownum + is.na.rle$lengths[i])
        df[rownums, 'grpnum'] <- grpnum
        df[rownums, 'longspan'] <- is.na.rle$longspan[i]
        df[rownums, 'shortspan'] <- is.na.rle$shortspan[i]
        rownum <- rownum + is.na.rle$lengths[i]
    }
    df
}

# ------------------
# flag_na_on_ends()
# ------------------
# Flag with TRUE all observations at beginning and end of a group which are NA

flag_na_on_ends <- function(df, obsvar) {
    for (grp in unique(df$grpnum)) {
        v <- as.vector(df[df$grpnum == grp,][[obsvar]])
        lt <- length(v) - length(na.trim(v, 'left'))
        rt <- length(v) - length(na.trim(v, 'right'))
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
    # Add new columns to dataframe to store group labels and boolean flags
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

# --------------------------------------------------------------------------
# Main Routine
# --------------------------------------------------------------------------

# After reading in each dataset from the "output_data" folder, these will 
# be processed individually in the following sections, each section being 
# nearly identical, then output will be written in the final section. To 
# avoid repetition of code, it would be nice to put more of these sections
# into functions, but dplyr makes it difficult to use variables passed 
# through function parameters as column names, especially with group_by().
# We are using group_by() for this because it allows us to use tq_mutate() to 
# run rollmedian() groupwise by each "noquestioner".

# --------------
# Read datasets
# --------------

axivity_data <- read_csv('output_data/Axivity/axivity_data.csv')
wahoo_data <- read_csv('output_data/Heart rate/Wahoo/wahoo_data.csv')
polar_data <- read_csv('output_data/Heart rate/Polar/polar_data.csv')
questemp_data <- read_csv('output_data/Questemp/questemp_data.csv')

# -------------
# Axivity data
# -------------

# Define configuration variables
obsvar <- 'ENMO'
medvar <- 'ENMO_rm' 
maxgap <- 60
k <- 181

# Label and flag sequences by particpant, grouping for use with rollmedian()
axivity_data <- prep_rolling_median(axivity_data, 
                                    idvar = 'noquestioner', 
                                    obsvar = obsvar, 
                                    maxgap = maxgap, 
                                    k = k) 

# Save data that we will not be able to use for calculating medians
nomedian <- axivity_data %>%
    filter(longspan == TRUE | shortspan == TRUE | natrim == TRUE)

# Calculate the rolling median
axivity_data <- axivity_data %>% 
    filter(longspan == FALSE & shortspan == FALSE & natrim == FALSE) %>% 
    group_by(noquestioner, grpnum) %>% 
    tq_mutate(select=obsvar, mutate_fun=rollmedian, k=k, fill=c(NA),
              align=c("center"), col_rename = medvar)

# Add the excluded data back into the dataset and sort by timestamp
axivity_data <- bind_rows(list(ungroup(axivity_data), nomedian)) %>% 
    arrange(noquestioner, timestamp)

# Clean up the dataset by removing and renaming variables
axivity_data <- axivity_data %>% 
    select(-grpnum, -longspan, -shortspan, -natrim) %>% 
    rename(ENMO_raw = ENMO, ENMO = ENMO_rm)

# -----------
# Wahoo data
# -----------

# Define configuration variables
obsvar <- 'heartrate'
medvar <- 'heartrate_rm' 
maxgap <- 60
k <- 181

# Label and flag sequences by particpant, grouping for use with rollmedian()
wahoo_data <- prep_rolling_median(wahoo_data, 
                                  idvar = 'noquestioner', 
                                  obsvar = obsvar, 
                                  maxgap = maxgap, 
                                  k = k) 

# Save data that we will not be able to use for calculating medians
nomedian <- wahoo_data %>%
    filter(longspan == TRUE | shortspan == TRUE | natrim == TRUE)

# Calculate the rolling median
wahoo_data <- wahoo_data %>% 
    filter(longspan == FALSE & shortspan == FALSE & natrim == FALSE) %>% 
    group_by(noquestioner, grpnum) %>% 
    tq_mutate(select=obsvar, mutate_fun=rollmedian, k=k, fill=c(NA),
              align=c("center"), col_rename = medvar)

# Add the excluded data back into the dataset and sort by timestamp
wahoo_data <- bind_rows(list(ungroup(wahoo_data), nomedian)) %>% 
    arrange(noquestioner, timestamp)

# Clean up the dataset by removing and renaming variables
wahoo_data <- wahoo_data %>% 
    select(-grpnum, -longspan, -shortspan, -natrim) %>% 
    rename(heartrate_raw = heartrate, heartrate = heartrate_rm)

# -----------
# Polar data
# -----------

# Define configuration variables
obsvar <- 'heartrate'
medvar <- 'heartrate_rm' 
maxgap <- 60
k <- 181

# Label and flag sequences by particpant, grouping for use with rollmedian()
polar_data <- prep_rolling_median(polar_data, 
                                  idvar = 'noquestioner', 
                                  obsvar = obsvar, 
                                  maxgap = maxgap, 
                                  k = k) 

# Save data that we will not be able to use for calculating medians
nomedian <- polar_data %>%
    filter(longspan == TRUE | shortspan == TRUE | natrim == TRUE)

# Calculate the rolling median
polar_data <- polar_data %>% 
    filter(longspan == FALSE & shortspan == FALSE & natrim == FALSE) %>% 
    group_by(noquestioner, grpnum) %>% 
    tq_mutate(select=obsvar, mutate_fun=rollmedian, k=k, fill=c(NA),
              align=c("center"), col_rename = medvar)

# Add the excluded data back into the dataset and sort by timestamp
polar_data <- bind_rows(list(ungroup(polar_data), nomedian)) %>% 
    arrange(noquestioner, timestamp)

# Clean up the dataset by removing and renaming variables
polar_data <- polar_data %>% 
    select(-grpnum, -longspan, -shortspan, -natrim) %>% 
    rename(heartrate_raw = heartrate, heartrate = heartrate_rm)

# --------------
# Questemp data
# --------------

# Configuration
obsvar <- 'eartemp'
medvar <- 'eartemp_rm'
maxgap <- 6
k <- 19

# Set out-of-range values to NA
questemp_data <- questemp_data %>% 
    mutate(eartemp = ifelse(eartemp > 40 | eartemp < 32, NA, eartemp))

# Label and flag sequences by particpant, grouping for use with rollmedian()
questemp_data <- prep_rolling_median(questemp_data, 
                                     idvar = 'noquestioner', 
                                     obsvar = obsvar, 
                                     maxgap = maxgap, 
                                     k = k) 

# Save data that we will not be able to use for calculating medians
nomedian <- questemp_data %>%
    filter(longspan == TRUE | shortspan == TRUE | natrim == TRUE)

# Calculate the rolling median
questemp_data <- questemp_data %>% 
    filter(longspan == FALSE & shortspan == FALSE & natrim == FALSE) %>% 
    group_by(noquestioner, grpnum) %>% 
    tq_mutate(select=obsvar, mutate_fun=rollmedian, k=k, fill=c(NA),
              align=c("center"), col_rename = medvar)

# Add the excluded data back into the dataset and sort by timestamp
questemp_data <- bind_rows(list(ungroup(questemp_data), nomedian)) %>% 
    arrange(noquestioner, timestamp)

# Clean up the dataset by removing and renaming variables
questemp_data <- questemp_data %>% 
    select(-grpnum, -longspan, -shortspan, -natrim) %>% 
    rename(eartemp_raw = eartemp, eartemp = eartemp_rm)

# -----------------
# Save the results
# -----------------

# Write the data to CSV files
write.csv(axivity_data, 
          'output_data/Axivity/axivity_data_rollmedian.csv', 
          row.names = FALSE)
write.csv(wahoo_data, 
          'output_data/Heart rate/Wahoo/wahoo_data_rollmedian.csv', 
          row.names = FALSE)
write.csv(polar_data, 
          'output_data/Heart rate/Polar/polar_data_rollmedian.csv', 
          row.names = FALSE)
write.csv(questemp_data, 
          'output_data/Questemp/questemp_data_rollmedian.csv', 
          row.names = FALSE)
