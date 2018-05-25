# ------------------------------------------------------------------
# Compare results of R and Matlab versions of the KF Model function
# ------------------------------------------------------------------

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

# -------------------
# Get results from R
#--------------------

set.seed(1)
HR <- round(rnorm(20, 80, 20))
HR
write.table(HR, 'HR.csv', row.names = F, col.names = F, sep = ',', quote = F)

CTstart <- 37

source('kfmodel.R')
CT_r <- kf_model(HR, CTstart)

# ------------------------
# Get results from Matlab
#-------------------------

pacman::p_load_gh('renozao/RcppOctave')

o_source('kfmodel.m')
CT_m <- .CallOctave('KFModel', HR, CTstart)

# ----------------
# Compare results
# ----------------

# Compare results from running R function with running Matlab function
cat(paste('Number of matching values = '), 
    length(na.omit(match(CT_r, CT_m))), '/', length(HR) , '\n')
identical(CT_r, CT_m)
all.equal(CT_r, CT_m)

# Display results
CT_r
CT_m

# Previous command should show same results as running in Octave/Matlab:
cmd <- 'echo "source(\\"kfmodel.m\\");
              HR = csvread(\\"HR.csv\\");
              disp(sprintf(\\"%0.5f\\n\\",KFModel(HR, 37)))" | octave'
out <- system(cmd, intern = T, ignore.stdout = F, ignore.stderr = F, wait = T)
out

# Parse (character string) results and store in a numeric vector
CT_o <- as.vector(na.omit(as.numeric(out)))

# Compare results as before

setdiff(round(CT_r, 5), round(CT_o, 5))
# If results match then previous command produces "numeric(0)"

identical(round(CT_r, 5), round(CT_o, 5))
# If results match then previous command produces "TRUE"

# -------------------------
# Test with TNC study data
# -------------------------

pacman::p_load(data.table, dplyr)
pacman::p_load_gh('renozao/RcppOctave')

# Read the files containing the KF Model function
o_source('kfmodel.m')
source('kfmodel.R')

# Set the starting core body temperature
CTstart <- 37

# Read the sensor data
sensor_data <- fread('output_data/sensor_data.csv')
HR.df <- sensor_data %>% 
    select(noquestioner, heartrate, `time`) %>% na.omit() %>% unique()

# Do a quick test of the model on first 1000 rows of dataset
nobs <- 1000
CT_r <- kf_model(as.numeric(HR.df$heartrate[1:nobs]), CTstart)
CT_m <- .CallOctave('KFModel', as.numeric(HR.df$heartrate[1:nobs]), CTstart)
cat(paste('Number of matching values = '), 
    length(na.omit(match(CT_r, CT_m))), '/', nobs , '\n')

# Compare the results
identical(CT_r, CT_m)
all.equal(CT_r, CT_m)

# Run the model once for each participant
CT_r_sensor <- sapply(unique(HR.df$noquestioner), function(x) { 
    kf_model(as.numeric(HR.df[HR.df$noquestioner == x]$heartrate), CTstart)
    })
CT_m_sensor <- sapply(unique(HR.df$noquestioner), function(x) { 
    .CallOctave('KFModel', 
                as.numeric(HR.df[HR.df$noquestioner == x]$heartrate), CTstart)
    })

# Compare the results
identical(CT_r, CT_m)
all.equal(CT_r, CT_m)

# Store the results in the dataframe
for (x in unique(HR.df$noquestioner)) {
    HR.df[HR.df$noquestioner == x, 'ct.est.r'] <- CT_r_sensor[[x]]
    HR.df[HR.df$noquestioner == x, 'ct.est.m'] <- CT_m_sensor[[x]]
}

# Compare the results
identical(HR.df$ct.est.r, HR.df$ct.est.m)
cat('Size of dataset =', nrow(HR.df), 'rows\n')
cat(paste('Number of matching values = '), 
    length(na.omit(match(HR.df$ct.est.r, HR.df$ct.est.m))), 
    '/', nrow(HR.df) ,'\n')

identical(HR.df$ct.est.r, HR.df$ct.est.m)
all.equal(HR.df$ct.est.r, HR.df$ct.est.m)
