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
HR <- rnorm(20, 80, 20)
write.table(HR, 'HR.csv', row.names = F, col.names = F, sep = ',', quote = F)

source('kfmodel.R')
CT_r <- kf_model(HR, 37)

# ------------------------
# Get results from Matlab
#-------------------------

pacman::p_load_gh('renozao/RcppOctave')

o_source('kfmodel.m')
CT_m <- .CallOctave('KFModel', HR, 37)

# ----------------
# Compare results
# ----------------

# Compare results from running R function with running Matlab function

setdiff(CT_r, CT_m)
# If results match then previous command produces "numeric(0)"

identical(CT_r, CT_m)
# If results match then previous command produces "TRUE"

# Display results
CT_r

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

