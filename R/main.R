################################################################################
# Main script Functional Limits of Agreement (FLoA)
#
# FLoA are calculated using a randomized cluster bootstrap
#
# Thus far, a transformation of time series data to functional data (Fourier,
# Splines etc.) is not implemented.
#
# TODO:
#   * Implement balanced data in floa_rcb.R
#   * Simulierte Daten in einer Funktion zusammenfassen
#   * Cross validation
#   * mean oder median as estimator?
#   * Method comparisons: Against FDA (Lenhoff/Roislien) and pointwise!?
#   * See appendix Lenhoff et al.
#   * Use only synthetic data? (Leave out real world example?)
#   * Umbennung in CLoA (Continuous LoA)?
#   * quantile() function: Bias correction useful/necessary?
#   * Lenhoff (1999)
#   * Konvergenzanalyse --> "[...] the  achieved  level  of  bootstrap bands  is
# roughly  equal  to  the nominal  level  with as  few  as  25  or  so  curves.
################################################################################

# library(fda)
# library(funData)

rm(list = ls())

dir.script <- "C:/Users/Daniel/Desktop/tmp/floa/R"
dir.data <- "C:/Users/Daniel/Desktop/tmp/floa/R/examples"

setwd(dir.script)
source("example_data.R")
source("draw_clusters.R")
source("floa_rcb.R")
source("floa_point.R")
source("plot_loa.R")
source("get_coverage.R")
# source("fdaDelta.R")


################################### Data sets ##################################

# Wrapper function for example data sets.
#
# Function arguments:
#
# * Empirical validation data: "imu_mc"
# * Smooth, wave data (normal error, constant variance, no trend): "smooth"
# * Biased data (constant variance, no trend): "bias"
# * Non-constant variance data (normal error, no trend): "non_const_var"
# * Non-stationary data (trend, no bias) data:"non_stationary"
# * Log-normal error data (no bias,  constant variance, no trend): "log_normal"
# * Data with shock peaks (no bias, no trend): "shock"

data <- example_data(dat = "smooth", dir.data)


################################ Calculate FLoA ################################

n.boot <- 100

# Randomized Cluster Bootstrap
# ------------------------------------------------------------------------------

# * In the first stage, n=length(subjects) random strides are sampled
# from all strides (with replacement). Strides are selected from the entire set
# of curves (NOT! one curve per subject).
# * The process is repeated n.boot times.
# From the resulting distribution, percentiles (2.5%, 50%, 97.5%) are calculated.

# Function returns percentiles (2.5%, 50%, 97.5%)
floa.boot.percentiles.intrp <- floa_rcb(data, n.boot, plt = TRUE)

# Pointwise LoA
# ------------------------------------------------------------------------------
floa.point <- floa_point(data)

# Method Roislien et al.
# ------------------------------------------------------------------------------

# Method Lenhoff
# ------------------------------------------------------------------------------


################################### Plot data ##################################

floa.rcb <- data.frame(t(floa.boot.percentiles.intrp))
floa.point <- data.frame(t(floa.point))

plot_loa(data, floa.rcb) # Select floa method


#################################### Coverage ##################################

# Calculate coverage (entire curves within the percentile boundaries)
coverage <- get_coverage(data, t(floa.rcb)) # Select floa method

print(coverage)


############################### Cross validation ###############################

# Leave-one (subject) out method to estimate the achieved coverage
# See Lenhoff (1999)
#
# Specify method calculated in crossval_coverage()
#   * method = "all"
#   * method = "floa.rcb"
#   * method = "floa.point"
#
# Output:
#   * Coverage levels [%] and SEM across n=length(subjectID) iterations
#   * Standard error of estimate
# ------------------------------------------------------------------------------

cover.cross <- crossval_coverage(data, method)

crossval_coverage <- function (data, method) {

  n.subj <- unique(data$subjectID)

  cover.distro  <- c()

  for (i in n.subj) {

    data.subset <- subset(data, subjectID != i)

    if (method == "floa.rcb" | method == "all") {

      floa.boot.percentiles.intrp <- floa_rcb(data.subset, n.boot, plt = TRUE)
      floa.rcb <- data.frame(t(floa.boot.percentiles.intrp))

      cover.distro <- c(cover.distro,
                        get_coverage(data, t(floa.rcb)) # Select floa method
      )
    }

    if (method == "floa.point"  | method == "all") {

      floa.point <- floa_point(data)
      floa.point <- data.frame(t(floa.point))

      cover.distro <- c(cover.distro,
                        get_coverage(data, t(floa.rcb)) # Select floa method
      )
    }
  }
}


# Boxplot of coverage distribution
boxplot(cover.distro)
