################################################################################
# Main script Functional Limits of Agreement (FLoA)
#
# FLoA are calculated using a randomized cluster bootstrap
#
# Thus far, a transformation of time series data to functional data (Fourier,
# Splines etc.) is not implemented.
#
# TODO:
#     * Implement balanced data in floa_rcb.R
#     * Simulierte Daten in einer Funktion zusammenfassen
#     * Cross validation
#     * mean oder median as estimator?
#     * Method comparisons: Against FDA (Lenhoff/Roislien) and/or pointwise!?
#     * See appendix Lenhoff et al.
#     * Use only synthetic data? (Leave out real world example?)
#     * Umbennung in CLoA (Continuous LoA)?
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


###################################### FLoA #####################################

n.boot <- 100

# Randomized Cluster Bootstrap -------------------------------------------------
#
# Function returns percentiles (2.5%, 50%, 97.5%)
floa.boot.percentiles.intrp <- floa_rcb(data, n.boot, plt = TRUE)

# Method Roislien et al. -------------------------------------------------------

# Method Lenhoff ---------------------------------------------------------------

# Pointwise LoA ----------------------------------------------------------------


################################### Plot data ##################################

# Prepare data for ggploting -------------------------------------------------
#
# The following lines of the script are exclusively for plotting the data.
# All calculations are done above.
# ----------------------------------------------------------------------------

# "imu_mc" NEEDS BALANCED DATA!!

device1 <- data.frame(subset(data, device == "IMU")$value)
device2 <- data.frame(subset(data, device == "MC")$value)

device.diff <- device1 - device2

colnames(device.diff)[1] <- "value"

device.diff$frame <- seq(0, 100)
n.frames <- length(unique(data$frame))
n.strides <- length(unique(data$strideID))
n.subjects <- length(unique(data$subjectID))
strides.per.subject <- length(unique(data$strideID)) / n.subjects
device.diff$strideID <- as.factor(rep(1:n.strides, each = 101)) # as.factor(seq(0, n.strides, by = n.frames)) # as.factor(rep(seq(1, strides.per.subject), each = 101))
device.diff$subjectID <- as.factor(rep(1:n.subjects, each = strides.per.subject * n.frames))


floa <- data.frame(t(floa.boot.percentiles.intrp))

# For line graphs, the data points must be grouped so that it knows which points to connect.
# In this case, it is simple -- all points should be connected, so group=1.
# When more variables are used and multiple lines are drawn, the grouping for lines is usually done by variable.
PLOT.DIFF <- ggplot(data = device.diff, aes(x = frame, y = value, color = subjectID, group = strideID)) +
  geom_line() +
  scale_color_grey(start = 0.8, end = 0.2) +
  geom_line(data = floa,
            aes(x = seq (0,100), y = X1, col = "red", group = 1),
            linetype = "solid",
            size = 3,
            colour = "red") +
  geom_line(data = floa,
            aes(x = seq (0,100), y = X2, col = "red", group = 1),
            linetype = "dotted",
            size = 3,
            colour = "red") +
  geom_line(data = floa,
            aes(x = seq (0,100), y = X3, col = "red", group = 1),
            linetype = "solid",
            size = 3,
            colour = "red") +
  scale_y_continuous(limits = c(min(device.diff$value), max(device.diff$value))) +
  labs(x = "Time-normalized signal [%]", y = "Difference") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 22),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 22),
        legend.position = "none")

PLOT.DIFF


#################################### Coverage ##################################

# Calculate coverage (entire curves within the percentile boundaries) ----------
coverage <- get_coverage(floa.boot.percentiles.intrp)

print(coverage)


############################### Cross validation ###############################

# Leave-one (subject) out method to estimate the achieved coverage
# See Lenhoff
#
# Output:
#     * Coverage level (%)
#     * Standard error of estimate
#     * "[...] the  achieved  level  of  bootstrap bands  is  roughly  equal  to  the
# nominal  level  with as  few  as  25  or  so  curves. --> Konvergenzanalyse



