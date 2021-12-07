# Main script prediction bands
rm(list = ls())

library(ggplot2)

dir.script <- "~/floa/R"
dir.data <- "~/floa/R/examples"

setwd(dir.script)

source("example_data.R")
source("fdaDelta.R")
source("pick_subwise_curves.R")
source("floa_boot.R")
source("floa_point.R")
source("floa_roislien.R")
source("plot_loa.R")
source("get_coverage_fraction.R")
source("get_coverage_singlecurve.R")
source("get_coverage_singlecurve_fraction.R")
source("singlecurve_coverage.R")
source("singlecurve_coverage_fraction.R")
source("estimate_uncertainty_loa.R")

# ********************************* Data sets **********************************
# Wrapper function for example data sets. Function arguments:
#
# * Real world empirical validation data: "imu_mc"
# * Realistic looking, smooth, wave data (normal error, no trend): "smooth_realistic"
# * Heteroscedastic errors (no trend): "heteroscedastic"
# * Data with non-gaussian (Weibull distributed) error (no trend): "non_gaussian"
# * Phase shifted data (x-axis direction): "shift"
data <- example_data(dat = "shift", dir.data)

# Plot original data --------------------------------------------------------------------
data <- example_data(dat = "smooth_realistic", dir.data)
data.single.mc <- subset(data, device == "MC")
data.single.imu <- subset(data, device == "IMU")

PLOT.1 <- ggplot(data = data.single.mc, aes(x = frame, y = value, group = strideID)) +
  geom_line() +
  scale_color_grey(start = 0.6, end = 0.9) +
  geom_line(data = data.single.imu, aes(x = frame, y = value, group = strideID, colour = as.factor(subjectID)), alpha = 0.3) +
  labs(x = "Time-normalized signal [%]", y = "Angle [°]") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "none")


data <- example_data(dat = "non_gaussian", dir.data)
data.single.mc <- subset(data, device == "MC")
data.single.imu <- subset(data, device == "IMU")

PLOT.2 <- ggplot(data = data.single.mc, aes(x = frame, y = value, group = strideID)) +
  geom_line() +
  scale_color_grey(start = 0.6, end = 0.9) +
  geom_line(data = data.single.imu, aes(x = frame, y = value, group = strideID, colour = as.factor(subjectID)), alpha = 0.3) +
  labs(x = "Time-normalized signal [%]", y = "Angle [°]") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "none")


data <- example_data(dat = "shift", dir.data)
data.single.mc <- subset(data, device == "MC")
data.single.imu <- subset(data, device == "IMU")

PLOT.3 <- ggplot(data = data.single.mc, aes(x = frame, y = value, group = strideID)) +
  geom_line() +
  scale_color_grey(start = 0.6, end = 0.9) +
  geom_line(data = data.single.imu, aes(x = frame, y = value, group = strideID, colour = as.factor(subjectID)), alpha = 0.3) +
  labs(x = "Time-normalized signal [%]", y = "Angle [°]") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "none")


data <- example_data(dat = "imu_mc", dir.data)
data.single.mc <- subset(data, device == "MC")
data.single.imu <- subset(data, device == "IMU")

PLOT.4 <- ggplot(data = data.single.mc, aes(x = frame, y = value, group = strideID)) +
  geom_line() +
  scale_color_grey(start = 0.6, end = 0.9) +
  geom_line(data = data.single.imu, aes(x = frame, y = value, group = strideID, colour = as.factor(subjectID)), alpha = 0.3) +
  labs(x = "Time-normalized signal [%]", y = "Angle [°]") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "none")


# Create plot matrix
PLOT <- ggpubr::ggarrange(PLOT.1, PLOT.2, PLOT.3, PLOT.4,
                          labels = c("A", "B", "C", "D"),
                          ncol = 2,
                          nrow = 2)
PLOT

# ggsave("~/Nextcloud/project-fab-forschung/Publikationen/FLOA/tex/Grafiken/original_curves.png", device = "png", dpi = 300)














# ***************************** Prediction bands *******************************
n.boot <- 5

floa.point     <- floa_point(data)
floa.roislien  <- floa_roislien(data)
floa.boot.pred <- floa_boot(data,
                            k_reihe = 50,
                            n.boot = n.boot,
                            band = "prediction",
                            cp.begin = 0,
                            alpha = 0.05,
                            iid = FALSE)

floa.boot.conf  <- floa_boot(data,
                             k_reihe = 50,
                             n.boot = n.boot,
                             band = "confidence",
                             cp.begin = 0,
                             alpha = 0.05)


plot_loa(data, floa.point, floa.roislien, floa.boot.pred, ylim = c(-5, 5))
plot_loa(data, floa.point, floa.roislien, floa.boot.conf, ylim = c(-15, 15))


# ********************************* Coverage ***********************************
# Calculate coverage (entire curves within the limits of agreement)
coverage <- get_coverage(data, floa.roislien) # Select floa method: floa or floa.point
print(coverage)


# ***************************** Cross validation *******************************
# Currently, two different methods for estimating the uncertainty in the achieved
# coverage are implemented:
# 1. Leave all curves of a single subject out
# 2. Leave-one single curve out
#
# Currently, different versions of the sampling process in draw_clusters() (nested
# in floa_rcb()) are implemented:
#
# v1 : n = length(subjects) random strides from all strides
# v2 : One random stride per subject
# v3 : Fetch a SINGLE random stride from all strides
# v4 : Roislien approach (Get one random stride from each subject ONCE and boot-
#      strap the resulting sample (of length (n=length(subjects))
# v5 : Pointwise B & A Limits of Agreement
#
# Output:
#   * Coverage levels [%] across n=length(subjectID) iterations
# ******************************************************************************
# 1. Leave-one subject out
cover.cross.subject <- crossval_coverage(data, n.boot)

# Display cross validation coverages (entire curve covered) across methods
plot_cov_ver(cover.cross.subject)

# Mean fraction of the curve that is covered) across methods
cover.cross.fraction.subject <- crossval_coverage_fraction(data, n.boot)

plot_cov_ver(cover.cross.fraction.subject)

# 2. Leave-one curve out
cover.cross.singlecurve <- singlecurve_coverage(data, n.boot)

calculate_percent <- function(data) {
  # Calculate percent from counts
  percentage <- (sum(data) / length(data))
  percentage <- round(percentage, digits = 2)
}
covered.curves.percent <- apply(cover.cross.singlecurve, 2, calculate_percent)

cover.cross.fraction.singlecurve <- singlecurve_coverage_fraction(data, n.boot)
plot_cov_ver(cover.cross.fraction.singlecurve)



# Estimate the uncertainty across several iterations
# Returns results for all implemented methods
estimate_uncertainty_loa(data, n.rep, n.boot)


# Average distance to the outer point of the curve set
dist.all <- data.frame(distance_2_floa(data, floa)$upr.dist,
                       distance_2_floa(data, floa)$lwr.dist,
                       distance_2_floa(data, floa.point)$upr.dist,
                       distance_2_floa(data, floa.point)$lwr.dist,
                       distance_2_floa(data, floa.roislien)$upr.dist,
                       distance_2_floa(data, floa.roislien)$lwr.dist
                       )

colnames(dist.all) <- c("floa.up", "floa.lw", "point.up", "point.lw", "roislien.up", "roislien.lw")

boxplot(dist.all, ylab = "floa boundary - curve maximum")

