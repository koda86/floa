# Main script Functional Limits of Agremment (FLoA)

rm(list=ls())

# Transform data to long format
# ------------------------------------------------------------------------------
data.long <- RawData_Long(n.subjects) # Data are interpolated to 101 data points and aggregated into joints and sides
