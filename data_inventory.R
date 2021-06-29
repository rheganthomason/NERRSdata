## NERRs data inventory

# Install package - comment aka hastag out this once the package is installed
install.packages('SWMPr')

# Library packages
library(SWMPr)

# get list of site codes 
codes <- SWMPr::site_codes()

View(codes)

# more to come later