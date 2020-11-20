#
# Loads everything to the global R session environment.
# It will be called first and once for each session. 
#

library(rgdal)

# Load dataframes
load(file="cn_data.rda")

# Load maps layers
nl_layer <- readOGR(dsn = ".", layer = "nl_layer")
#ds_layer <- readOGR(dsn = ".", layer = "ds_layer")
#cn_layer <- readOGR(dsn = ".", layer = "cn_layer")
#st_layer <- readOGR(dsn = ".", layer = "st_layer")
