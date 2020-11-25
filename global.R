#
# Loads everything to the global R session environment.
# It will be called first and only once for each session. 
#
# We will define here the functions that never change (i.e. draw map). These functions will collect the parameters
# and the data from the server file.
#

# Load the libraries -----------------------------------------------------------
library(rgdal)
require(rgeos)

# Read in data files -----------------------------------------------------------

# load geo files
st_geo <- readRDS("gadm36_MMR_1_sp.rds")
tw_geo <- readRDS("gadm36_MMR_3_sp.rds")

nl_layer <- readOGR(dsn = ".", layer = "nl_layer")
#cn_layer <- readOGR(dsn = ".", layer = "cn_layer")

# load dataframes
load(file="cn_data.rda")
load(file="cn_conflicts_list.rda")
load(file="nl_data.rda")


# Set variables ----------------------------------------------------------------

period = "2018-05-01"

# Functions to display correct UI options --------------------------------------




# Functions to get correct dataframes based on parameters ----------------------





# Functions for the map --------------------------------------------------------

draw_base_map <- function() {
  
  tm_shape(st_layer) +
    tm_borders(col="black", lwd=4)
}

update_region_map <- function(map, region) {
  
  st_layer = st_geo[st_geo@data$NAME_1==region,]
  tw_layer = tw_geo[tw_geo@data$NAME_1==region,]
  
  nl_filter = nl_data[which(nl_data$state==region),]
  tw_filter = tw_geo[which(tw_geo@data$NAME_1==region),]
  nl_layer = tw_filter
  nl_layer@data$radiance = nl_filter[which(nl_filter$date==period),]$radiance
  
  
  tm_shape(nl_layer) + 
    tm_fill(col = "radiance",
            n = 15,
            palette = "cividis",
            contrast = c(0,1)) +
    tm_legend(show=TRUE) + 
    tm_layout("Conflicts Impact",
              legend.title.size = 1,
              legend.text.size = 0.6,
              legend.outside.position = c("left","bottom"),
              legend.bg.color = "white",
              legend.bg.alpha = 1,
              legend.outside = TRUE) +
    tm_layout(frame=FALSE)+
  tm_shape(tw_layer) +
    tm_borders(col = "white", 
               lwd = 1, 
               alpha = 0.5)+
  tm_shape(st_layer) +
    tm_borders(col="black", lwd=2)

}

# Functions for the exploration plot -------------------------------------------




# Functions for the prediction plot --------------------------------------------



