#
# Loads everything to the global R session environment.
# It will be called first and only once for each session. 
#
# We will define here the functions that never change (i.e. draw map). These functions will collect the parameters
# and the data from the server file.
#

### Load the libraries ---------------------------------------------------------
library(rgdal)
require(rgeos)

### Read in data files ---------------------------------------------------------

# load geo files
co_geo <- readRDS("gadm36_MMR_0_sp.rds")
st_geo <- readRDS("gadm36_MMR_1_sp.rds")
ds_geo <- readRDS("gadm36_MMR_2_sp.rds")
tw_geo <- readRDS("gadm36_MMR_3_sp.rds")

#nl_layer <- readOGR(dsn = ".", layer = "nl_layer")
#cn_layer <- readOGR(dsn = ".", layer = "cn_layer")

# load dataframes
load(file="cn_events.rda")
load(file="cn_events_geo.rda")
load(file="cn_conflicts_list.rda")
load(file="nl_data.rda")
load(file="nl_changes.rda")


### Set variables --------------------------------------------------------------

parameter_date = "2018-05-01"

### Functions to display correct UI options ------------------------------------




### Functions to get correct dataframes based on parameters --------------------





### Functions for the map ------------------------------------------------------

draw_base_map <- function(map, region) {
  
  tm_shape(st_layer) +
    tm_borders(col="black", lwd=4)
}

draw_NL_map <- function(map, region) {
  
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
    tm_borders(col="black", lwd=1)

}


draw_NLchanges_map <- function(map, region) {

  st_layer = st_geo[st_geo@data$NAME_1==region,]
  tw_layer = tw_geo[tw_geo@data$NAME_1==region,]
  
  nl_layer = tw_geo[which(tw_geo@data$NAME_1==region),]
  nl_layer@data$change = nl_changes[which(nl_changes$region==region),5]

  cn_layer = cn_events_geo[cn_events_geo@data$NAME_1==region,]
  
  myBreaks <- c(-1, -0.3, -0.05, 0.05, 0.3, 1)
  myLabels <- c("Strong Decrease", "Decrease", "Similar", "Increase", "Strong Increase")
  myPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3", "#6a51a3")
  
  tm_shape(nl_layer) + 
    tm_fill(col = "change",
            #style = "fixed",
            breaks = myBreaks,
            labels = myLabels,
            palette = "RdYlGn",
            contrast = c(0,1),
            zindex = 403,
            legend.show = TRUE,
            legend.z = 1) +

  tm_shape(cn_layer) +
    tm_bubbles(col = "impact_level",
             alpha = 0.5,
             palette = c("white","pink","red","black"),
             size = 0.5,
             zindex = 403) +

  tm_shape(tw_layer) +
    tm_borders(col = "grey", 
             lwd = 1, 
             alpha = 0.8,
             zindex = 401) +
  
  tm_shape(st_layer) +
    tm_borders(col="black", 
               lwd=1,
               alpha = 0.8,
               zindex = 402) +
  
  tm_legend(show=TRUE) +
  tm_layout(frame = FALSE)
  
}

addLayer_map <- function(map, layer, region, session) {
 
  # layer will be used to "case" different types of info
  cn_layer = cn_events_geo[cn_events_geo@data$NAME_1==region,]
  
  tmapProxy(map, session, {

    # remove layers
    tm_remove_layer(403) + # zindex = 403 is the layer id 
    
    # add layers
    tm_shape(cn_layer) +
      tm_bubbles(col = "impact_level",
                 alpha = 0.5,
                 palette = c("white","pink","red","black"),
                 size = 0.5,
                 zindex = 403) +
    tm_legend(show=TRUE)
  }) 
}


### Functions for the exploration plot -----------------------------------------




### Functions for the prediction plot ------------------------------------------



