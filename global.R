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
library(ggplot2)


#for the prediction

library(dplyr) # easier data wrangling 
library(viridis) # colour blind friendly palette, works in B&W also
library(lubridate) # for easy date manipulation
library(ggExtra) # because remembering ggplot theme options is beyond me
library(tidyr) 
library(plotly)

### Read in data files ---------------------------------------------------------

# load GADM files
st_geo <- readRDS("gadm36_MMR_1_sp.rds")
tw_geo <- readRDS("gadm36_MMR_3_sp.rds")

# load dataframes
load(file="cn_conflicts.rda")
load(file="cn_conflicts_list.rda")
load(file="cn_events.rda")
load(file="cn_events_geo.rda")
load(file="cn_locations.rda")
load(file="cn_events_explore.rda")
load(file="nl_data.rda")
load(file="nl_changes.rda")
load(file="md_final.rda")


### Set variables --------------------------------------------------------------

parameter_date = as.Date("2012-04-01")
st_layer = st_geo[st_geo@data$NAME_1=="Kachin",]
changesPalette = c("#00dfff","#D6EFF6","#F2F3EC","#fdfce1","#fbf79b")
changesLevels = c("Strong Decrease", "Decrease", "Similar", "Increase", "Strong Increase")
changesBreaks = c(-1, -0.3, -0.05, 0.05, 0.3, 1)

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

draw_region_map <- function(map, layer, session) {

  tmapProxy("map", session, {
    
    # remove layers
    tm_remove_layer(401) +
    
    # add layers  
    tm_shape(layer) +
    tm_borders(col="black", 
               lwd=2,
               alpha = 0.8,
               zindex = 401) +
    tm_layout(frame = FALSE)
    
  })
}

draw_nl_layer <- function(map, layer, session) {
  
  tmapProxy("map", session, {
    
    # remove layers
    tm_remove_layer(402) +
      tm_remove_layer(403) +
      
    # add layers
    tm_shape(layer) + 
    tm_fill(col = "change",
            breaks = c(-1, -0.3, -0.05, 0.05, 0.3, 1),
            labels = c("Strong Decrease", "Decrease", "Similar", "Increase", "Strong Increase"),
            palette = "RdYlGn",
            contrast = c(0,1),
            zindex = 402,
            legend.show = TRUE,
            legend.z = 1) +
    tm_borders(col = "black", 
               lwd = 1, 
               lty="dotted",
               alpha = 0.5,
               zindex = 403) +
    tm_legend(show=TRUE)
    
  })
}

draw_events_layer <- function(map, layer, session) {
  
    tmapProxy("map", session, {
  
      # remove layers
      tm_remove_layer(404) +
      
      # add layers
      tm_shape(layer) +
        tm_bubbles(col = "impact_level",
                   alpha = 0.5,
                   palette = c("white","pink","red","black"),
                   size = 0.25,
                   zindex = 404) +
      tm_legend(show=TRUE)
      
    })
    
}


### Functions for the exploration plot -----------------------------------------

draw_plot <- function(region){
  

  
}


### Functions for the prediction plot ------------------------------------------



