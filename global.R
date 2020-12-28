#
# Loads everything to the global R session environment.
# It will be called first and only once for each session. 
#
# We will define here the functions that never change. 
# These functions will collect the parameters and the data from the server file.
#

### Load the libraries ---------------------------------------------------------

# Processing
library(dplyr) 
library(lubridate) 
library(ggExtra)
library(tidyr)
library(reshape)

# Exploration
library(tmap)
library(tmaptools)
library(rgdal)
require(rgeos)
library(ggplot2)
#library(plotly)
library(scales)

# Dashboard
library(shinydashboard)
library(shinyWidgets)

### Read files -----------------------------------------------------------------

# load geographical borders
rg_geo <- readRDS("rg_geo.rds")
#rg_geo <- readRDS("rg_clusters.rds")
tw_geo <- readRDS("tw_geo.rds")

# load dataframes
load(file="cn_conflicts.rda")
load(file="cn_conflicts_list.rda")
load(file="cn_events.rda")
#load(file="cn_locations.rda")
#load(file="cn_locations_detail.rda")
load(file="nl_data.rda")
load(file="nl_changes.rda")
load(file="pp_changes.rda")
load(file="rg_clusters.rda")

### Set variables --------------------------------------------------------------

# Initial parameters
parameter_region = "Kachin"
parameter_startDate = as.Date(min(cn_conflicts[cn_conflicts$region==parameter_region,5]))
if (parameter_startDate < as.Date("2012-04-01")) { parameter_startDate = as.Date("2012-04-01") }
parameter_endDate = as.Date(max(cn_conflicts[cn_conflicts$region==parameter_region,6]))

# Colors
severityPalette = c("#FF0000","#C0C0C0","#FF8C00")
names(severityPalette) <- levels(cn_events$severity_level)
severityScale <- scale_colour_manual(name="severity_level", values=severityPalette)

lightsPalette = c("#00dfff","#D6EFF6","#F2F3EC","#fdfce1","#fbf79b")
names(lightsPalette) = c("Plummeted", "Decreased", "Similar", "Increased", "Rocketed")
lightsScale <- scale_colour_manual(name = "light_level", values=lightsPalette)
lightsBreaks = c(-1, -0.3, -0.05, 0.05, 0.3, 1)

# Lights Levels
lights_score <- mean(nl_changes[nl_changes$region == parameter_region, 6], na.rm=TRUE)
lights_level <- case_when(
  lights_score < -0.30 ~ "Plummeted",
  lights_score < -0.05 ~ "Decreased",
  lights_score < 0.05 ~ "Similar",
  lights_score < 0.30 ~ "Increased",
  TRUE ~ "Rocketed")
  
# Severity Levels
severity_score <- mean(cn_events[cn_events$region == parameter_region, 30], na.rm=TRUE)
severity_level <- case_when(
  severity_score < 20 ~ "Low",
  severity_score < 50 ~ "Medium",
  TRUE ~ "High")

### Functions to display correct UI options ------------------------------------


### Functions to get correct dataframes based on parameters --------------------



### Functions for the map ------------------------------------------------------

tmap_mode("view")

### Functions for the exploration plot -----------------------------------------

draw_exploration_plot <- function(eventsData) {
  
  # if there aren't events in the selected region we make an empty plot
  if (is.data.frame(eventsData) && nrow(eventsData)==0) {
    
    p <- ggplot()

  } else {
    
    p <- ggplot(eventsData, aes(x=date_start, y=severity_score, colour=severity_level)) +
      geom_point(size=4, stroke=0, shape=16, alpha=0.3) +
      geom_smooth(method = "lm", formula = y ~ x, alpha=0.2, aes(fill = severity_level))
    
  }
      
  p <- p + theme(
      legend.position = "none",
      plot.background = element_blank(),
      panel.background = element_rect(fill = "#FFFFFF"),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(size = 0.5, 
                                        linetype = 'dotted',
                                        colour = "grey"), 
      panel.grid.major.y = element_line(size = 0.5, 
                                        linetype = 'dotted',
                                        colour = "grey"), 
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.title.y=element_text(size=10, 
                                colour="dark grey",
                                margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.text.y=element_text(size=8, colour="dark grey"),
      axis.ticks.y=element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_text(size=8, colour="dark grey"),
      axis.ticks.x=element_blank(),
      axis.ticks = element_blank()
    ) +
    ylab("severity score") +
    ylim(c(0, NA)) +
    scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, by = 20)) +
    scale_x_date(limits = as.Date(c("2012-01-01", "2019-12-01"))) +
      severityScale

    p
    
}

### Functions for the prediction plot ------------------------------------------

draw_prediction_plot <- function(populationData) {
  
  p <- ggplot(populationData, aes(month, township, fill=light_level)) +
    geom_tile(color="grey", size=0.1) + 
    scale_fill_manual(values=lightsPalette, na.value="white") +
    facet_grid(.~year)
  
  p <- p + theme(
      panel.spacing.x=unit(0.01, "lines"), 
      panel.spacing.y=unit(0.01, "lines"),
      panel.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
      plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
      plot.title=element_blank(),
      strip.background = element_blank(),
      legend.position = "none",
      axis.title.y=element_text(size=10, 
                                colour="dark grey",
                                margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text=element_blank(),
      strip.text.x = element_text(size=8, colour="dark grey", angle=90)
    ) +
    xlab("") + 
    ylab("townships") +
    removeGrid()
  
  p 
  
}