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
library(plotly)
library(scales)

# Dashboard
library(shinydashboard)
library(shinyWidgets)
#library(shinydashboardPlus)

### Read files -----------------------------------------------------------------

# load geographical borders
rg_geo <- readRDS("rg_geo.rds")
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

# Lights
lights_score <- mean(nl_changes[nl_changes$region == parameter_region, 6], na.rm=TRUE)
lights_level <- case_when(
  lights_score < -0.30 ~ "Plummeted",
  lights_score < -0.05 ~ "Decreased",
  lights_score < 0.05 ~ "Similar",
  lights_score < 0.30 ~ "Increased",
  TRUE ~ "Rocketed")
  
# Severity
severity_score <- mean(cn_events[cn_events$region == parameter_region, 30], na.rm=TRUE)
severity_level <- case_when(
  severity_score < 20 ~ "Low",
  severity_score < 50 ~ "Medium",
  TRUE ~ "High")

### Functions to display correct UI options ------------------------------------

# A function for getting integer y-axis values
integer_breaks <- function(n = 5, ...) {
  
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  
  return(fxn)
}

### Functions to get correct dataframes based on parameters --------------------



### Functions for the map ------------------------------------------------------

tmap_mode("view")

### Functions for the exploration plot -----------------------------------------



### Functions for the prediction plot ------------------------------------------



