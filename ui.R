#
# User-interface. 
# It will be called everytime is any change in the session. 
#

# libraries -------------------
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)
library(ggiraph)
library(dashboardthemes)
library(tmap)
library(ggplot2)
library(rgdal)

# user interface -------------------
ui <- shinyUI(
  
  fluidPage(

    # Include CSS files
    includeCSS(path = "boostrap.css"),
    includeCSS(path = "shinydashboard.css"), 
    includeCSS(path = "AdminLTE.css"),
    
    setBackgroundColor("#F5F5F5"),
  
    navbarPage(
        
        title = "Data Lights",
        
        tabPanel("Explore",
             
                # Parameters
               absolutePanel(
                 
                 id = "parameters", class = "standard", 
                 fixed = TRUE, draggable = FALSE, 
                 top = 50, left = "0%", bottom = "auto",
                 width = "20%", height = "100%",
                   
                 sidebarPanel(
                   
                   width = 300, 
                   
                   h5("Select the type of analysis"),
                       selectInput("analysis",
                               h6("Choose from the list below:"),
                               choices = c("Impact of conflicts by region"),
                               selected = "Impact of conflicts by region",
                               selectize = TRUE),
                   
                   hr(),
                   
                   h5("Select the parameters for the analysis"),
                   
                   selectInput("region",
                               h6("Region or state:"),
                               choices = c("Yangon",
                                           "Mandalay",
                                           "Magway",
                                           "Sagaing",
                                           "Bago",
                                           "Ayeyarwady",
                                           "Taninthayi",
                                           "Kachin",
                                           "Kayah",
                                           "Kayin",
                                           "Chin",
                                           "Mon",
                                           "Rakhine",
                                           "Shan"),
                               selected = "Kachin"),

                   radioGroupButtons("precision", 
                                     h6("Precision of the results:"),
                                     choices = c("Low", "Medium", "High"),
                                     selected = "Medium",
                                     size="xs",
                                     justified=TRUE,
                                     individual=FALSE),
                   
                   hr(),
                   
                   h5("Select the data to predict"),
                  selectInput("data_predict",
                               h6("Choose from the list below:"),
                               choices = c("Deaths",
                                           "Human migration"),
                               selected = "Deaths"),
                   

                       
                     sidebarPanel(
                       
                           h5("App in development, version #13", style = "font-size: 8px;color: red;font-style: italic;")
                   )
                 ),
                 
                 # Map
                 absolutePanel(
                 
                   id = "map", class = "panel panel-default", 
                   fixed = TRUE, draggable = FALSE, 
                   top = 75, left = "22%", bottom = "auto",
                   width = "50%", height = "85%",
                    
                   tmapOutput("tm", width = "100%", height = "100%"),
                
                  ),
               
                # Explorations
                absolutePanel(
                 
                  id = "past", class = "panel panel-default",
                  fixed = TRUE, draggable = FALSE, 
                  top = 75, right = "1%", bottom = "auto",
                  width = "26%", height = "45%",
                  
                    # KPIs
                    fluidRow(
                      splitLayout(
                        style = "border: 0px solid silver;",
                        cellWidths = "50%",
                        cellArgs = list(style = "padding: 5px;
	                                                line-height: 40% !important;"),
                        valueBoxOutput("KPI1"),
                        valueBoxOutput("KPI2")
                      )
                    ),
                  
                    # graphic
                    plotOutput("line_plot", width = "97%", height = "65%")

               ),
               
               # Predictions
               absolutePanel(
                 
                 id = "future", class = "panel panel-default",
                 fixed = TRUE, draggable = FALSE, 
                 top = "auto", right = "1%", bottom = "2%",
                 width = "26%", height = "39%",
                 
                 h5("No predictions for the selected data", style = "text-align: center; 
                                                        color: light-grey;
                                                        padding-top: 100px;")
                 
               ),
               
             ), 
        ),
    
        tabPanel("Project"
        ),
    
        tabPanel("Data"
        )
    )
  )
)