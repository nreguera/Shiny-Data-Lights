#
# User-interface. 
# Called every time there is any change in the session. 
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
ui <- shinyUI(fluidPage(

  # Include CSS files
  includeCSS(path = "boostrap.css"),
  includeCSS(path = "shinydashboard.css"), 
  includeCSS(path = "AdminLTE.css"),
    
  setBackgroundColor("#F5F5F5"),

  navbarPage(
        
    title = "Data Lights",
        
    tabPanel("App",
             
      # Parameters
      absolutePanel(
                   
        id = "parameters", class = "parameters", 
        fixed = TRUE, draggable = FALSE, 
        top = 75, left = "1%", bottom = "auto",
        width = "18%", height = "80%",
  
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
          choices = c("Ayeyarwady",
            "Bago",
            "Chin",
            "Kachin",
            "Kayah",
            "Kayin",
            "Magway",
            "Mandalay",
            "Mon",
            "Rakhine",
            "Sagaing",
            "Shan",
            "Taninthayi",
            "Yangon"),
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
            choices = c("Human movements"),
            selected = "Human movements"),
          
          ), # absolute panel - Parameters
  
          # Messages
          absolutePanel(
          
            id = "messages", class = "messages", 
            fixed = TRUE, draggable = FALSE, 
            top = "auto", left = "1%", bottom = "5%",
            width = "18%", height = "8%",
           
            "App in development,",
            br(),
            "some features may not be available.",
            br(),
            "version #14"
            
          ), # absolute panel - Messages
                   
          # Map
          absolutePanel(
          
            id = "map", class = "panel panel-default", 
            fixed = TRUE, draggable = FALSE, 
            top = 75, left = "22%", bottom = "auto",
            width = "50%", height = "85%",
            
            tmapOutput("tm", width = "100%", height = "100%"),
          
          ), # absolute panel - Map
                 
          # Explorations
          absolutePanel(
           
            id = "past", class = "panel panel-default",
            fixed = TRUE, draggable = FALSE, 
            top = 75, right = "1%", bottom = "auto",
            width = "26%", height = "45%",
          
            # KPIs
            fluidRow(
              splitLayout(
                style = "border:0px; padding:0px;",
                cellWidths = "50%",
                cellArgs = list(style = "padding: 1%;
                                line-height: 10% !important;
                                vertical-align: center !important;
                                horizontal-align: center !important;"),
                valueBoxOutput("KPI1"),
                valueBoxOutput("KPI2")
              )
            ),
            
            # graphic
            h5("Historical deadly events", style = "text-align: center;"),
            plotOutput("line_plot", width = "97%", height = "65%")
          
          ), # absolute panel - Explorations
                 
          # Predictions
          absolutePanel(
           
            id = "future", class = "panel panel-default",
            fixed = TRUE, draggable = FALSE, 
            top = "auto", right = "1%", bottom = "2%",
            width = "26%", height = "38%",
            
            h5("No predictions available for the selected data", 
                                                  style = "text-align: center; 
                                                  color: light-grey;
                                                  padding-top: 100px;")
           
          ), # absolute panel - Predictions
               
        ), # tabPanel - App
    
        tabPanel("Project",
                 includeMarkdown("Project.Rmd"),
                 br()

        ), # tabPanel - Project
    
        tabPanel("Data",
                 includeMarkdown("Data.Rmd"),
                 br()

        ) # tabPanel - Data

    ) # navbarPage
))