#
# User-interface. 
# Called every time there is any change in the session. 
#

# Load libraries -------------------------------------------------
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)
library(ggiraph)
library(dashboardthemes)
library(tmap)
library(ggplot2)
library(rgdal)

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

        h5("Adjust the parameters for the analysis"),
     
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
                                "Naypyitaw",
                                "Rakhine",
                                "Sagaing",
                                "Shan",
                                "Tanintharyi",
                                "Yangon"),
                    selected = "Kachin"),
        
        radioGroupButtons("type", 
                          h6("Type of conflict:"),
                          choices = c("All", "State", "No-state", "1-sided"),
                          selected = "All",
                          size="xs",
                          justified=TRUE,
                          individual=FALSE),
          
        selectInput("conflict",
                    h6("Conflict name:"),
                    choices = c("All conflicts in the region",cn_conflicts_list$conflict_name),
                    selected = "All conflicts in the region"),
        
        radioGroupButtons("precision", 
                          h6("Precision of the events:"),
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
        top = "auto", left = "0%", bottom = "4.5%",
        width = "16%", height = "6%",
       
        "App in development [v.18]",
        br(),
        "Some features may not be available",

      ), # absolute panel - Messages
               
      # Map
      absolutePanel(
      
        id = "map", class = "panel panel-default", 
        fixed = TRUE, draggable = FALSE, 
        top = 75, left = "22%", bottom = "auto",
        width = "50%", height = "85%",
        
        tmapOutput("map", width = "100%", height = "100%"),
      
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
        
        # plot
        h5("Severity of the events in the region", style = "text-align: center;"),
        plotOutput("explore_plot", width = "97%", height = "65%")
      
      ), # absolute panel - Explorations
             
      # Predictions
      absolutePanel(
       
        id = "future", class = "panel panel-default",
        fixed = TRUE, draggable = FALSE, 
        top = "auto", right = "1%", bottom = "1.6%",
        width = "26%", height = "38%",
        
        # plot
        h5("Human movements predictions", style = "text-align: center; padding-top: 5px;")
       
      ), # absolute panel - Predictions
           
    ), # tabPanel - App

    tabPanel("Project",
             
      # Text
      absolutePanel(
       
       id = "project", class = "panel panel-default", 
       fixed = TRUE, draggable = FALSE, 
       top = 75, left = "22%", bottom = "auto",
       width = "50%", height = "85%",
       
       includeMarkdown("Project.Rmd"),
       br()
       
      ), # absolute panel - Text

    ), # tabPanel - Project

    tabPanel("Data",
             
      # Text
      absolutePanel(
       
       id = "data", class = "panel panel-default", 
       fixed = TRUE, draggable = FALSE, 
       top = 75, left = "22%", bottom = "auto",
       width = "50%", height = "85%",
       
       includeMarkdown("Data.Rmd"),
       br()
       
      ), # absolute panel - Text

    ) # tabPanel - Data

  ) # navbarPage
))