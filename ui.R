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
                    choices = c("All conflicts in the region", cn_conflicts_list$conflict_name),
                    selected = "All conflicts in the region"),
        
        sliderTextInput("precision", 
                        h6("Precision of the events:"),
                        choices = c("Low", "Medium", "High"),
                        selected = c("Medium","High"),
                        animate = FALSE, 
                        grid = TRUE, 
                        hide_min_max = TRUE, 
                        from_fixed = FALSE,
                        to_fixed = FALSE, 
                        from_min = NULL, 
                        from_max = NULL, 
                        to_min = NULL,
                        to_max = NULL, 
                        force_edges = TRUE,
                        width = NULL, 
                        pre = NULL,
                        post = NULL, 
                        dragRange = FALSE),
        
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
       
        "App in development (v.23),",
        br(),
        "some features may not be available"

      ), # absolute panel - Messages
               
      # Map
      absolutePanel(
      
        id = "map", class = "panel panel-default", 
        fixed = TRUE, draggable = FALSE, 
        top = 75, left = "21%", bottom = "auto",
        width = "50%", height = "86%",
        zindex = 100,

        tmapOutput("map", width = "100%", height = "100%"),
        

        h2("Impact of the conflicts over nightlights changes"),
        h1(paste0("Yearly rates from ",
                  month.abb[month(parameter_startDate)], "-", format(parameter_startDate,"%Y"),
                  " to ",
                  month.abb[month(parameter_endDate)], "-", format(parameter_endDate,"%Y"))),


        # Click township button
        

      ), # absolute panel - Map
             
      #  play with top % and if it doesnt work,
      # add fluid page and 2 rows to align the predictions to the explorations
      # https://mastering-shiny.org/basic-ui.html
      
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
        h4("Severity trends of the conflict events", style = "text-align: center;"),
        plotOutput("explore_plot", width = "100%", height = "68%")
      
      ), # absolute panel - Explorations
             
      # Predictions
      absolutePanel(
       
        id = "future", class = "panel panel-default",
        fixed = TRUE, draggable = FALSE, 
        top = "auto", right = "1%", bottom = "1%",
        width = "26%", height = "39%",
        
        # plot
        h4("Estimated population change", style = "text-align: center; padding-top: 5px;"),
        plotlyOutput("prediction_plot", width = "98%", height = "85%")
        
      ) # absolute panel - Predictions
           
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
       
      ) # absolute panel - Text

    ), # tabPanel - Project

    tabPanel("Data",
             
     # Instructions
     absolutePanel(
       
       id = "data_instructions", class = "parameters", 
       fixed = TRUE, draggable = FALSE, 
       top = 75, left = "1%", bottom = "auto",
       width = "18%", height = "80%",
       
       h5("Data interpretation"),
       br(),
       br(),
       
       "For the severity of the events",
       hr(),
       "For the nightlights change",
       hr(),
       "For the accuracy of the events",
       hr(),
       "For the map",
       hr(),
       "For the exploration plot",
       hr(),
       "For the prediction plot",
       
       ),
      
      # Text
      absolutePanel(
       
       id = "data", class = "panel panel-default", 
       fixed = TRUE, draggable = FALSE, 
       top = 75, left = "22%", bottom = "auto",
       width = "50%", height = "85%",
       
       includeMarkdown("Data.Rmd"),
       br()
       
      ), # absolute panel - Text
      
      # info
      absolutePanel(
        
        id = "data_info", class = "parameters",
        fixed = TRUE, draggable = FALSE, 
        top = "50%", right = "1%", bottom = "auto",
        width = "26%", height = "100%",
        
        "The data related to the nightlights have some circumstances that make use them difficult",
        "so the project will be halted until new improvements are made.",
        hr(),
        "The models will be updated when the nightlights data provides mor accuracy about the readings",
        "At the moment, unforyunately, they don´t reflect accurate predictions becasue as commented before",
        "there are some issues with the nightlights data that make the modeling extremely difficult",
        hr(),
        "For the collection of the conflicts data it has been used a restful API. I don´t expect the",
        "structure of the data changes, but if you see some error, please let me know.",
        "For the nightlights data there is set up a manual process as it involves more processing time."
        
      ) # absolute panel - info

    ) # tabPanel - Data

  ) # navbarPage
))
