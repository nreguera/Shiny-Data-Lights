#
# User-interface. 
# Called every time there is any change in the session. 
#


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
        top = "11%", left = "1%", bottom = "auto",
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
        top = "11%", left = "21%", bottom = "auto",
        width = "50%", height = "86%",
        zindex = 100,

        tmapOutput("map", width = "100%", height = "100%"),
        

        h2("Changes in nightlights in the locations with events"),
        h1(textOutput("dateRange"))

        # Click township button
        

      ), # absolute panel - Map
             
      # Explorations
      absolutePanel(
       
        id = "past", class = "panel panel-default",
        fixed = TRUE, draggable = FALSE, 
        top = "11%", right = "1%", bottom = "auto",
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
        h4("Severity trends of the events", style = "text-align: center;"),
        plotOutput("explore_plot", width = "100%", height = "68%")
      
      ), # absolute panel - Explorations
             
      # Predictions
      absolutePanel(
       
        id = "future", class = "panel panel-default",
        fixed = TRUE, draggable = FALSE, 
        top = "58%", right = "1%", bottom = "auto",
        width = "26%", height = "39%",
        
        # plot
        h4("Estimated population change", style = "text-align: center; padding-top: 5px;"),
        plotOutput("prediction_plot", 
                   width="98%", height="85%"),
        verbatimTextOutput("hover_info")
        
      ) # absolute panel - Predictions
           
    ), # tabPanel - App

    tabPanel("Project",
             
             # Background
             absolutePanel(
               
                id = "data_instructions", class = "parameters", 
                fixed = TRUE, draggable = FALSE, 
                top = "11%", left = "1%", bottom = "auto",
                width = "18%", height = "80%",
                
                h5("Background"),
                br(),
                br(),
                "Migration of people in the Lower Mekong Region (Myanmar, Thailand, Laos, Cambodia, and Vietnam) ",
                "is poised to grow in significance, especially with the development of the ASEAN Economic Region ",
                "that has as one of its main goals increase connectivity in part lowering of barriers ",
                "to the movement of people across its borders. However, official data may not necessarily show the full ",
                "picture: decennial census data is often outdated and not representative of whole populations, ",
                "particularly for marginalized communities such as those living in rural and remote areas and ",
                "in urban slums, and indigenous and ethnic minority populations. There are also often ",
                "issues with data collection methodologies and approaches. Thus, actual numbers of people migrating ",
                "and the resulting migratory patterns and trends are difficult to estimate. ",
                br(),
                br(),
                "For these reasons, it is necessary to look beyond official data to better understand population dynamics ",
                "and migration flows in the Mekong Region, to improve different development impacts, including:",
                br(),
                br(),
                "•	More accurate targeting of assistance and services to locations where it is most needed. ",
                br(),
                "•	Better evidence to guide decision-making on directing resources towards areas where marginalized communities are actually located.",
                br(),
                "•	Support for the identification of population flows to track humanitarian disasters.",
                br(),
                "•	Detection of regional inequalities as result of migration flows.",
                br(),
                "•	Observe how growth can occur in different parts of countries, so that communities can share in national prosperity and not get left behind.",
               
               
               ), # background panel
             
      # Text
      absolutePanel(
       
       id = "project", class = "panel panel-default", 
       fixed = TRUE, draggable = FALSE, 
       top = "11%", left = "22%", bottom = "auto",
       width = "50%", height = "85%",
       
       includeMarkdown("Project.Rmd"),
       br()
       
      ), # absolute panel - Text
      
      # info
      absolutePanel(
        
        id = "data_info", class = "parameters",
        fixed = TRUE, draggable = FALSE, 
        top = "11%", right = "1%", bottom = "auto",
        width = "26%", height = "100%",
        
        h5("About us"),
        br(),
        br(),
        "If you have any suggestion or question about the app, please send us an email and we will try to get you ",
        "back as soon as possible.",
        br(),
        br(),
        br(),
        a(actionButton(inputId = "email1", label = "Contact Us", 
                       icon = icon("envelope", lib = "font-awesome")),
          href="mailto:my_awesome_email_address.com")
        
      ) # absolute panel - Info

    ), # tabPanel - Project

    tabPanel("Data",
             
     # Instructions
     absolutePanel(
       
       id = "data_instructions", class = "parameters", 
       fixed = TRUE, draggable = FALSE, 
       top = "11%", left = "1%", bottom = "auto",
       width = "18%", height = "80%",
       
       h5("How to interpret the data?"),
       br(),
       br(),
       
       "We have created a metric to value the severity of the events based on the length of the events, ",
       "and the numbers of civilians, unknown and total, giving a bit more important to the civilians.",
       "For each levels of severity, we have set a score range to clasify the events based on their score.",
       br(),       
       br(),
       "For the accuracy of the events we have done something similar, calculating a metric that scores the ",
       "accuracy of each event based on the location and dates precision, the number of sources, and ",
       "the clarity of the event, giving a score and clasifying them into a predefined level of accuracy.",
       br(),       
       br(),
       "The nightlights represent the radiance measured in nanoWatts/cm2/sr. The changes has been classified ",
       "in 5 group with breaks at 30% and 5% both negatives and positives.",
       br(),       
       br(),
       "For the map there are basically two layers: one showing the yearly radiances changes for a period, ",
       "counted from the start of the conflicts in the selected region, or from the start of the conflict in ",
       "the selected conflict; anohter showing the location and severity of the events based on the previous classification.",
       br(),       
       br(),
       "For the exploration plot it is shown the events classified by its level of severity, and the trend ",
       "over the time.",
       br(),       
       br(),
       "Finally, the prediction plot shows the predicted changes in population for each township in the selected region",
       "classified based on the previous nightlights changes levels.",
       
       ),
      
      # Text
      absolutePanel(
       
       id = "data", class = "panel panel-default", 
       fixed = TRUE, draggable = FALSE, 
       top = "11%", left = "22%", bottom = "auto",
       width = "50%", height = "85%",
       
       includeMarkdown("Data.Rmd"),
       br()
       
      ), # absolute panel - Text
      
      # info
      absolutePanel(
        
        id = "data_info", class = "parameters",
        fixed = TRUE, draggable = FALSE, 
        top = "11%", right = "1%", bottom = "auto",
        width = "26%", height = "100%",
        
        h5("How data is ingested in the app?"),
        br(),
        br(),
        "For the integration of conflicts data in the app it has been used a RESTful API that automatically ",
        "connects with the UDP server and downloads it. If you want to updated it now, just click the button below.",
        br(),
        br(),
        br(),
        actionButton("button", "Update conflict data", size="xs", style = "material-flat"),
        br(),
        br(),
        br(),
        "For the nightlights data there is set up a manual process that acquires the data and ingest into the app, ",
        "as it involves more processing time and resources that are not currently available in the server where this app is ",
        "deployed. It will be updated in a monthly basis.", 
        br(),
        br(),
        "Population data is manually downloaded and included in the app as well, when a collection by ",
        "the corresponding government office is made.",
        hr(),
        "A note about the nightlights data: ",
        "the data currently available have some noise coming from moonlight, fires, and other natural sources of light",
        ", making sometimes difficult to get accurate data. NASA’s Goddard Space Flight Center is developing new software to make ",
        "this data clearer and accurate.",
        br(),
        br(),
        "A note about the predictions: ",
        "it has been implemented two types of models in the app: ",
        "regression and classification. Regression models are trained with conflicts data, with population data (of ",
        "which there is only one period), and with nightlights data (that as it has been already explained ",
        "it lacks of enough accuracy. For these reasons, the estimations calculated in this app must be taken ",
        "as preliminary 'drafts' until more quality data is available.",
        
      ) # absolute panel - info

    ) # tabPanel - Data

  ) # navbarPage
))
