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
       
        "App in development (v.24),",
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
        

        h2("Changes in nightlights in the conflicts clusters"),
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
             
      # Project Text
      absolutePanel(
       
        id = "project", class = "panel panel-default", 
        fixed = TRUE, draggable = FALSE, 
        top = "11%", left = "22%", bottom = "auto",
        width = "50%", height = "85%",
        
        h3("Exploring the drivers of migration through the nightlights lens"),
        br(),
        br(),
        "The main goal of this project is to provide users with tools to explore information linked to migration on development, ",
        "supporting them in understanding the current situation, and allowing them to estimate past (where data doesn't exist) ",
        "and future changes, that would help to take action to improve the impact of migration in the Lower Mekong Region.",
        br(),
        br(),
        "To achieve this, the app will use only open data sources, having the nightlights data as base to find correlations ",
        "with other sources of data that are of interest.",
        "The domains that will be observed to explore the drivers of migartion are conflicts, economic forces, ",
        "climate change and humanitarian disasters.",
        
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
        "If you have any suggestion or question about the app, or want to include any kind of information or answer a question, ",
        "drop us a message, we will be glad to help you as soon as possible.",
        br(),
        br(),
        br(),
        a(actionButton(inputId = "b_email", 
                       label = "Contact Us", 
                       icon = icon("refresh"),
                       class = "btn btn-standard"),
                       href="mailto:natxo.reguera@gmail.com",
                       target="_blank")
        
      ) # absolute panel - Info

    ), # tabPanel - Project

    tabPanel("Data",
             
     # Instructions
     absolutePanel(
       
       id = "data_instructions", class = "parameters", 
       fixed = TRUE, draggable = FALSE, 
       top = "11%", left = "1%", bottom = "auto",
       width = "18%", height = "80%",
       
       h5("How to read the data?"),
       br(),
       br(),
       
       "A metric values the severity of the events based on the length of the events, ",
       "and the number of deats (civilians, unknown and total), giving special relevence to civilians deaths.",
       "Each event has a score that is used to clasify the event in a severity level.",
       br(),       
       br(),
       "Another metric values the accuracy of the events based on the location precision and the date precision, ",
       "on the number of sources, and on the clarity of the event. From this data, each event receives a score ",
       "and then is clasified into a level of accuracy.",
       br(),       
       br(),
       "The nightlights data represents radiance readings measured in nanoWatts/cm2. ",
       "Based on these readings, it has been calculated the changes and annualized them to make them comparable. ",
       "Then these changes has been classified in 5 group depending on the difference with the previous period, ",
       "setting breaks at 30% and 5%, at both negatives and positives sides.",
       br(),       
       br(),
       "The map shows two layers of information: the first with the radiances changes (counting from the start of the conflicts ",
       "in the selected region, or from the start of the conflict if user has selected a conflict);",
       "the second with the location where events happened and its severity.",
       br(),       
       br(),
       "In the right side, the two top boxes show the level of changes and severity for the selected region, as an average of ",
       "it's townships. Below them, a plot shows all the events for the seleceted region/conflict, classified by its level ",
       "of severity and with its trend over the time.",
       br(),       
       br(),
       "In the right bottom the plot shows the estimated changes in population for each township in the selected region",
       "classified based on the previous nightlights changes levels.",
       
       ),
      
      # Data Text
      absolutePanel(
       
          id = "data", class = "panel panel-default", 
          fixed = TRUE, draggable = FALSE, 
          top = "11%", left = "22%", bottom = "auto",
          width = "50%", height = "85%",
          
          h3("Only Open Data"),
          br(),
          br(),
          "The app is using mainly Remote Sensing data together with manually produced and collected data.",
          "Specifically the app is going to ingest the following sources of data:",
          br(),
          br(),
          h5("DMSP data collected by US Air Force Weather Agency Image and data processing by NOAA's National Geophysical Data Center"),
          h6(a("[link]", href="https://www.ngdc.noaa.gov/eog/download.html")),
          h5("VIIRS data produced by Earth Observation Group, Payne Institute for Public Policy"),
          h6(a("[link]", href="https://eogdata.mines.edu/download_dnb_composites.html")),
          h5("UCDP Georeferenced Event dataset, collected by the Department of Peace and Conflict Research, Uppsala University"),
          h6(a("[link]", href="https://ucdp.uu.se/")),
          h5("Geographical Administrative Divisions v3.6, produced and distributed by GADM"),
          h6(a("[link]", href="https://gadm.org/")),
          h5("Myanmar Census data, produced and distributed by Myanmar Information Managament Unit"),
          h6(a("[link]", href="http://themimu.info/")),
                 
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
        "For the acquisition of conflicts data it has been used a RESTful API that connects with ",
        "UCDP server and downloads it. This process will be automatically executed monthly, but If you want to ",
        "update it now, just click the button below.",
        br(),
        br(),
        br(),
        actionButton("b_conflict", 
                     "Update conflict data", 
                     class = "btn btn-standard"),
        br(),
        br(),
        br(),
        "For the nightlights data, it has been set up a manual process that acquires the data, process it, and ingest into ",
        "the app, as it involves images processing that cost time and resources not currently available in the server where ",
        "this app is deployed. This process will be carried in a monthly basis.",
        br(),
        br(),
        "Population data is manually acquired and included in the app as well. This process will be carried when ",
        "the corresponding government office collects and makes available this data.",
        hr(),
        "A note about the nightlights data: ",
        "currently there is some noise coming from moonlight, fires, and other natural sources of light",
        ", making sometimes difficult to get accurate data. NASA’s Goddard Space Flight Center is developing ",
        "new software to make this data clearer and accurate. Once these improvements are available, we will ",
        "incude them in our models.",
        br(),
        br(),
        "A note about the predictions: ",
        "models are trained with conflicts, population and nightlights data. ",
        "Some of these data lack of accuracy or completeness, that may affect to the performance of the models. ",
        "For this reason, the estimations calculated must be taken as preliminary 'drafts' ",
        "until more quality data is available to re-train and improve the models.",
        
      ) # absolute panel - info

    ) # tabPanel - Data

  ) # navbarPage
))
