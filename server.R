#
# Server logic.
# It will be called once for each session. 
#
# The reactive function allows a user to monitor the status of an input or other changing variable 
# and return the value to be used elsewhere in the code. The monitoring of a reactive variable is considered 
# lazy: when their dependencies change, they don't re-execute right away but rather wait until they are called
# by someone else. For example, as you can call the variable inside the renderText environment, 
# once called the code inside the reactive call executes and re-evaluates the variable.
#
# Observe is similar to reactive, the main difference is that it does not return any value 
# to any other environment besides its own, and it is not lazy. The observe function continually monitors 
# any changes in all reactive values within its environment and runs the code in it's environment 
# when these values are changed.
#
# The main difference between observeEvent and observe is the trigger, as observe runs anytime anything
# changes, and observeEvent waits for the trigger. Note that this environment is similar to observe 
# in that it does not return non-reactive variables.
#

server <- function(input, output, session){
    
    ### Set UI choices and selections ------------------------------------------
    
    
    ### Data Manipulations -----------------------------------------------------
    
    # update UI conflict names based on the region
    observeEvent(input$region,{

        
        if (input$type == "All"){
            
            updateSelectInput(session,
                              'conflict', 
                              choices = c("All conflicts in the region", cn_conflicts$conflict_name[cn_conflicts$region == input$region]))
            
        } else {
            updateSelectInput(session,
                              'conflict',
                              choices = c("All conflicts in the region", cn_conflicts$conflict_name[cn_conflicts$type_of_violence == input$type &
                                                                         cn_conflicts$region == input$region]))
            
        }
        
        
    })

    # update UI conflict names based on the type of conflict
    observeEvent(input$type,{
        
        if (input$type == "All"){
            
            updateSelectInput(session,'conflict', choices = c("All conflicts in the region", 
                                          cn_conflicts$conflict_name[cn_conflicts$region == input$region]))
            
        } else {
            
            updateSelectInput(session,
                              'conflict',
                              choices = c("All conflicts in the region", 
                                          cn_conflicts$conflict_name[cn_conflicts$type_of_violence == input$type &
                                                                         cn_conflicts$region == input$region]))
            
        }
        
    })
    
    # update start and end date when a conflict is selected
    observeEvent(input$conflict, {
        
        if (input$conflict != "All conflicts in the region"){
            
            parameter_startDate = as.Date(min(cn_conflicts[cn_conflicts$conflict_name==input$conflict,5]))
            if (parameter_startDate < as.Date("2012-04-01")) { parameter_startDate = as.Date("2012-04-01") }
            parameter_endDate = as.Date(max(cn_conflicts[cn_conflicts$conflict_name==input$conflict,6]))
 
            
        } else {
            
            parameter_startDate = as.Date(min(cn_conflicts[cn_conflicts$region==input$region,5]))
            if (parameter_startDate < as.Date("2012-04-01")) { parameter_startDate = as.Date("2012-04-01") }
            parameter_endDate = as.Date(max(cn_conflicts[cn_conflicts$region==input$region,6]))

        }
        
        output$dateRange <- renderText({ 
            
            dateRange <- paste0("Yearly rates from ",
            month.abb[month(parameter_startDate)], 
            "-", 
            format(parameter_startDate,"%Y"),
            " to ",
            month.abb[month(parameter_endDate)],
            "-",
            format(parameter_endDate,"%Y"))
            
            dateRange
        })
        
    })

    # UPDATE GEOGRAPHIC REGION INFORMATION
    st_data <- reactive({
        
        rg_geo[rg_geo@data$NAME_1==input$region,]
        
    })
    
    # UPDATE NIGHTLIGHTS INFORMATION
    nl_dataChanges <- reactive({
        
        # select the first day of each month
        start = as.Date(cut(parameter_startDate, "month"))
        end = as.Date(cut(parameter_endDate, "month"))
        
        # Changes are normalized by year (like we do with  %  of interest of money):
        # the readings change will be divided by the years between the first and last date
        parameter_length <- as.numeric(difftime(end, start, unit="weeks"))/52.25
        
        nl_changes = nl_data[which(nl_data$region==input$region),]
        nl_changes = nl_changes[(nl_changes$date==start | nl_changes$date==end),]
        nl_changes = cast(nl_changes, region+district+township~date, mean, value="radiance")
        names(nl_changes) = c("region", "district", "township", "first", "last")
        
        # Yearly rate
        nl_changes$light_score = nl_changes$last / nl_changes$first / parameter_length
        
        # assign levels
        nl_changes = nl_changes %>% 
            mutate(light_level = case_when(
                light_score < -0.3 ~ "Plummeted",
                light_score < -0.05 ~ "Decreased",
                light_score < 0.05 ~ "Similar",
                light_score < 0.3 ~ "Increased",
                light_score >= 0.30 ~ "Rocketed"))
        
        # create the spatial object and attach level
        nl_data = tw_geo[tw_geo@data$NAME_1==input$region,]
        
        # attach the change level
        nl_data@data$light_score = nl_changes[which(nl_changes$region==input$region), 6]
        nl_data@data$light_level = nl_changes[which(nl_changes$region==input$region), 7]
        
        nl_data
        
    })
    
    # UPDATE EVENTS INFORMATION
    events_data <- reactive({
        
        # filter the events by region, type of conflict and conflict name
        if (input$conflict == "All conflicts in the region") {
            
            if (input$type == "All" ) {
                
                cn_temp = cn_events[cn_events$region==input$region,]
                
            } else {
                
                cn_temp = cn_events[cn_events$type_of_violence==input$type &
                                        cn_events$region==input$region,]
            }
            
            # filter by the events where we have nightlights information
            cn_temp = cn_temp[cn_temp$date_start > "2012-04-01",]
            
            # update start and end dates
            parameter_startDate = as.Date(min(cn_temp[,20]))
            if (parameter_startDate < as.Date("2012-04-01")) { parameter_startDate = as.Date("2012-04-01") }
            parameter_endDate = as.Date(max(cn_temp[,21]))
            
        } else {
            
            if (input$type == "All") {
                
                cn_temp = cn_events[cn_events$region==input$region &
                                        cn_events$conflict_name==input$conflict,]
            }
            
            else {
                
                cn_temp = cn_events[cn_events$type_of_violence==input$type &
                                        cn_events$region==input$region &
                                        cn_events$conflict_name==input$conflict,]
            }
            
            # filter by the events where we have nightlights information
            cn_temp = cn_temp[cn_temp$date_start > "2012-04-01",]
            
            # update start and end dates
            parameter_startDate = as.Date(min(cn_temp[,20]))
            if (parameter_startDate < as.Date("2012-04-01")) { parameter_startDate = as.Date("2012-04-01") }
            parameter_endDate = as.Date(max(cn_temp[,21]))
            
        }
        
        # Filter by precision of the events
        if (input$precision[1] == "Low" & input$precision[2] == "Low") {
            
            cn_temp = cn_temp %>% filter(precision_level == "Low")
            
        } else {
            
            
            if (input$precision[1] == "Medium" & input$precision[2] == "Medium") {
                
                cn_temp = cn_temp %>% filter(precision_level == "Medium")
                
            } else {
                
                
                if (input$precision[1] == "High" & input$precision[2] == "High") {
                    
                    cn_temp = cn_temp %>% filter(precision_level == "High")
                    
                } else {
                    
                    
                    if (input$precision[1] == "Low" & input$precision[2] == "Medium") {
                        
                        cn_temp = cn_temp %>% filter(precision_level == "Low" |
                                                         precision_level == "Medium")
                        
                    } else {
                        
                        
                        if (input$precision[1] == "Medium" & input$precision[2] == "High") {
                            
                            cn_temp = cn_temp %>% filter(precision_level == "Medium" |
                                                             precision_level == "High")
                        }}}}}
        
        # we always pick the events from 2012
        #cn_temp = cn_temp[cn_temp$date_start >= parameter_startDate,]
        
        # prepare the dataframe to deliver
        cn_temp = as.data.frame(cn_temp)
        
        # deliver
        cn_temp
        
    })
    
    # UPDATE LOCATIONS INFORMATION
    # as we have the filter of events precision, we need to build the locations dataframe in real time,
    # as locations are made from different events, and each event can have different precision
    locations_data <- reactive ({
        
        # filter the events by region, type of conflict and conflict name
        if (input$conflict == "All conflicts in the region") {
            
            if (input$type == "All" ) {
                
                cn_temp = cn_events[cn_events$region==input$region,]
                
            } else {
                
                cn_temp = cn_events[cn_events$type_of_violence==input$type &
                                        cn_events$region==input$region,]
            }
            
            # filter by the events where we have nightlights information
            cn_temp = cn_temp[cn_temp$date_start > "2012-04-01",]
            
            # update start and end dates
            parameter_startDate = as.Date(min(cn_temp[,20]))
            if (parameter_startDate < as.Date("2012-04-01")) { parameter_startDate = as.Date("2012-04-01") }
            parameter_endDate = as.Date(max(cn_temp[,21]))
            
        } else {
            
            if (input$type == "All") {
                
                cn_temp = cn_events[cn_events$region==input$region &
                                        cn_events$conflict_name==input$conflict,]
            }
            
            else {
                
                cn_temp = cn_events[cn_events$type_of_violence==input$type &
                                        cn_events$region==input$region &
                                        cn_events$conflict_name==input$conflict,]
            }
            
            # filter by the events where we have nightlights information
            cn_temp = cn_temp[cn_temp$date_start > "2012-04-01",]
            
            # update start and end dates
            parameter_startDate = as.Date(min(cn_temp[,20]))
            if (parameter_startDate < as.Date("2012-04-01")) { parameter_startDate = as.Date("2012-04-01") }
            parameter_endDate = as.Date(max(cn_temp[,21]))
        }
        
        
        # Filter by precision of the events
        if (input$precision[1] == "Low" & input$precision[2] == "Low") {
            
            cn_temp = cn_temp %>% filter(precision_level == "Low")
            
        } else {
            
            
            if (input$precision[1] == "Medium" & input$precision[2] == "Medium") {
                
                cn_temp = cn_temp %>% filter(precision_level == "Medium")
                
            } else {
                
                
                if (input$precision[1] == "High" & input$precision[2] == "High") {
                    
                    cn_temp = cn_temp %>% filter(precision_level == "High")
                    
                } else {
                    
                    
                    if (input$precision[1] == "Low" & input$precision[2] == "Medium") {
                        
                        cn_temp = cn_temp %>% filter(precision_level == "Low" |
                                                         precision_level == "Medium")
                        
                    } else {
                        
                        
                        if (input$precision[1] == "Medium" & input$precision[2] == "High") {
                            
                            cn_temp = cn_temp %>% filter(precision_level == "Medium" |
                                                             precision_level == "High")
                        }}}}}
        
        
        # Prepare the dataframe for analysis
        if (nrow(cn_temp)!=0) {
            
            cn_locations = cn_temp %>%
                group_by(conflict_name,longitude, latitude) %>%
                summarise(n_events = n(), 
                          c_deaths = sum(deaths_civilians),
                          u_deaths = sum(deaths_unknown),
                          t_deaths = sum(deaths_total),
                          t_conflict = type_of_violence,
                          region = region)
            
            # add length to the locations
            cn_temp2 = cn_temp %>%
                group_by(longitude, latitude) %>%
                summarise(start = min(date_start), 
                          end = max(date_end))
            
            # we add one to count one day events
            cn_temp2$length = round(as.numeric(difftime(cn_temp2$end, cn_temp2$start, unit="days")))+1
            
            # merge to the locations
            cn_locations = merge(cn_locations, cn_temp2)
            
            # Calculate the severity of each event based on the previous system
            cn_locations$severity_score = 0
            cn_locations$severity_level = ""
            
            for (i in 1:nrow(cn_locations)) {
                
                cn_scoreLength = case_when(
                    cn_locations[i,12] == 0 ~ 0,
                    cn_locations[i,12] < 8 ~ 3,
                    cn_locations[i,12] < 31 ~ 5,
                    TRUE ~ 10
                )
                
                cn_scoreCivilians = case_when(
                    cn_locations[i,5] == 0 ~ 0,
                    cn_locations[i,5] < 10 ~ 20,
                    cn_locations[i,5] < 31 ~ 30,
                    TRUE ~ 40
                )
                
                cn_scoreUnknown = case_when(
                    cn_locations[i,6] == 0 ~ 0,
                    cn_locations[i,6] < 10 ~ 15,
                    cn_locations[i,6] < 31 ~ 20,
                    TRUE ~ 30
                )
                
                cn_scoreTotal = case_when(
                    cn_locations[i,7] == 0 ~ 0,
                    cn_locations[i,7] < 10 ~ 10,
                    cn_locations[i,7] < 31 ~ 15,
                    TRUE ~ 20
                )
                
                cn_locations[i, 13] = cn_scoreLength + cn_scoreCivilians + cn_scoreUnknown + cn_scoreTotal
                
                # Assign the level of severity based on the previous score
                cn_locations[i,14] = case_when(
                    cn_locations[i,13] < 20 ~ "Low",
                    cn_locations[i,13] < 50 ~ "Medium",
                    TRUE ~ "High"
                )
                
            }
            
            # Convert severity_level in an ordered factor
            cn_locations$severity_level = factor(cn_locations$severity_level, ordered = TRUE)
            
            coordinates(cn_locations) <- ~longitude+latitude
            proj4string(cn_locations) <- proj4string(tw_geo)
            
            # remove the variables that we don't need
            rm(cn_temp, cn_scoreLength, cn_scoreCivilians, cn_scoreUnknown, cn_scoreTotal, cn_temp2)
            
            cn_locations
            
        } else {
            
            NULL
        }
        
    })

    # UPDATE CLUSTERS INFORMATION
    clusters_data <- reactive ({
        
        rg_clusters <- rg_clusters[rg_clusters$region==input$region,]
        sf::st_crs(rg_clusters) <- CRS.string
        rg_clusters
    })
    
    # UPDATE KPI1 INFORMATION
    kpi1data <- reactive({
        
        if (!is.null(input$region)) {parameter_region = input$region}
        
        list(
            
            lights_score = mean(nl_changes[nl_changes$region == parameter_region, 6], na.rm=TRUE),
            lights_level = case_when(
                lights_score < -0.30 ~ "Plummeted",
                lights_score < -0.05 ~ "Decreased",
                lights_score < 0.05 ~ "Similar",
                lights_score < 0.30 ~ "Increased",
                TRUE ~ "Rocketed"
            )
            
        )
        
    })
    
    # UPDATE KPI2 INFORMATION
    kpi2data <- reactive ({
        
        if (!is.null(input$region)) {parameter_region = input$region}
        
        list(
            
            severity_score = mean(cn_events[cn_events$region == parameter_region, 30], na.rm=TRUE),
            severity_level = case_when(
                severity_score < 20 ~ "Low",
                severity_score < 50 ~ "Medium",
                TRUE ~ "High"
            )
            
        )
        
    })
    
    # UPDATE POPULATION INFORMATION
    pp_data <- reactive ({
        
        pp_changes[pp_changes$region == input$region,]
        
    })
    
    # API button
    #api_data <- eventReactive(input$b_conflict, {
    #})
    
    ### Map Output -------------------------------------------------------------
    
    # draw the base map
    output$map <- renderTmap(
        
        
        if (!rlang::is_empty(locations_data())) {
            tm <- 
                # base map
                # https://leaflet-extras.github.io/leaflet-providers/preview/
                tm_basemap(server=leaflet::providers$Stamen.Toner, 
                           alpha=0.15) +
                
                # region borders
                tm_shape(st_data()) +
                tm_borders(col="black", 
                           lwd=2,
                           alpha = 1) +
                
                # add township layer to show additional information
                
                # nightlights layer
                tm_shape(nl_dataChanges()) + 
                tm_fill(col="light_level",
                        breaks = lightsBreaks,
                        labels = names(lightsPalette),
                        palette = lightsPalette,
                        legend.show = FALSE,
                        zindex = 402) +
                tm_borders(col = "grey", 
                           lwd = 1, 
                           lty="dotted",
                           alpha = 0.5,
                           zindex = 403) +
                tm_add_legend('fill', 
                              col = c("#00dfff","#D6EFF6","#F2F3EC","#fdfce1","#fbf79b"),
                              labels = c("Rocketed", "Increased", "Similar", "Decreased", "Plummeted"),
                              reverse = TRUE,
                              border.col = "grey40",
                              title="Lights Levels       ..",
                              z=1,
                              zindex = 401) +
                
                # locations layer
                tm_shape(locations_data()) +
                tm_dots(col="severity_level",
                        alpha = 0.7,
                        palette=severityPalette,
                        size=0.05,
                        border.col="white",
                        border.lwd=2,
                        zindex = 404,
                        legend.show = FALSE) +
                tm_add_legend('fill', 
                              col = c("#FF0000","#FF8C00","#C0C0C0"),
                              border.col = "grey40",
                              labels = c('High','Medium','Low'),
                              title="Severity Levels",
                              z=2,
                              zindex = 402) +
                
                # clusters layer
                # for the color try to do it with an if (severity) loop and a proxy
                # that will plot the border based on the severity (maybe the parameter if the function?)
                tm_shape(clusters_data()) + 
                tm_borders(col="grey",
                           lty="dashed",
                           lwd=2) +
                
                tm_view(control.position=c("right","bottom"),
                        view.legend.position=c("left","bottom"))
            tm
            
        } else {
            
            tm <- 
                
                # regional shape
                tm_shape(st_data()) +
                tm_borders(col="black", 
                           lwd=2,
                           alpha = 1) +
                
                # add township layer to show additional information
                
                
                # nightlights layer
                tm_shape(nl_dataChanges()) + 
                tm_fill(col="light_level",
                        breaks = lightsBreaks,
                        labels = names(lightsPalette),
                        palette = lightsPalette,
                        legend.show = FALSE,
                        zindex = 402) +
                tm_borders(col = "grey", 
                           lwd = 1, 
                           lty="dotted",
                           alpha = 0.5,
                           zindex = 403) +
                tm_add_legend('fill', 
                              col = c("#00dfff","#D6EFF6","#F2F3EC","#fdfce1","#fbf79b"),
                              labels = c("Rocketed", "Increased", "Similar", "Decreased", "Plummeted"),
                              reverse = TRUE,
                              border.col = "grey40",
                              title="Lights Levels ..",
                              z=1,
                              zindex = 401) +
                
                # general options
                tm_view(control.position=c("right","bottom"),
                        view.legend.position=c("left","bottom"))
            
            tm
            
        }
        
    )    
    ### Explore Output ---------------------------------------------------------

    # KPI-1
    output$KPI1 <- renderValueBox({

        level <- kpi1data()$lights_level
        score <- kpi1data()$lights_score

        valueBox(tags$p(paste0(level," lights"), style = "font-size: 40%;"), 
                 subtitle=tags$p(paste0(round(score*100), "% of difference"), style="font-size: 70%;"),
                 icon = icon("sun"), 
                 color="aqua")
    })
    
    # KPI-2
    output$KPI2 <- renderValueBox({
        
        level <- kpi2data()$severity_level
        score <- kpi2data()$severity_score
        
        valueBox(tags$p(paste0(level," severity"), style = "font-size: 40%;"), 
                 subtitle=tags$p(paste0("Scored ", round(score), " over 100"), style="font-size: 70%;"),
                 icon = icon("skull-crossbones"),
                 color="orange")
    })
    
    # Explore Plot
    output$explore_plot <- renderPlot({
        
        draw_exploration_plot(events_data())
        
    }, bg="transparent")

    ### Predict Output ---------------------------------------------------------
    
    output$prediction_plot <- renderPlot({
        
        draw_prediction_plot(pp_data())
        
    }, bg="transparent")

    
    ### Township Click Event ---------------------------------------------------

        
    ### Notifications ----------------------------------------------------------

        
    ### API --------------------------------------------------------------------
    
    
}