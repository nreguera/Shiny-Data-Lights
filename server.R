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
    
    # update locations information layer
    locations_data <- reactive ({
        
        # filter the events by region, type of conflict and conflict name
        if (input$conflict == "All conflicts in the region") {
            
            if (input$type == "All" ) {
                
                cn_temp = cn_events[cn_events$region==input$region,]
                
            } else {
                
                cn_temp = cn_events[cn_events$type_of_violence==input$type &
                                        cn_events$region==input$region,]
            }
            
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
            cn_temp = cn_events %>%
                group_by(longitude, latitude) %>%
                summarise(start = min(date_start), 
                          end = max(date_end))
            
            # we add one to count one day events
            cn_temp$length = round(as.numeric(difftime(cn_temp$end, cn_temp$start, unit="days")))+1
            
            # merge to the locations
            cn_locations = merge(cn_locations, cn_temp)
            
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
            
            cn_locations
        
        } else {
            
            NULL
        }
        
    })
    
    # update KPI1
    kpi1data <- reactive ({
        
        return(
            list(
                
                lights_score = mean(nl_changes[nl_changes$region == input$region, 6], na.rm=TRUE),
                lights_level = case_when(
                    lights_score < -0.3 ~ "Plummeted",
                    lights_score < -0.05 ~ "Decreased",
                    lights_score < 0.05 ~ "Similar",
                    lights_score < 0.3 ~ "Increased",
                    TRUE ~ "Rocketed"
                )
                
            )
        )
        
    })
    
    # update KPI2
    kpi2data <- reactive ({
        
        return(
            list(
                
                severity_score = mean(cn_events[cn_events$region == input$region, 30], na.rm=TRUE),
                severity_level = case_when(
                    severity_score < 20 ~ "Low",
                    severity_score < 50 ~ "Medium",
                    TRUE ~ "High"
                )
                
            )
        )
        
    })
    
    # regional borders
    st_data <- reactive({
        
        st_geo[st_geo@data$NAME_1==input$region,]
        
    })
    
    # events information
    events_data <- reactive({
        
        events_data = cn_events[cn_events$region==input$region,]
        events_data = events_data[events_data$date_start >= parameter_startDate,]
        events_data = as.data.frame(events_data)
        events_data
        
    })
    
    # night lights information
    nl_dataChanges <- reactive({
        
        # We update the changes of conflicts when selected, if not, it shows for all the regions
        if (input$conflict != "All conflicts in the region") {

            parameter_startDate = as.Date(min(cn_conflicts[cn_conflicts$conflict_name==input$conflict, 5]))
            parameter_endDate = as.Date(max(cn_conflicts[cn_conflicts$conflict_name==input$conflict, 6]))
            
        } else {
            
            parameter_startDate = as.Date(min(cn_conflicts[cn_conflicts$region==input$region, 5]))
            parameter_endDate = as.Date(max(cn_conflicts[cn_conflicts$region==input$region, 6]))

        }
        
        if (parameter_startDate < "2012-04-01") {parameter_startDate=as.Date("2012-04-01")}
        
        # select the first day of each month
        parameter_startDate = as.Date(cut(parameter_startDate, "month"))
        parameter_endDate = as.Date(cut(parameter_endDate, "month"))
        
        # Changes are normalized by year (like we do with  %  of interest of money):
        # the readings change will be divided by the years between the first and last date
        parameter_length <- as.numeric(difftime(parameter_endDate, parameter_startDate, unit="weeks"))/52.25
        
        nl_changes = nl_data[which(nl_data$region==input$region),]
        nl_changes = nl_changes[(nl_changes$date==parameter_startDate | nl_changes$date==parameter_endDate),]
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
    
    # locations information

    
    # prediction information
    md_data <- reactive ({
        
        md_df_results[md_df_results$region == input$region,]
        
    })
    
    # population changes information
    pp_data <- reactive ({
        
        pp_changes[pp_changes$region == input$region,]
        
    })
    
    ### Map Output -------------------------------------------------------------
    
    # draw the base map
    output$map <- renderTmap(
        
        if (!rlang::is_empty(locations_data())) {
            tm <- tm_shape(st_data()) +
                tm_borders(col="black", 
                           lwd=2,
                           alpha = 1) +
                
                # add township layer to show additional information
                
                # nightlights layer
                tm_shape(nl_dataChanges()) + 
                tm_fill(col="light_level",
                        breaks = changesBreaks,
                        labels = names(changesPalette),
                        palette = changesPalette,
                        legend.show = FALSE,
                        zindex = 402) +
                tm_borders(col = "black", 
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
                tm_bubbles(col="severity_level",
                           alpha = 0.7,
                           palette=severityPalette,
                           size="n_events",
                           border.col="grey",
                           border.lwd=1,
                           scale = 1,
                           zindex = 404,
                           legend.size.show = FALSE,
                           legend.col.show = FALSE) +
                tm_add_legend('fill', 
                              col = c("#FF0000","#FF8C00","#C0C0C0"),
                              border.col = "grey40",
                              labels = c('High','Medium','Low'),
                              title="Severity Levels",
                              z=2,
                              zindex = 402) +
                
                tm_view(control.position=c("right","bottom"),
                        view.legend.position=c("left","bottom"))
            
            tmap_mode("view")
            
            tm
            
        } else {
            
            tm_shape(st_data()) +
                tm_borders(col="black", 
                           lwd=2,
                           alpha = 1) +
                
                # add township layer to show additional information
                
                
                # nightlights layer
                tm_fill(col="light_level",
                        breaks = changesBreaks,
                        labels = names(changesPalette),
                        palette = changesPalette,
                        legend.show = TRUE,
                        zindex = 402) +
                tm_borders(col = "black", 
                           lwd = 1, 
                           lty="dotted",
                           alpha = 0.5,
                           zindex = 403) +
                tm_add_legend('fill', 
                              col = c("#00dfff","#D6EFF6","#F2F3EC","#fdfce1","#fbf79b"),
                              labels = c("Rocketed", "Increased", "Similar", "Decreased", "Plummeted"),
                              reverse = TRUE,
                              border.col = "grey40",
                              title="Lights Levels   ",
                              z=1) +
                
                tm_view(control.position=c("right","bottom"),
                        view.legend.position=c("left","bottom"))
            
            tmap_mode("view")
            
            tm
            
        }

        
    )
    
    # update regional border
    observe({
        
        if (!rlang::is_empty(st_data())) {
            
            draw_region_map("map", st_data(), session)
        }
    })

    # update nightlights information layer
    observe({
        
        if (!rlang::is_empty(nl_dataChanges())) {
           
            draw_nl_layer("map", nl_dataChanges(), session)
        }
    })
    
    # update locations information layer
    observe({
        
        # this gaves an error with cn_locations
        #if (!rlang::is_empty(locations_data())) {
        #    
        #    draw_events_layer("map", location_data(), session)
        #
        #}
    })
    
    ### Explore Output ---------------------------------------------------------

    # KPI-1
    output$KPI1 <- renderValueBox({

        .tmp1 <- kpi1data()
        w <- .tmp1$lights_level
        x <- .tmp1$lights_score

        valueBox(tags$p(paste0(w," lights"), style = "font-size: 40%;"), 
                 subtitle=tags$p(paste0(round(x*100), "% of difference"), style="font-size: 70%;"),
                 icon = icon("sun"), 
                 color="aqua")
    })
    
    # KPI-2
    output$KPI2 <- renderValueBox({
        
        .tmp2 <- kpi2data()
        y <- .tmp2$severity_level
        z <- .tmp2$severity_score
        
        valueBox(tags$p(paste0(y," severity"), style = "font-size: 40%;"), 
                 subtitle=tags$p(paste0("Scored ", round(z), " over 100"), style="font-size: 70%;"),
                 icon = icon("skull-crossbones"),
                 color="orange",
                 width=4)
    })
    
    # Explore plot
    explore_plot <- reactive({
        
        p <- ggplot(data=events_data(), 
                    aes(x=date_start, y=severity_score, colour=severity_level)) +
            geom_point(size=4, 
                       stroke=0,
                       shape=16,
                       alpha=0.3) +
            geom_smooth(method = "lm", alpha=0.2, aes(fill = severity_level)) +
            theme(
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
            scale_y_continuous(breaks = integer_breaks()) +
            scale_x_date(limits = as.Date(c(parameter_startDate, parameter_endDate)) 
        )
            
            p + severityScale
            
        # update KPI1
        
        # update KPI2
        
    })
    
    # Plot
    output$explore_plot <- renderPlot({
        
        # check if there are events in the selected region
        if (is.data.frame(events_data()) && nrow(events_data())==0) {
            # if not, draw and empty plot
            plot.new()
        } else {
            explore_plot()
        }
        
    }, bg="transparent")

    ### Predict Output ---------------------------------------------------------
    
    output$prediction_plot <- renderPlotly({
        

        p <- ggplot(pp_data(), aes(month, township, fill=light_level)) +
            geom_tile(color="grey", size=0.1) + 
            scale_fill_manual(values=changesPalette, na.value="white") +
            facet_grid(.~year)
        
        p <- p + theme(
            panel.spacing.x=unit(0.01, "lines"), 
            panel.spacing.y=unit(0.01, "lines"),
            panel.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
            plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
            plot.title=element_blank(),
            strip.background = element_blank(),
            legend.position = "none",
            axis.title.y=element_text(size=7, colour="dark grey"),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text=element_blank(),
            strip.text.x = element_text(size=6, colour="dark grey", angle=90),
        ) +
            xlab("") + 
            removeGrid()
        
        ggplotly(p) 
        
    })
    
    ### Township Click Event ---------------------------------------------------

        
    ### Notifications ----------------------------------------------------------

        
    ### API --------------------------------------------------------------------
    
    
}