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
    
    # update locations information
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
                group_by(conflict_name, longitude, latitude) %>%
                summarise(n_events = n(), 
                          t_deaths = sum(deaths_total),
                          c_deaths = sum(deaths_civilians),
                          t_conflict = type_of_violence,
                          region = region)
            
            coordinates(cn_locations) <- ~longitude+latitude
            proj4string(cn_locations) <- proj4string(tw_geo)
            
            cn_locations
        
        } else {
            NULL
            }
        
    })
    
    # update KPI2
    kpi2data <- reactive ({
        
        return(
            list(
                
                score = mean(cn_events[cn_events$region == input$region, 30]),
                level = case_when(
                    score < 20 ~ "Low",
                    score < 50 ~ "Medium",
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
    nl_data <- reactive({

        nl_data = tw_geo[tw_geo@data$NAME_1==input$region,]
        nl_data@data$change = nl_changes[which(nl_changes$region==input$region), 6]
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
            
            # nightlights layer
            tm_shape(nl_data()) + 
            tm_fill(col = "change",
                    breaks = changesBreaks,
                    labels = names(changesPalette),
                    palette = changesPalette,
                    contrast = c(0,1),
                    zindex = 402,
                    legend.show = TRUE,
                    legend.z = 1) +
            tm_borders(col = "black", 
                       lwd = 1, 
                       lty="dotted",
                       alpha = 0.5,
                       zindex = 403) +
            tm_layout(legend.show=TRUE,
                      "",
                      legend.title.size = NA,
                      legend.text.size = 0.2,
                      legend.position = c("left","bottom"),
                      legend.bg.color = "white",
                      legend.bg.alpha = 1) +
            
            # locations layer
            tm_shape(locations_data()) +
            tm_bubbles("n_events",
                       alpha = 0.7,
                       scale = 1,
                       zindex = 404) +
            tm_layout(legend.show=FALSE) +
            
                tm_view(control.position=c("right","bottom"),
                        view.legend.position=c("left","bottom"))
            
            # Pipe the tmap object into tmap_leaflet() to create a leaflet widget,
            # so that we can use leaflet::hideGroup().
            #tm = tm %>% 
            #    tmap_leaflet() #%>%
                #leaflet(options = leafletOptions(zoomControl = FALSE)) # %>%
               #addControl(actionButton("zoomer","Reset"),position="topright")
            
            tm
            
        } else {
            
            tm_shape(st_data()) +
                tm_borders(col="black", 
                           lwd=2,
                           alpha = 1) +
                
                # nightlights layer
                tm_shape(nl_data()) + 
                tm_fill(col = "change",
                        breaks = changesBreaks,
                        labels = names(changesPalette),
                        palette = changesPalette,
                        contrast = c(0,1),
                        zindex = 402,
                        legend.show = TRUE,
                        legend.z = 1) +
                tm_borders(col = "black", 
                           lwd = 1, 
                           lty="dotted",
                           alpha = 0.5,
                           zindex = 403) +
                tm_layout(legend.show=TRUE,
                          "",
                          legend.title.size = 0,
                          legend.text.size = 0.6,
                          legend.position = c("left","bottom"),
                          legend.bg.color = "white",
                          legend.bg.alpha = 1)
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
        
        if (!rlang::is_empty(nl_data())) {
           
            draw_nl_layer("map", nl_data(), session)
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

        valueBox(tags$p(paste0("Similar"," lights"), style = "font-size: 40%;"), 
                 subtitle=tags$p(paste0("1%", " difference"), style = "font-size: 70%;"),
                 icon = icon("sun"), 
                 color="aqua")   })
    
    # KPI-2
    output$KPI2 <- renderValueBox({
        
        .tmp <- kpi2data()
        x <- .tmp$level
        y <- .tmp$score
        
        valueBox(tags$p(paste0(x," severity"), style = "font-size: 40%;"), 
                 subtitle=tags$p(paste0("Scored ", round(y), " over 100"), style = "font-size: 70%;"),
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
                axis.ticks = element_blank()
            ) +
            xlab("") +
            ylab("") +
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
        

        p <- ggplot(pp_data(), aes(month, township, fill=as.factor(change_level))) +
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
 #           ylab("") +
            removeGrid()
        
        ggplotly(p) 
        
    })
    
    ### Township Click Event ---------------------------------------------------

        
    ### Notifications ----------------------------------------------------------

        
    ### API --------------------------------------------------------------------
    
    
}