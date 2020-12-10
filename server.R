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
    
    # regional shape
    st_data <- reactive({
        region <- input$region
        if(is.null(region)) {
            region <- "Kachin"
        }
        
        st_geo[st_geo@data$NAME_1==region,]
        
    })
    
    # night lights information
    nl_data <- reactive({
        region <- input$region
        if(is.null(region)) {
            region <- "Kachin"
        }

        nl_data = tw_geo[tw_geo@data$NAME_1==region,]
        nl_data@data$change = nl_changes[which(nl_changes$region==region), 6]
        nl_data
    })
    
    # events information
    ev_data <- reactive({
        region <- input$region
        if(is.null(region)) {
            region <- "Kachin"
        }
        
        ev_data = cn_events_geo[cn_events_geo@data$NAME_1==region,]
        ev_data
    })
    
    # data for the exploration plot
    load_cn_data <- reactive({
        
        cn_explore = cn_events_explore[cn_events_explore$NAME_1==input$region,]
        cn_explore = cn_explore[cn_explore$date_start >= parameter_date,]
        cn_explore = as.data.frame(cn_explore)
        cn_explore

        
    })
    
    # data for the prediction plot
    md_data <- reactive ({
        
        region <- input$region
        
        md_final[md_final$region == region,]

    })
    
    ### Map Output -------------------------------------------------------------
    
    # draw the map when
    output$map <- renderTmap(
        
        if (!rlang::is_empty(ev_data())) {
        tm <- tm_shape(st_data()) +
            tm_borders(col="black", 
                       lwd=2,
                       alpha = 1) +
            
            # nightlights layer
            tm_shape(nl_data()) + 
            tm_fill(col = "change",
                    breaks = c(-1, -0.3, -0.05, 0.05, 0.3, 1),
                    labels = c("Strong Decrease", "Decrease", "Similar", "Increase", "Strong Increase"),
                    palette = "RdYlGn",
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
                      "Wealth (or so)",
                      legend.title.size = 1,
                      legend.text.size = 0.6,
                      legend.position = c("left","bottom"),
                      legend.bg.color = "white",
                      legend.bg.alpha = 1) +
            
            # events layer
            tm_shape(ev_data()) +
            tm_bubbles(col = "black",
                       alpha = 0.5,
                       size = 0.25,
                       zindex = 404) +
            tm_layout(legend.show=FALSE) +
                tm_view(control.position=c("right","bottom"),
                        view.legend.position=c("left","bottom"))
            
            # Pipe the tmap object into tmap_leaflet() to create a leaflet widget,
            # so that we can use leaflet::hideGroup().
            #tm = tm %>% 
            #    tmap_leaflet() %>%
            #    leaflet::hideGroup(ev_data())
            tm
            
        } else {
            
            tm_shape(st_data()) +
                tm_borders(col="black", 
                           lwd=2,
                           alpha = 1) +
                
                # nightlights layer
                tm_shape(nl_data()) + 
                tm_fill(col = "change",
                        breaks = c(-1, -0.3, -0.05, 0.05, 0.3, 1),
                        labels = c("Strong Decrease", "Decrease", "Similar", "Increase", "Strong Increase"),
                        palette = "RdYlGn",
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
                          "Wealth (or so)",
                          legend.title.size = 1,
                          legend.text.size = 0.6,
                          legend.position = c("left","bottom"),
                          legend.bg.color = "white",
                          legend.bg.alpha = 1)
        }

    )
    
    # update regional shape
    observe({
        
        if (!rlang::is_empty(st_data())) {
            
            draw_region_map("map", st_data(), session)
        }
    })

    # update nightlights information
    observe({
        
        if (!rlang::is_empty(nl_data())) {
           
            draw_nl_layer("map", nl_data(), session)
        }
    })
    
    # update events information
    observe({
        
        if (!rlang::is_empty(ev_data())) {
            
            draw_events_layer("map", ev_data(), session)
            
        }
    })
    
    ### Explore Output ---------------------------------------------------------

    # KPI-1
    output$KPI1 <- renderValueBox({
        valueBox(tags$p(paste0(90,"%"), style = "font-size: 50%;"), 
                 subtitle=tags$p("Impact of events in the area", style = "font-size: 70%;"),
                 icon = icon("skull-crossbones"), 
                 color="black",
                 width=4)
    })
    
    # KPI-2
    output$KPI2 <- renderValueBox({
        valueBox(tags$p(paste0(50,"%"), style = "font-size: 50%;"), 
                 subtitle=tags$p("Lights change in the area", style = "font-size: 70%;"),
                 icon = icon("sun"), 
                 color="black")
    })
    
    # Explore plot
    explore_plot <- reactive({
        
        ggplot(data=load_cn_data(), aes(x=date_start, y=deaths_total, group=1)) +
            geom_point(color="red", size=2) +
            geom_smooth(method = "lm") +
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
            ylab("") 
            
    })
    
    # Plot
    output$explore_plot <- renderPlot({
        
        # check if there are events in the selected region
        if (is.data.frame(load_cn_data()) && nrow(load_cn_data())==0) {
            # if not, draw and empty plot
            plot.new()
        } else {
            explore_plot()
        }
        
    }, bg="transparent")

    ### Predict Output ---------------------------------------------------------
    
    output$prediction_plot <- renderPlotly({
        

        p <- ggplot(md_data(), aes(month, township, fill=population)) +
            geom_tile(color="white", size=0.5) + 
            scale_fill_viridis(name="Population", option="E") +
            facet_grid(.~year)
        
        p <- p + theme(
            panel.spacing.x=unit(0.05, "lines"), 
            panel.spacing.y=unit(0.01, "npc"),
            panel.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
            plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
            plot.title=element_blank(),
            legend.position = "none",
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text=element_text(size=10),
            strip.background = element_blank()
        ) +
            xlab("") + 
            ylab("") +
            removeGrid()
        
        ggplotly(p)

    })
    
    ### Township Click Event ---------------------------------------------------

        
    ### Notifications ----------------------------------------------------------
    
}