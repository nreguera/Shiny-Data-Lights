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
    
    # Set UI choices and selections --------------------------------------------
    
    
    # Data Manipulations -------------------------------------------------------
    
    # update the data for the plot
    load_cn_data <- reactive({
        x <- cn_data
    })
    
    # Map Output ---------------------------------------------------------------
    
    # draw the map with the preset parameters
    output$map <- renderTmap(
        draw_base_map()
    )

    # update current map when region changes
    output$map <- renderTmap(
        update_region_map("map", input$region)
    )
    
    # Explore Output -----------------------------------------------------------

    # update the data for the plot
    my_line_plot <- reactive({
        ggplot(data=load_cn_data(), aes(x=as.numeric(year), y=deaths_total, group=1)) +
            geom_line() +
            geom_point() +
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank())
    })
    
    # Plot
    output$line_plot <- renderPlot({
        my_line_plot()
    })
    
    # KPI-1
    output$KPI1 <- renderValueBox({
        valueBox(tags$p("90", style = "font-size: 50%;"), 
                 subtitle=tags$p("Deaths linked to the conflict", style = "font-size: 70%;"),
                 icon = icon("skull-crossbones"), 
                 color="blue",
                 width=4)
    })
    
    # KPI-2
    output$KPI2 <- renderValueBox({
        valueBox(tags$p(paste0(80,"%"), style = "font-size: 50%;"), 
                 subtitle=tags$p("Nightlights level change", style = "font-size: 70%;"),
                 icon = icon("sun"), 
                 color="aqua")
    })
    
    # Predict Output -----------------------------------------------------------
    
    
    # Township Click Event -----------------------------------------------------

        
    # Notifications ------------------------------------------------------------
    
}