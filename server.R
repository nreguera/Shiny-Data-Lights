#
# Server logic.
# It will be called once for each session. 
#

server <- function(input, output, session){
    
    # update the data for the plot
    load_cn_data <- reactive({
        x <- cn_data
    })
    
    # update the data for the plot
    my_line_plot <- reactive({
        ggplot(data=load_cn_data(), aes(x=as.numeric(year), y=deaths_total, group=1)) +
            geom_line()+
            geom_point()+
            ggtitle("\nHistorical Deaths") +
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank())
    })
    
    
    observe({
        
        # Map Output ----------------------------------------------------------
        output$tm <- renderTmap(
            tm_shape(nl_layer) + 
                tm_fill(col = "radiance",
                        n = 15,
                        palette = "cividis",
                        contrast = c(0,1)) +
                
                tm_legend(show=TRUE) + 
                tm_layout("Conflicts Impact",
                          legend.title.size = 1,
                          legend.text.size = 0.6,
                          legend.outside.position = c("left","bottom"),
                          legend.bg.color = "white",
                          legend.bg.alpha = 1,
                          legend.outside = TRUE) +
                tm_layout(frame=FALSE)
        )

        # Explore Output ----------------------------------------------------------
        # KPI-1
        output$KPI1 <- renderValueBox({
            valueBox(tags$p("90", style = "font-size: 80%;"), 
                     subtitle=tags$p("Deaths in the region", style = "font-size: 90%;"),
                     icon = tags$i(class = "fas fa-thumbs-down", style="font-size: 50px"), 
                     color="blue",
                     width=4)
        })
    
        # KPI-2
        output$KPI2 <- renderValueBox({
            valueBox(tags$p(paste0(80,"%"), style = "font-size: 80%;"), 
                     subtitle=tags$p("Nightlights level change", style = "font-size: 90%;"),
                     icon = tags$i(class = "fas fa-thumbs-down", style="font-size: 50px"),
                     color="blue")
        })
        
        # Plot
        output$line_plot <- renderPlot({
            my_line_plot()
        })

        # Prediction Output ----------------------------------------------------------
    })
    
}