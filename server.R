library("ggplot2")
library("maps")
library("fiftystater")

# # deifnes my.server by taking in state name and year as
# arguments and returns a table with filtered data
my.server <- function(input, output) {
  create.table <- function(Year){
    table <- data %>%
      select(States, Year)
    return(table)
  }
  
  # returns a table with choices from ui
  output$table <- renderDataTable({
    return(create.table(input$Range))
  })
  
  # includes an explanation about the data in
  # the table for usability
  output$Explanation <- renderText({
    paste("The information displayed on the table 
          shows the number of fatalities casued by 
          motor vehicles in each state during specific
          years.")
    
  })
  
  # creates a chloropleth map with a legend that 
  # is color coordinated for each country by 
  # carbon emissions
  output$map <- renderPlot({
    states <- map_data("state")
    colnames(states)[5] <- "States"
    table <- create.table(input$Range)
    table[, "States"] <- tolower(table[, "States"])
    table[, "States"] <-trimws(table[, "States"], which = c("right"))
    map.states <- left_join(states, table, 
                              by = "States")
    map.states[, 7] <- as.numeric(gsub(",", "", map.states[,7]))
    ggplot(map.states, aes(long, lat, 
                              group = group, 
                              fill = cut(map.states[ ,7], 
                                      breaks = 5))) +
      geom_polygon(color = "grey10", show.legend = TRUE) +
      scale_fill_brewer(palette = "Reds") +
      guides(fill = guide_legend(title = "Fatalities")) +
      ggtitle("Fatalities caused by motor vehicles")
    
  })
  
  # includes an explanation about how to use the 
  # widgets for the map
  output$map.explanation <- renderText({
    paste("Choose the year for which you want the map
          to display the number of fatalities in each 
          state.")
  })
  # prints lattitude and longitude values when user
  # click on the map
  output$print <- renderText ({
    paste(input$plot_click$x, ", ", input$plot_click$y)
  })
  
  }
