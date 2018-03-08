library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(maps)
library(fiftystater)

my.server <- function(input, output) {
  fuel.data <- read.csv('fuel_data.csv', stringsAsFactors = FALSE)
  
  react <- reactive({
    fuel.data %>%
      select(year, input$filter, fuel_use_per_vehicle)
  })
  
  output$table <- renderTable({
    react()
  })
  
  output$plot <- renderPlot({
    x <- select(fuel.data, input$filter)
    y <- select(fuel.data, fuel_use_per_vehicle)
    ggplot(fuel.data, aes(x = x, y = y)) +
      geom_point() +
      geom_smooth() +
      labs(x = paste0(input$filter), y = "fuel_use_per_vehicle", title = paste0(input$filter, " vs. Average Fuel Use Per Vehicle"))
  })
  
  output$info <- renderText({
    paste0("The ", input$filter, " in ", input$year, " was ", fuel.data[input$year - 1949, input$filter], " dollars per gallon.",
           "\n", "The average fuel use per vehicle in ", input$year, " was ", fuel.data[input$year - 1949, "fuel_use_per_vehicle"], " gallons.")
  })
  
  output$more_info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("")
      paste0(input$filter, ": ", round(e$x, 3), "\nfuel use per vehicle: ", round(e$y, 1), "\n")
    }
    paste0(xy_str(input$plot_click))
  })
  
  create.table <- function(Year){
    table <- data %>%
      select(States, Year)
    return(table)
  }
  
  # returns a table with choices from ui
  output$table1 <- renderDataTable({
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

shinyServer(my.server)