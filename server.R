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
}

shinyServer(my.server)