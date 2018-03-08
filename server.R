library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(maps)
library(fiftystater)
library(leaflet)
library(ggmap)
library(mapdata)
source("Highway_Data.R")

finance <- read.csv("data/finance.csv", stringsAsFactors = FALSE, strip.white = TRUE)

finance2 <- apply(finance, 2, gsub, pattern = ",", replacement = "")
finance2 <- apply(finance2, 2, gsub, pattern = "4/", replacement = "")
finance2 <- data.frame(finance2, stringsAsFactors = F)

finance2[,2] <- as.numeric(finance2[, 2])
finance2[,3] <- as.numeric(finance2[, 3])
finance2[,4] <- as.numeric(finance2[, 4])
finance2[,5] <- as.numeric(finance2[, 5])
finance2[,6] <- as.numeric(finance2[, 6])
finance2[,7] <- as.numeric(finance2[, 7])
finance2[,8] <- as.numeric(finance2[, 8])
finance2[,9] <- as.numeric(finance2[, 9])
finance2[,10] <- as.numeric(finance2[, 10])
finance2[,11] <- as.numeric(finance2[, 11])
finance2[,12] <- as.numeric(finance2[, 12])
finance2[,13] <- as.numeric(finance2[, 13])
finance2[,14] <- as.numeric(finance2[, 14])
finance2[,15] <- as.numeric(finance2[, 15])
finance2[,16] <- as.numeric(finance2[, 16])
finance2[,17] <- as.numeric(finance2[, 17])
finance2[,18] <- as.numeric(finance2[, 18])
finance2[,19] <- as.numeric(finance2[, 19])
finance2[,20] <- as.numeric(finance2[, 20])
finance2[,21] <- as.numeric(finance2[, 21])
finance2[,22] <- as.numeric(finance2[, 22])
finance2[,23] <- as.numeric(finance2[, 23])
finance2[,24] <- as.numeric(finance2[, 24])
finance2[,25] <- as.numeric(finance2[, 25])
finance2[,26] <- as.numeric(finance2[, 26])
finance2[,27] <- as.numeric(finance2[, 27])
finance2[,28] <- as.numeric(finance2[, 28])
finance2[,29] <- as.numeric(finance2[, 29])
finance2[,30] <- as.numeric(finance2[, 30])
finance2[,31] <- as.numeric(finance2[, 31])
finance2[,32] <- as.numeric(finance2[, 32])
finance2[,33] <- as.numeric(finance2[, 33])

finance2 <- mutate(finance2, "2014" = X2014Maintenance / X2014CapitalOutlay)
finance2 <- mutate(finance2, "2013" = X2013Maintenance / X2013CapitalOutlay)
finance2 <- mutate(finance2, "2012" = X2012Maintenance / X2012CapitalOutlay)
finance2 <- mutate(finance2, "2011" = X2011Maintenance / X2011CapitalOutlay)
finance2 <- mutate(finance2, "2010" = X2010Maintenance / X2010CapitalOutlay)
finance2 <- mutate(finance2, "2009" = X2009Maintenance / X2009CapitalOutlay)
finance2 <- mutate(finance2, "2008" = X2008Maintenance / X2008CapitalOutlay)
finance2 <- mutate(finance2, "2007" = X2007Maintenance / X2007CapitalOutlay)
finance2 <- mutate(finance2, "2006" = X2006Maintenance / X2006CapitalOutlay)
finance2 <- mutate(finance2, "2005" = X2005Maintenance / X2005CapitalOutlay)
finance2 <- mutate(finance2, "2004" = X2004Maintenance / X2004CapitalOutlay)
finance2 <- mutate(finance2, "2003" = X2003Maintenance / X2003CapitalOutlay)
finance2 <- mutate(finance2, "2002" = X2002Maintenance / X2002CapitalOutlay)
finance2 <- mutate(finance2, "2001" = X2001Maintenance / X2001CapitalOutlay)
finance2 <- mutate(finance2, "2000" = X2000Maintenance / X2000CapitalOutlay)
finance2 <- mutate(finance2, "1999" = X1999Maintenance / X1999CapitalOutlay)
finance2$State <- sapply(finance2[, 1], tolower)

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
  
  output$Vehicle_Miles <- renderPlot(GetDataCoordinates(GetMilesData(input$yr), "Vehicle Miles (Millions of Miles)", input$state))
  output$Motor_Fuel <- renderPlot(GetDataCoordinates(GetStateFuel(input$yr), "Motor-Fuel use (Thousands of Gallons)", input$state))
  output$revenue <- renderPlot(GetDataCoordinates(GetRevenueData(input$yr), "Revenue Used For Roads (Thousands of Dollars)", input$state))
  output$coorelation <- renderText({
    coeff <- GetCoorelationCoefficent(input$range1, input$state)
    if (is.na(coeff)) {
      return(" ")
    }
    return(paste0("The coorelation coefficient, showing the relationship between vehicle miles, motor-fuels and revenue used on roads, for ", input$state, " from the year ", input$range1[1], " to the year ", input$range1[2], " is ", round(coeff, 5), "."))
  })
  
  selected.filtered <- reactive({
    #print(finance2[, as.character(input$year[1])])
    filtered <- mutate(finance2, difference = 
                         round(100 * (finance2[, as.character(input$year1[2])] - 
                                        finance2[, as.character(input$year1[1])])) 
    ) %>%  
      select(State, difference)
    return(filtered)
  })
  
  output$plot1 <- renderPlot({
    map <- map_data("state")
    map.finance <- left_join(map, selected.filtered(), by = c("region" = "State"))
    #print(selected.filtered())
    break.point <- c(-90, -40, -30, -20, -10, 0.5, 10, 20, 30, 40, 90)
    classes <- cut(map.finance$difference, break.point)
    #print(map.finance)
    ggplot(data = map.finance) +
      geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = classes)) +
      coord_quickmap() +
      scale_fill_brewer(type = "seq", palette = "Greens")
  })
  
}

shinyServer(my.server)