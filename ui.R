library("tidyr")
library("dplyr")
library("plotly")

# lists the choices for the dropdownmenu

year.choices <- c("1967", "1968", "1969", "1970", "1971", 
                  "1972", "1973", "1974", "1975", "1976",
                  "1977", "1978", "1979", "1980", "1981", 
                  "1982", "1983", "1984", "1985", "1986", 
                  "1987", "1988", "1989", "1990", "1991",
                  "1992", "1993", "1994", "1995", "1996",
                  "1997", "1998", "1999", "2000", "2001",
                  "2002", "2003", "2004", "2005", "2006",
                  "2007", "2008", "2009", "2010", "2011",
                  "2012", "2013", "2014")


my.ui <- fluidPage(
  navbarPage("U.S. Highway Statistics",
    tabPanel("Gasoline Cost and Fuel Use Information",
             titlePanel("Gasoline Cost and Fuel Use Information"),
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year", "Year:",
                             value = 2005,
                             min = 1950,
                             max = 2011,
                             sep = ""),
                 radioButtons("filter", "Switch between gasoline tax rate and real cost of gas:",
                              c("Gasoline tax rate" = "gas_tax_rate",
                                "Real cost of gas" = "real_cost_of_gas")),
                 br(),
                 p(strong("Guiding Question:")),
                 p("What effect, if any, do the gasoline tax rate and real cost of gas have on fuel use per vehicle across the United States?"),
                 p(strong("Data Analysis:")),
                 p("Based on the generated plots, it appears as though the average fuel use per vehicle across the United States
                   decreases as a result of a higher gas tax rate/real cost of gas. However, this may simply be a result of the
                   fact that the gas tax rate and real cost of gas have increased over time, and the fuel efficiency of vehicles
                   has also increased over time. This is one possible explanation of the decreased average fuel use per vehicle."),
                 p(strong("Interesting anomalies in the data:")),
                 p("In looking at the gas tax rate vs. fuel use per vehicle plot, one might notice that the average fuel use per
                   vehicle varies greatly when the gas tax is .04 dollars per gallon and .184 dollars per gallon. This is partly
                   due to the fact that these were the gas taxes for the longest period of time. The gas tax was .04 dollars per
                   gallon from 1959 to 1982, and the gas tax was .184 dollars from 1993 to 1995 and 1997 to 2011.")
               ),
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("Table", 
                                      h4("Gasoline Tax Rate/Real Cost of Gasoline vs. Average Fuel Use Per Vehicle"), 
                                      p(strong("gas_tax_rate:"), "Federal and State gasoline tax rates effective end of calendar year. (dollars/gallon)"),
                                      p(strong("real_cost_of_gas:"), "The 'real' cost of a gallon of gasoline adjusted for inflation to 2005 dollars. Regular leaded gasoline (1950 to 1977), all grades of gasoline (1978 to present)."),
                                      p(strong("fuel_use_per_vehicle:"), "Average amount of fuel used per registered motor vehicle. (gallons)"),
                                      p("Use the slider and radio buttons to select a year and type of data to display"),
                                      p(verbatimTextOutput("info")),
                                      tableOutput("table")),
                             tabPanel("Plot Visualization", 
                                      h4("Gasoline Tax Rate/Real Cost of Gasoline vs. Average Fuel Use Per Vehicle"), 
                                      p(strong("gas_tax_rate:"), "Federal and State gasoline tax rates effective end of calendar year (dollars/gallon)"),
                                      p(strong("real_cost_of_gas:"), "The 'real' cost of a gallon of gasoline adjusted for inflation to 2005 dollars. Regular leaded gasoline (1950 to 1977), all grades of gasoline (1978 to present)."),
                                      p(strong("fuel_use_per_vehicle:"), "Average amount of fuel used per registered motor vehicle. (gallons)"),
                                      p("Use the radio buttons to select the value displayed along the x-axis. Click anywhere on the plot to reveal
                                        the gas tax rate/real cost of gas and fuel use per vehicle at that point"),
                                      div(
                                        style = "position:relative",
                                        plotOutput("plot", click = "plot_click"),
                                        verbatimTextOutput("more_info")
                                      )
                             )
                 )
               )
             )),
    tabPanel("Fatalities",
             titlePanel("Fatalities"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("Range", label = "Year",
                             choices = year.choices)
                 
               ),
               mainPanel(
                 tabsetPanel(
                   type = "tabs",
                   tabPanel("Table", textOutput("Explanation"),
                            dataTableOutput("table1")),
                   tabPanel("Map", 
                            textOutput("map.explanation"), 
                            plotOutput("map", click = "plot_click"),
                            textOutput("print"))
                   
                 )
                 
               )
             )
             
             
    ),
    tabPanel("Roadway Spending and Metric Fuel & Miles Per Driver",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("range1", "Range of Years For Coorelation:", 
                             value = c(2005, 2010),
                             min = 2004,
                             max = 2014),
                 sliderInput("yr", "Year of Highway Data:",
                             value = 2008,
                             min = 2004,
                             max = 2014),
                 selectInput("state", "Choose a U.S Region:", state.choice, selected = "All U.S Regions")
               ),
               mainPanel(
                 h1("Map Visualizations:"),
                 p("Being a resident in the state of Washington,
                   I get to experience one of the worst experiences 
                   as a commuter. Commuters in this state spend a lot
                   of time wasted in traffic due to the never-ending
                   construction of the roads. To a commuter here,
                   it feels like there is always construction work
                   on the same street without any progress at all. We  
                   decided to look into data from the Office of Highway
                   Policy Information, to explore if there is a 
                   relationship between the vehicle miles traveled, 
                   the motor fuel use, and the revenue used on highways annually 
                   on a state-level. The following data visualizations intend to 
                   paint a picture between the relationship of these three variables, 
                   in hopes of exploring whether states are spending so much 
                   on building new highways due to the amount of commuting. 
                   (IMPORTANT: Alaska was not included in our data in hopes of
                   not over-complicating the data visualizations."),
                 br(),
                 h4("Vehicle Miles of Travel Per State:"),
                 p("The map below is a chloropleth map showing the vehicle miles traveled for each state 
                   annually in millions of miles. We find this data important since 
                   it gives us an idea about the amount of commuting that is done
                   annually in each state, and how that compares to other states."),
                 plotOutput("Vehicle_Miles", click = "VM_Click"),
                 h4("Motor-Fuel Use Per State"),
                 p("The map below is a chloropleth map showing the motor-fuel use for each state 
                   annually in thousands of gallons. We find this specific data significant
                   since it gives us an idea of how much fuel is being put to use
                   in each state, and how that affects the investment in roads. For example, 
                   one might assume that if a state consumes more fuel than usual, then 
                   the state would invest more on roads in hopes of encouraging 
                   it's drivers to spend more on fuels."),
                 plotOutput("Motor_Fuel", click = "MF_Click"),
                 h4("Revenue Used For Roads Per State:"),
                 p("The map below is a chloropleth map showing the revenue used for roads for each state 
                   annually in thousands of dollars. We find this information important since 
                   it directly gives un an idea of how much each state is invested in it's 
                   roads and how important that is to each state."),
                 plotOutput("revenue", click = "Rev_Click"), 
                 p(textOutput("coorelation"))
                 )
                 )
                 ),
    tabPanel("Changyu",
             titlePanel("How do the percent of maintenance outlays 
                        in overall capital outlays change from 1999 to 2014?"),
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year1",
                             "Which year:",
                             min = 1999, 
                             max = 2014,
                             value = c(1999, 2010)
                 )
               ),
               
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("Outlay",
                                      strong(h2("Map of Percent of Maintenance Outlays 
                                                in Overall Capital Outlays")),
                                      strong(em("The color with higher saturation 
                                                represents larger amount of increase in 
                                                the percentage and the color with lower 
                                                saturation represents larger amount of 
                                                decrease in the percentage.")),
                                      #verbatimTextOutput("tmp"),
                                      plotOutput("plot1", click = "plot_click"),
                                      h3("We may use this map to explore if there is any
                                         relation between percent of maintenance in capital outlays
                                         and fatalities rate.")
                                      #verbatimTextOutput("info")
                                      #tableOutput("table")
                                      )
                                      )
                 
                                      )
               )
             
                 )
))

shinyUI(my.ui)