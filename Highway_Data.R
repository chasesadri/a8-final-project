suppressWarnings(library(maps))
suppressWarnings(library(sp))
suppressWarnings(library(maptools))

library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

GetStateFuel <- function(year) {
  file.name <- paste0("Data/Motor-fuel-", year, ".csv")
  fuel.data <- read.csv(file.name, stringsAsFactors = FALSE, strip.white = TRUE)
  cutoff.rows <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  if (year < 2010) {
    cutoff.rows <- c(cutoff.rows, 11, 12, 13)
  }
  fuel.data <- fuel.data[-cutoff.rows, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 18)]
  fuel.data <- top_n(fuel.data, 53, X.17)
  fuel.data[, "X"] <- tolower(fuel.data[, "X"])
  colnames(fuel.data)[1] <- "region"
  colnames(fuel.data)[ncol(fuel.data)] <- "Total"
  fuel.data[, "Total"] <- as.numeric(gsub(",", "", fuel.data[, "Total"]))
  new.data <- fuel.data
  new.data[9, "region"] <- "district of columbia"
  new.data <- select(new.data, region, Total)
}


GetMilesData <- function(year) {
  file.name <- paste0("Data/Vehicle-Miles-", year, ".csv")
  miles.data <- read.csv(file.name, stringsAsFactors = FALSE, strip.white = TRUE)
  cutoff.rows <- c(1, 2, 3, 4, 5, 6, 7)
  cutoff.cols <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
  if (year > 2008) {
    cutoff.cols <- c(cutoff.cols, 17, 18)
  }
  miles.data <- miles.data[-cutoff.rows, cutoff.cols]
  miles.data <- top_n(miles.data, 54, X.14)
  miles.data[, 1] <- tolower(miles.data[, 1])
  colnames(miles.data)[1] <- "region"
  colnames(miles.data)[ncol(miles.data)] <- "Total"
  miles.data[, "Total"] <- as.numeric(gsub(",", "", miles.data[, "Total"]))
  new.data <- miles.data
  new.data <- select(new.data, region, Total)
  new.data[9, "region"] <- "district of columbia"
  return(new.data)
}




GetRevenueData <- function(year) {
  file.name <- paste0("Data/Revenue-", year, ".csv")
  revenue.data <- read.csv(file.name, stringsAsFactors = FALSE, strip.white = TRUE)
  cutoff.rows <- c(1)
  revenue.data <- revenue.data[-cutoff.rows, c(cutoff.rows, 14)]
  revenue.data[, 1] <- tolower(revenue.data[, 1])
  colnames(revenue.data)[1] <- "region"
  colnames(revenue.data)[ncol(revenue.data)] <- "Total"
  revenue.data[, "Total"] <- as.numeric(gsub(",", "", revenue.data[, "Total"]))
  revenue.data <- top_n(revenue.data, 54, Total)
  revenue.data <- select(revenue.data, region, Total)
  revenue.data[9, "region"] <- "district of columbia"
  return(revenue.data)
}


states <- map_data("state")

GetDataCoordinates <- function(new.data, title, name.of.state) {
  new.data <- left_join(new.data, states, by = "region")
  name.of.state <- tolower(name.of.state)
  breaks = c(0, fivenum(new.data[, "Total"]))
  labels <- c(paste0(breaks[1], " <= x <= ", breaks[2]), 
              paste0(breaks[2], " <= x <= ", breaks[3]),
              paste0(breaks[3], " <= x <= ", breaks[4]),
              paste0(breaks[4], " <= x <= ", breaks[5]),
              paste0(breaks[5], " <= x")
              )
  new.data[, "Total"] <- factor(
    cut(new.data[, "Total"], breaks = breaks, labels = labels)
  )
  if (name.of.state != "all u.s regions") {
    new.data <- filter(new.data, region == name.of.state)
  }
  ggplot(data = new.data, aes(x = long, y = lat, group = group, fill = new.data[, "Total"])) +
    geom_polygon(color = "grey10", show.legend = TRUE) +
    coord_fixed(1.3) +
    coord_equal() +
    scale_fill_brewer(palette = "Reds") +
    labs(title = title,
         x = "Longitude",
         y = "Latitude",
         fill = title) 
}


GetCoorelationCoefficent <- function(range, name.of.state) {
  name.of.state <- tolower(name.of.state)
  if (name.of.state != "all u.s regions") {
    min <- range[1]
    max <- range[2]
    miles <- GetMilesData(min)
    fuel <- GetStateFuel(min)
    rev <- GetRevenueData(min)
    if (name.of.state != "all u.s regions") {
      miles <- filter(miles, region == name.of.state)
      fuel <- filter(fuel, region == name.of.state)
      rev <- filter(rev, region == name.of.state)
    }
    min = min + 1
    while(min <= max + 1) {
      m <- GetMilesData(min)
      f <- GetStateFuel(min)
      r <- GetRevenueData(min)
      if (name.of.state != "all u.s regions") {
        m <- filter(m, region == name.of.state)
        f <- filter(f, region == name.of.state)
        r <- filter(r, region == name.of.state)
      }
      miles <- rbind(miles, m)
      fuel <- rbind(fuel, f)
      rev <- rbind(rev, r)
      min = min + 1
    }
  
    miles[, "Total Fuel"] <- fuel[, "Total"] 
    miles[, "Total Rev"] <- rev[, "Total"]
    miles <- na.omit(miles)
    miles <- cor(miles[,unlist(lapply(miles, is.numeric))])
    coeff <- (miles["Total", "Total Fuel"] + miles["Total", "Total Rev"] + miles["Total Rev", "Total Fuel"]) / 3
    return(coeff)
  }
  return(NA)
}

state.choice <- state.name
state.choice <- state.choice[-2]
state.choice <- c("All U.S Regions", state.choice, "District of Columbia")





