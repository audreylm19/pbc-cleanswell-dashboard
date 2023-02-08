library(shiny)
library(tidyverse)
library(shinythemes)

#setwd("C:/Users/audre/Documents/Data Science Portfolio/PBC-CleanSwell/clean csv")

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("PBC CleanSwell Data Prep"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"
                ),
      ),
      
      #checkboxInput("header", "Header", TRUE),
      #checkboxInput("lower", "use lowercase column names", TRUE),
      downloadButton('downloadData', 'Download Cleaned CSV')
    ),
    
    mainPanel(
      tableOutput("cleanedData")
    )
  )
)

columns = c("Cleanup ID","Zone","State","Country","GPS","Cleanup Type","Cleanup Date","Group Name","Adults","Children","People","Pounds","Miles","Cigarette butts","Food wrappers (candy, chips, etc.)","Food containers (plastic)","Food containers (foam)","Bottle caps (plastic)","Lids (plastic)","Straws/stirrers (plastic)","Utensils (plastic)","Beverage bottles (plastic)","Beverage bottles (glass)","Beverage cans","Grocery bags (plastic)","Cups, Plates (plastic)","Cups, Plates (foam)","Lines, nets, traps, ropes, etc.","Other plastic/foam packaging","Foam dock pieces","Strapping bands","Balloons","Cigar tips","Cigarette lighters","Tobacco packaging/wrap","Tobacco products (lighters, cigar tips, wrap)","Foam packaging","Clothing","Electronic waste (phones, batteries)","Footwear (shoes/slippers)","Personal hygiene (Clean Swell)","Gloves & masks (PPE)","Plastic/foam pieces","Other plastic waste","Cotton bud sticks (swabs)","Total Items Collected")

good_cols = gsub("[^[:alnum:]]", ".", columns)

server <- function(input, output) {
  
  columns <- reactive({
    #req(input$lower)
    good_cols <- tolower(good_cols)
    
    good_cols
  })
  
  dataInput <- reactive({
    req(input$file)
    
    data <- read.csv(input$file$datapath)
    
    #cleanswell adds "totals" row which is useless
    data <- subset(data, GPS!="")
    
    #making latitude and longitude columns
    gps_list <- strsplit(data$GPS, split = ",")
    gps_df <- as.data.frame(do.call(rbind, gps_list))
    names(gps_df) <- c('latitude', 'longitude')
    
    data <- cbind(data, gps_df)
    
    #making huge list of all possible group names
    pbc_groups <- c()
    
    #have to make lowercase so that none are missed due to capitalization
    data$Group.Name <- tolower(data$Group.Name)
    
    for (name in data$Group.Name)
    {
      if (grepl('pbc', name, fixed=TRUE)){
        pbc_groups <- c(pbc_groups, name)
        
      } else if (grepl('pcb', name, fixed=TRUE)){
        pbc_groups <- c(pbc_groups, name)
        
      } else if (grepl('pacif', name, fixed=TRUE)){
        pbc_groups <- c(pbc_groups, name)
        
      } else if (grepl('coalition', name, fixed=TRUE)){
        pbc_groups <- c(pbc_groups, name)
        
      } else if (grepl('ncl', name, fixed=TRUE)){
        pbc_groups <- c(pbc_groups, name)
        
      } else if (grepl('ymsl', name, fixed=TRUE)){
        pbc_groups <- c(pbc_groups, name)
      }
    }
    
    pbc_groups <- unique(pbc_groups)
    
    groups_df <- data[data$Group.Name %in% pbc_groups,]
    
    #GPS filtering
    sf_pacifica_latitude <- c(37.489488, 37.7911798)
    sf_pacifica_longitude <- c(-122.533339, -122.436522)
    
    sf_pacifica_df <- data[between(data$latitude, sf_pacifica_latitude[1], sf_pacifica_latitude[2]),]
    sf_pacifica_df <- sf_pacifica_df[between(sf_pacifica_df$longitude, sf_pacifica_longitude[1], sf_pacifica_longitude[2]),]
    
    hmb_latitude <- c(37.36298, 37.518358)
    hmb_longitude <- c(-122.477034, -122.389143)
    
    hmb_df <- data[between(data$latitude, hmb_latitude[1], hmb_latitude[2]),]
    hmb_df <- hmb_df[between(hmb_df$longitude, hmb_longitude[1], hmb_longitude[2]),]
    
    polygon_df <- rbind(sf_pacifica_df, hmb_df)
    polygon_df <- polygon_df[polygon_df$Group.Name %in% append(pbc_groups,""),]
    
    #combining both filtered data frames
    
    df <- rbind(groups_df, polygon_df)
    
    df <- distinct(df)
    
    names(df) <- tolower(names(df))
    
    df[is.na(df)] <- as.integer(0)
    df
  })
  
  cleanedData <- reactive({
    req(dataInput())
    
    dataInput()[columns()]
  })
  
  output$cleanedData <- renderTable({
    cleanedData()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      name = input$file
      name = gsub(".csv", "", name)
      paste0(name,"_cleaned.csv")
    },
    content = function(file) {
      write.csv(cleanedData(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
