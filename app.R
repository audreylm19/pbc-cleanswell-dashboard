library(shiny)
library(tidyverse)
library(shinythemes)

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
      checkboxInput("lower", "use lowercase column names", TRUE),
      downloadButton('downloadData', 'Download Cleaned CSV')
    ),
    
    mainPanel(
      tableOutput("cleanedData")
    )
  )
)

relevant1 = c("Cleanup ID","Zone","State","Country","GPS","Cleanup Type","Cleanup Date","Group Name","Adults","Children","People","Pounds","Miles","Cigarette butts","Food wrappers (candy, chips, etc.)","Food containers (plastic)","Food containers (foam)","Bottle caps (plastic)","Lids (plastic)","Straws/stirrers (plastic)","Utensils (plastic)","Beverage bottles (plastic)","Beverage bottles (glass)","Beverage cans","Grocery bags (plastic)","Cups, Plates (plastic)","Cups, Plates (foam)","Lines, nets, traps, ropes, etc.","Other plastic/foam packaging","Foam dock pieces","Strapping bands","Balloons","Cigar tips","Cigarette lighters","Tobacco packaging/wrap","Tobacco products (lighters, cigar tips, wrap)","Foam packaging","Clothing","Electronic waste (phones, batteries)","Footwear (shoes/slippers)","Personal hygiene (Clean Swell)","Gloves & masks (PPE)","Plastic/foam pieces","Other plastic waste","Cotton bud sticks (swabs)","Total Items Collected")
#relevant2 = c("Cleanup ID","Zone","State","Country","GPS","Cleanup Type","Cleanup Date","Group Name","Adults","Children","People","Pounds","Miles","Cigarette butts","Food wrappers (candy, chips, etc.)","Food containers (plastic)","Food containers (foam)","Bottle caps (plastic)","Lids (plastic)","Straws/stirrers (plastic)","Utensils (plastic)","Beverage bottles (plastic)","Beverage bottles (glass)","Beverage cans","Grocery bags (plastic)","Cups, Plates (plastic)","Cups, Plates (foam)","Lines, nets, traps, ropes, etc.","Other plastic/foam packaging","Foam dock pieces","Strapping bands","Balloons","Cigar tips","Cigarette lighters","Tobacco packaging/wrap","Tobacco products (lighters, cigar tips, wrap)","Foam packaging","Clothing","Electronic waste (phones, batteries)","Footwear (shoes/slippers)","Personal hygiene (Clean Swell)","Gloves & masks (PPE)","Plastic/foam pieces","Other plastic waste","Cotton bud sticks (swabs)","Total Items Collected")

good_cols = gsub("[^[:alnum:]]", ".", relevant1)

server <- function(input, output) {
  
  columns <- reactive({
    #req(input$lower)

    if(input$lower){
    good_cols <- tolower(good_cols)
    }
    good_cols
  })
  
  dataInput <- reactive({
    req(input$file)
    
    data <- read.csv(input$file$datapath)
    
    if (input$lower){
      names(data) <- tolower(names(data))
    }
    data[is.na(data)] <- as.integer(0)
    data
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
