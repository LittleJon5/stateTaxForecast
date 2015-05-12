library(shiny)
library(forecast)
library(ggplot2)
library(fImport)

shinyUI(fluidPage(
  
  
# ------------------------- This loads the css file for apps style
  includeCSS("styles.css"),
  
  titlePanel(h1("State Government Revenue Forecasts")),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText(h3("Graphs, Models, and Forecasts")),
      
      selectInput("data.type", label = h4("Tax Type"),
                  choices = c(
                              "Total Tax Revenue",
                              "General Sales & Gross Receipts",
                              "Individual Income",
                              "Corporate Income",
                              "Total Income",
                              "Property"
                  ),
                  selected = "Total Tax Revenue"
      ),
      
      selectInput("state", label = h4("State"),
                  choices = states,
                  selected = "Alabama"),
      
      actionButton("getData", "Forecast"),
      
      helpText(h3("Options")),
      
      selectInput("manipulate", label = h4("Transformations"),
                  choices = c("No Transformation",
                              "Change",
                              "Percent Change"),
                  selected = "No Transformation"),
      
      selectInput("scalefactor", 
                  label = h4("Scale Factor"), 
                  c("No Change" = 1,
                    "Thousands" = 1000,
                    "Millions" = 1000000,
                    "Billions" = 1000000000),
                  selected = "Billions"),
      
      sliderInput("smooth",
                  label = h4("Smoothing Parameter"),
                  min=.30,
                  max=.75,
                  value= .51,
                  animate = TRUE),
      
      numericInput("horizon", 
                   label = h4("Forecast Horizon"), 
                   value = 2), 
      
      dateInput("date", 
                label = h4("Initial Date (YYYY-MM-DD)"), 
                value = "1994-01-01")
    ),
    
    
    
    mainPanel(
        tabsetPanel(
          tabPanel("Forecast Graph", h5(plotOutput("plot"))),
          tabPanel("Model",
                   helpText(h5("Method")),
                   h2(verbatimTextOutput("text")),
                   helpText(h5("Smooth Parameters")),
                   h2(verbatimTextOutput("text2")),
                   helpText(h5("Model")),
                   h2(verbatimTextOutput("text3"))),
          tabPanel("Forecasts",
                   helpText(h5("Forecasted Values")),
                   h5(tableOutput("table")))
      
  )))))
