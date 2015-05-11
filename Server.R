require(shiny)
require(forecast)
require(ggplot2)
require(fImport)
require(magrittr)
require(lubridate)
require(reshape2)
source("helper.R")

shinyServer(function(input, output) {
  
##################
#------------------------------- Indicator category
# This recieves to the Indicator and saves it as category.input
# for later use
                                        
    category.input <- reactive({
      input$class
    })
    
    
####################
    # The next part of this converts the user input for data.type
    # and turns into the associated fred ticker symbol
    ###############
 
    indicator.base <- eventReactive(input$getData, {
      
                        switch(input$data.type,
                               "Total Tax Revenue" = "TOTLTAX",
                               "General Sales & Gross Receipts" = "SALESTAX",
                               "Individual Income" = "INCTAX",
                               "Corporate Income" = "CORPINCTX",
                               "Total Income" = "TLINCTAX",
                               "Property" = "PROPTAX"
                               )
      })
    
    indicator.state <- eventReactive(input$getData, {
      
        postalCode(input$state)
      
    })
    
    indicator.complete <- reactive({
      
      paste(indicator.state(), indicator.base(), sep = "")
      
    })
    
########################################
    # This part of the server gets the data from FRED. It uses the 
    # indicator.type() to indicate which fred series I'll pull
    #######################
    
    fred.data <- reactive({
                            fredSeries(indicator.complete(), from = input$date) %>%
                              applySeries(by = "monthly", FUN = mean)
                          })
    
    
  
##############################
    # This part of the is what we used to dermine the
    # the compound frequncy of the fredSeries
    # this will come into play in the next part of the server
    # we need this information for some of the manipulation functions
    ##########################
    
                  
    fred.compound <- reactive({
                                frequency(fred.data())
                              })
#######################
    # This function calls on functions located in the helper script that
    # change the time series. 
    ###################
    
    fred.final <- reactive({
                            switch(input$manipulate,
                                   "No Transformation" = fred.data(),
                                   "Change" = fred.data() %>% chg,
                                   "Change From a Year Ago" = fred.data() %>% ch1(n_obs_per_year = fred.compound()),
                                   "Percent Change" = fred.data() %>% pch,
                                   "Percent Change from a Year Ago" = fred.data() %>% pc1(n_obs_per_year = fred.compound()),
                                   "Compounded Annual Rate of Change" = fred.data() %>% pca(n_obs_per_year = fred.compound()),
                                   "Continuously Compounded Rate of Change" = fred.data() %>% cch,
                                   "Countinuously Compounded Annual Rate of Change" = fred.data() %>% cca(n_obs_per_year = fred.compound()),
                                   "Natural Log" = fred.data() %>% log
                                   )   
                            })
    
    output$debug <- renderTable({
      
      fred.final() %>% as.data.frame
      
    })
    
######################
    # The next function takes the final time series and scales it
    # next it makes an ets model
    # then if forecast the models out by how ever far the user desires
    ##################
    ets.model <- reactive({
                        (fred.final()/ as.numeric(input$scalefactor)) %>%
                        ets
                        })
    
    ets.forecast <- reactive({
                          ets.model() %>% forecast(h = input$horizon)
                          })
########################
    # this part out puts the model paramerter the model table on the ui
    #####################
   
    
    output$text <- renderPrint({
      
      ets.forecast()$model$method
                                    
      })
    
    
    
    output$text2 <- renderPrint({
      
      ets.forecast()$model$par
      
    })
    
    
    output$text3 <- renderPrint({
      
      nrow.value <- nrow(ets.forecast()$model$states)
      
      ets.forecast()$model$states[nrow.value, ]
      
    })
    
#############################
    # This displays the forecast information
    # it calls the forecast.frame function from the helper script
    #########################
    
    output$table <- renderTable({
      
      validate(
        need(input$getData, "Please Select the data you wish to Forecast and Click the 'Forecast' Button,
             when this message disappears your forecast is on its way.")
      )
      
#       forecast.df <- forecast.frame(ets.forecast())
#       forecast.df
      
      ets.forecast() %>% as.data.frame
      
    })
    
#######################
    # This calls three functions from helper.r to get the two data
    # required to use in the plot function
    # #################
    
   output$plot <- renderPlot({
    
    validate(
      need(input$getData, "Please Select the data you wish to Forecast and Click the 'Forecast' Button.
           When this message disappears your forecast is on its way.")
    )
     
    forecast.df <- forecast.plot.frame(ets.forecast())
 
    plot.data <- past.data(ets.forecast())
    
    ggforecast(plot.data, forecast.df, input$smooth, input$date)
    
 })
   
  
  
  }
)


