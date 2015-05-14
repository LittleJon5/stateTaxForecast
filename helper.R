# FRED transformation functions
# October 11, 2012 Revised March 19, 2015
# Author: Ray Nelson
###############################################################################
require(magrittr)

# ---------------------------------- Change
chg <- function(tsData) {
  tsData %>% 
    diff %>% 
    na.omit
}

# --------------------------------- Year over year change
ch1 <- function(tsData, n_obs_per_year) {
  tsData %>% 
    diff(lag = n_obs_per_year) %>% 
    na.omit
}

# --------------------------------- Percentage change
pch <- function(tsData) {
  (tsData/lag(tsData, 1)-1) * 100 %>% 
    na.omit
}


# ---------------------------- New Plot method

#################################
# Function for creating a forecast plot
# in ggplot2
##########################

forecastPlotData <- function(etsForecast, fredData){
  
  forecasts <- etsForecast %$%
    cbind(.$mean, .$lower, .$upper)
  
  plotData <- cbind(as.ts(fredData), forecasts)
  
  plotData <- data.frame(as.Date(time(plotData)), plotData)
  
  colnames(plotData) <- c("Date", "Indicator", "Forecast", "Lower80", "Lower95",
                          "Upper80", "Upper95")
  
  return(plotData)
  
}

#################################
# Function creates a ggplot of forecast data
##########################

forecastPlot <- function(plotData, span.val, input.date) {
  
  load(url("http://marriottschool.net/teacher/govfinance/recessions.RData"))
  recessions <- subset(recessions, Start >= plotData$Date[1])
  
  ggplot(data = plotData) +
    geom_ribbon(aes(x = Date, ymin = Lower95, ymax = Upper95), fill = "lightblue") +
    geom_ribbon(aes(x = Date, ymin = Lower80, ymax = Upper80), fill = "yellow") +
    geom_line(aes(x = Date, y = Indicator), color = "blue") +
    geom_line(aes(x = Date, y = Forecast), color = "red") +
    geom_rect ( data = recessions , aes ( xmin = Start , xmax = End , 
                                          ymin = -Inf , ymax = +Inf ) , fill = 'grey65', alpha = 0.4 ) +
    geom_smooth ( aes ( x = Date, y = Indicator ) , method = "loess" , span = span.val,
                  size = .65 , color = "black" , fill = "springgreen4" ) +
    labs(y = "")
  
}

# ----------------------------- end New Plot method

##################################
       # This is just a vector of the state postal abreviations
    #################################


postalCode <- function(state) {
  switch(state,
         'Alabama' = 'AL',
         'Alaska' ='AK',
         'Arizona' = 'AZ',
         'Arkansas' = 'AR',
         'California' = 'CA',
         'Colorado' = 'CO',
         'Connecticut' = 'CT',
         'Delaware' = 'DE',
         'Florida' = 'FL',
         'Georgia' = 'GA',
         'Hawaii' = "HI",
         'Idaho' = 'ID',
         'Illinois' = 'IL',
         'Indiana' ='IN',
         'Iowa' = 'IA',
         'Kansas' = 'KS',
         'Kentucky' ='KY',
         'Louisiana' = 'LA',
         'Maine' = 'ME',
         'Maryland' = 'MD',
         'Massachusetts' = 'MA',
         'Michigan' = 'MI',
         'Minnesota' = 'MN',
         'Mississippi' = 'MS',
         'Missouri' ='MO',
         'Montana' = 'MT',
         'Nebraska' = 'NE',
         'Nevada' = 'NV',
         'New Hampshire' = 'NH',
         'New Jersey' = 'NJ',
         'New Mexico' = 'NM',
         'New York' = 'NY',
         'North Carolina' = 'NC',
         'North Dakota' = 'ND',
         'Ohio' = 'OH',
         'Oklahoma' = 'OK',
         'Oregon' = 'OR',
         'Pennsylvania' = 'PA',
         'Rhode Island' = 'RI',
         'South Carolina' = 'SC',
         'South Dakota' = 'SD',
         'Tennessee' = 'TN',
         'Texas' = 'TX',
         'Utah' = 'UT',
         'Vermont' = 'VT',
         'Virginia' = 'VA',
         'Washington' = 'WA',
         'West Virginia' = 'WV',
         'Wisconsin' = 'WI',
         'Wyoming' = 'WY')
}

 stateList <-  c('Alabama', 'Alaska', 'Arizona', 'Arkansas',
                  'California', 'Colorado', 'Connecticut',
                  'Delaware', 'Florida', 'Georgia', 'Hawaii',
                  'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas',
                  'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts',
                  'Michigan', 'Minnesota', 'Mississippi', 'Missouri', 'Montana',
                  'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico',
                  'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon',
                  'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee',
                  'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming')

                 


