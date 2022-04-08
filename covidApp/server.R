

# setup -------------------------------------------------------------------

library(shiny)
library(tidyverse)
library(lubridate)
library(kableExtra)
library(leaflet)
library(plotly)
library(dygraphs)


# data loadin and cleaning ------------------------------------------------

covid_data <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv", stringsAsFactors = FALSE, check.names =  FALSE)

covid_data = covid_data %>% 
    mutate(
        date = ymd(date)
    )


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    newCases_subset = reactive({
        # get country iso code from input$iso from ui.R
        iso = input$iso
        
        # subset of new cases
        covid_data %>% 
            select(date, iso_code, location, new_cases) %>% 
            filter(iso_code == iso)
    })

    output$newCasesLine <- renderDygraph({
        
        # time series plot of new cases vs time (time series)
        newCases_subset() %>% 
            dygraph() %>% 
            dySeries("new_cases") %>% 
            dyRangeSelector()

    })

})
