

# setup -------------------------------------------------------------------

library(shiny)
library(tidyverse)
library(lubridate)
library(kableExtra)
library(leaflet)
library(plotly)
library(dygraphs)
library(xts)


# data loadin and cleaning ------------------------------------------------

## load data from global.R


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    
    # world map of covid data
    output$covid_map <- renderLeaflet({
        # selectedVar = input$num_var_map
        # pal = colorNumeric(palette = "OrRd",
        #                    domain = covid_data$selectedVar)
        leaflet() %>%
            addTiles()
        # leaflet() %>% 
        #     addTiles() %>%
        #     addPolygons(stroke = FALSE,
        #                 smoothFactor = 0.2,
        #                 fillOpacity = 0.7,
        #                 color = ~pal(selectedVar))
    })
    
    output$time_plot <- renderDygraph({
        varname = input$plot_var
        countries = input$plot_countries
        
        # subset dataset into selected variable and time, by countries (iso_code)
        covid_subset = covid_data %>% 
            select(date, iso_code, varname) %>% 
            filter(iso_code %in% countries) %>% 
            pivot_wider(names_from = iso_code,
                        values_from = varname)
        
        # create time series
        subset.xts = xts(select(covid_subset, !date), 
                         order.by = covid_subset$date)
        # time series plot of new cases vs time (time series)
        subset.xts %>% 
            dygraph() %>% 
            dyRangeSelector() %>% 
            dyHighlight()
    })

})
