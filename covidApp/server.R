

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
        leaflet(countries,
                options = leafletOptions(worldCopyJump = TRUE,
                                         minZoom = 1.25,
                                         zoomSnap = 0.25,
                                         zoomDelta = 0.25)) %>%
            addTiles() %>% 
            setMaxBounds(lng1 = "Infinity", lat1 = -90, 
                         lng2 = "-Infinity", lat2 = 90) %>% 
            setView(lng = 0, lat = 45, zoom = 1) %>% 
            addPolygons(
                weight = 1,
                dashArray = "",
                color = "#B3B6B7",
                opacity = 1,
                fillColor = "#F8F9F9",
                fillOpacity = 0,
                highlightOptions = highlightOptions(weight = 2,
                                                    color = "#FFFFFF",
                                                    dashArray = "",
                                                    fillColor = "#FF2600",
                                                    fillOpacity = 0.7,
                                                    bringToFront = TRUE,
                                                    sendToBack = TRUE)
            )
        
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
    
    output$recommendation <- renderText({
        "dummy text"
    })
    
    output$dummyPlot <- renderPlot({
        plot(mtcars$wt, mtcars$qsec)
    })

})
