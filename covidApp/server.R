

# setup -------------------------------------------------------------------

library(shiny)
library(tidyverse)
library(lubridate)
library(kableExtra)
library(leaflet)
library(plotly)
library(dygraphs)
library(xts)
library(sp)

# data loadin and cleaning ------------------------------------------------

## load data from global.R


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # world map of covid data
    output$covid_map <- renderLeaflet({
        ## For Choropleths
        # selectedVar = input$num_var_map
        # pal = colorNumeric(palette = "OrRd",
        #                    domain = covid_data$selectedVar)
        
        ## The data you want to display in labels, per ISO code
        # average population
        pop_summary = covid_data %>% 
            group_by(iso_code) %>% 
            summarise(avg.pop = mean(population, na.rm = TRUE))
        
        ## Combine with geo data
        countries_data = merge(countries, pop_summary,
                               by.x = "ISO_A3", by.y = "iso_code",
                               all.x = TRUE) # reserve all geo data
        
        
        ## Fashion into leaflet map, with data labels on hover
        leaflet(countries_data,
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
                                                    sendToBack = TRUE),
                ## popup labels on click alternative
                # popup = paste0(
                #     "<strong>",countries_data@data$ADMIN,"</strong>", "<br>",
                #     "Average population: ", format(countries_data@data$avg.pop,
                #                                    big.mark = ",")
                # )
                ## hover over labels alternative
                label = lapply(
                    paste0(
                        "<strong>",countries_data@data$ADMIN,"</strong>", "<br>",
                        "Average population: ", format(countries_data@data$avg.pop,
                                                       big.mark = ",")
                    ), HTML
                ),
                labelOptions = labelOptions(direction = "auto")
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
