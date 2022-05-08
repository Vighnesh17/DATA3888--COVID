

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
library(DT)

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
                layerId = countries_data$ISO_A3,
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
                        "<strong>",countries_data$ADMIN,"</strong>", "<br>",
                        "Average population: ", format(countries_data$avg.pop,
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
    
    ## the time series plot in Analysis Plots tab
    output$time_plot <- renderDygraph({
        # validate that user input, to avoid error message if nothing is passed on
        validate(
            need(input$plot_countries, "Please select a country.")
        )
        
        varname = input$plot_var
        countries = input$plot_countries
        
        # subset dataset into selected variable and time, by countries (iso_code)
        covid_subset = covid_data %>% 
            select(date, iso_code, all_of(varname)) %>% 
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
    
    output$clickInfo <- renderPrint({
        input$covid_map_shape_click
        # return the country ISO code on click
    })
    
    ## the time series plot under the map, reactive to click on map
    output$timePlot_click <- renderDygraph({
        # validate that user input, to avoid error message if nothing is passed on
        validate(
            need(input$covid_map_shape_click, "Please click on a country.")
        )
        
        varname = input$timePlot_click_var
        countries = input$covid_map_shape_click$id
        
        # subset dataset into selected variable and time, by countries (iso_code)
        covid_subset = covid_data %>% 
            select(date, iso_code, varname) %>% 
            filter(iso_code %in% countries) %>% 
            pivot_wider(names_from = iso_code,
                        values_from = varname)
        
        # create time series
        subset.xts = xts(select(covid_subset, !date), 
                         order.by = covid_subset$date)
        # time series plot of var vs time (time series)
        subset.xts %>% 
            dygraph() %>% 
            dyRangeSelector() %>% 
            dyHighlight()
    })
    
    ## time series plot of people vaccinated & people fully vaccinated
    output$timePlot_vacc <- renderDygraph({
        # validate that user input, to avoid error message if nothing is passed on
        validate(
            need(input$vacc_country, "Please select a country.")
        )
        
        varname = c("people_vaccinated", "people_fully_vaccinated")
        countries = input$vacc_country
        
        # subset dataset into selected variable and time, by countries (location)
        covid_subset = covid_data %>% 
            select(date, location, varname) %>% 
            filter(location %in% countries) %>% 
            pivot_wider(names_from = location,
                        values_from = varname)
        
        # create time series
        subset.xts = xts(select(covid_subset, !date), 
                         order.by = covid_subset$date)
        # time series plot of vaccination vs time (time series)
        subset.xts %>% 
            dygraph(main = "Vaccination Trend") %>% 
            dyRangeSelector() %>% 
            dyHighlight()
    })
    
    output$prediction <- renderPrint({
        print("placeholder for prediction output")
        # user inputs
        new_data = tibble(
            population = input$population,
            gdp_per_capita = input$gdp,
            total_vaccinations = input$vacc_available
        )
        # the predicted value based on user input
        # lmfit from global.R, a simple linear model
        print(predict(lmfit, new_data, interval = "confidence"))
    })

})
