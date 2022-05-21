# data loadin and cleaning ------------------------------------------------

## load data from global.R

shinyServer(function(input, output) {
    
    # world map of covid data
    output$covid_map <- renderLeaflet({
        ## For Choropleths
        # selectedVar = input$num_var_map
        # pal = colorNumeric(palette = "OrRd",
        #                    domain = covid_data$selectedVar)
        
        # vri_date = my(input$vriDate)
        # # filter based on date selected
        # countries$data = rep(NA, length(countries$names))
        # df = covid_data %>% 
        #     filter(date >= vri_date & date <= rollforward(vri_date)) %>% 
        #     filter(str_count(iso_code) < 4) %>% 
        #     group_by(iso_code) %>% 
        #     summarise(
        #         value = sum(new_cases, na.rm = TRUE)
        #     )
        
        # for (iso in vri_data$iso_code) {
        #     countries$data[countries$names == iso.expand(iso, regex = FALSE)[1]] = vri_data$vri_scaled[which(vri_data$iso_code == iso)]
        # }
        
        ## Fashion into leaflet map, static
        leaflet(countries,
                options = leafletOptions(worldCopyJump = TRUE,
                                         minZoom = 1.25,
                                         zoomSnap = 0.25,
                                         zoomDelta = 0.25)) %>% 
            addTiles(group = "tiles") %>% 
            setMaxBounds(lng1 = "Infinity", lat1 = -90,
                         lng2 = "-Infinity", lat2 = 90) %>%
            setView(lng = 10, lat = 45, zoom = 1) %>% 
            leaflet::addLegend(
                group = "legend",
                pal = pal,
                values = vri_data$vri_scaled,
                title = "VRI (0~1)",
                position = "topright",
                opacity = 0.7
            ) %>%
            addPolygons(
                layerId = ~ADM0_A3,
                group = "polygons",
                weight = 0.5,
                smoothFactor = 0.2,
                dashArray = "",
                color = "#B3B6B7",
                opacity = 1,
                fillColor = ~pal(vri_scaled),
                fillOpacity = 0.7,
                highlightOptions = highlightOptions(weight = 2,
                                                    color = "#FFFFFF",
                                                    dashArray = "",
                                                    # fillColor = "#FF2600",
                                                    # fillOpacity = 0.7,
                                                    bringToFront = TRUE,
                                                    sendToBack = TRUE),
                label = lapply(
                    paste0(
                        "<strong>",countries$ADMIN,"</strong>", "<br>",
                        "VRI (0~1): ", countries$vri_scaled
                    ), HTML
                ),
                labelOptions = labelOptions(direction = "auto")
            )
    })
    
    ## filtered vri data reactive to vriDate
    # here testing
    # countries_filtered = reactive({
    #     vri_date = my(input$vriDate)
    #     # filter based on date selected
    #     countries$data = rep(NA, length(countries$names))
    #     df = covid_data %>% 
    #         filter(date >= vri_date & date <= rollforward(vri_date)) %>%
    #         group_by(iso_code) %>% 
    #         summarise(
    #             value = sum(new_cases, na.rm = TRUE)
    #         )
    #     for (iso in df$iso_code) {
    #         countries$data[countries$iso_code == iso] = df$value[df$iso_code == iso]
    #     }
    # })
    
    # ## add polygons leaflet proxy, changes with filtered data
    # observe({
    #     mapdata = countries_filtered()
    #     ## Fashion into polygons
    #     leafletProxy("covid_map", data = mapdata) %>% 
    #         clearShapes() %>% 
    #         addPolygons(
    #             layerId = ~iso_code,
    #             weight = 0.5,
    #             smoothFactor = 0.2,
    #             dashArray = "",
    #             color = "#B3B6B7",
    #             opacity = 0.8,
    #             # fillColor = "#F8F9F9",
    #             fillColor = ~pal(data),
    #             fillOpacity = 0.7,
    #             highlightOptions = highlightOptions(weight = 2,
    #                                                 color = "#FFFFFF",
    #                                                 dashArray = "",
    #                                                 # fillColor = "#FF2600",
    #                                                 # fillOpacity = 0.7,
    #                                                 bringToFront = TRUE,
    #                                                 sendToBack = TRUE),
    #             ## popup labels on click alternative
    #             # popup = paste0(
    #             #     "<strong>",countries_data@data$ADMIN,"</strong>", "<br>",
    #             #     "Average population: ", format(countries_data@data$avg.pop,
    #             #                                    big.mark = ",")
    #             # )
    #             ## hover over labels alternative
    #             label = lapply(
    #                 paste0(
    #                     "<strong>", mapdata$names,"</strong>", "<br>",
    #                     "Data: ", format(mapdata$data,
    #                                      big.mark = ",")
    #                 ), HTML
    #             ),
    #             labelOptions = labelOptions(direction = "auto")
    #         )
    # })
    
    ## user input values for VRI prediction, in a reactive dataframe
    vriInput_df = reactive({
        tibble(
            population = input$vriInput_pop,
            gdp_per_capita = input$vriInput_gdp
        )
    })
    
    ## a table of user input values, for user to double check their inputs
    output$vriInput_table <- renderTable(striped = TRUE, {
        input_table = rename(
            vriInput_df(),
            "Population" = "population",
            "GDP per capita" = "gdp_per_capita"
        )
    })
    
    output$vriOutput <- renderPrint({
        my(input$vriDate)
    })
    
    output$vri_dtable <- renderDT({
        countries@data %>% select(ADMIN, vri_scaled) %>% 
            filter(!is.na(vri_scaled)) %>% 
            arrange(-vri_scaled) %>% 
            rename("Location" = ADMIN, "VRI (0-1)" = vri_scaled) %>% 
            datatable() %>% formatRound("VRI (0-1)", digits = 4)
    })
    
    ## plot for vaccination prediction (time series?)
    output$vri_timeplot <- renderDygraph({
        # validate that user input, to avoid error message if nothing is passed on
        validate(
            need(input$plot_countries, "Please select a country.")
        )
        
        varname = c("people_vaccinated")
        countries = input$plot_countries
        
        # subset dataset into selected variable and time, by countries (iso_code)
        covid_subset = covid_data %>% 
            select(date, location, varname) %>% 
            filter(location %in% countries) %>% 
            pivot_wider(names_from = location,
                        values_from = all_of(varname))
        
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
        
        iso = input$covid_map_shape_click$id
        country_name = countries$ADMIN[countries$ADM0_A3 == iso]
        validate(
            need( (iso %in% r_list$iso_code), paste0("There was no vaccination data for ",country_name,".") )
        )
        
        idx = which(r_list$iso_code == iso)
        
        # create time series
        subset.xts = xts(cbind( r_list$y.real[[idx]], fitted(r_list$fit[[idx]]) ), 
                         order.by = r_list$date[[idx]])
        names(subset.xts) = c("Real", "Fitted")
        
        # time series plot of var vs time (time series)
        subset.xts %>% 
            dygraph(main = paste0("People Vaccinated Trend (",country_name,")")) %>% 
            dySeries("Fitted", color = "red", strokePattern = "dashed") %>% 
            dyRangeSelector() %>% 
            dyHighlight()
    })
    
    ## vaccination time lag dataframe subset on selected countries
    covid_subset_lag = reactive({
        
        countries = input$countries_lag
        
        covid_data %>% 
            # filter(iso_code %in% countries) %>% 
            select(date, location, varname_lag) %>% 
            filter(location %in% countries)
    })
    
    ## time series plot for vaccination time lag
    output$timeLag_timePlot <- renderDygraph({
        # validate that user input, to avoid error message if nothing is passed on
        validate(
            need(input$countries_lag, "Please select a country.")
        )
        
        # subset dataset into selected variable and time, by countries (location)
        covid_subset = covid_subset_lag() %>% 
            pivot_wider(names_from = location,
                        values_from = all_of(varname_lag)) %>% 
            # smoothing
            mutate(
                across(-date,
                       ~lowess(x = date, y = ., f = 0.2)$y)
            )
        
        # create time series
        subset.xts = xts(select(covid_subset, !date), 
                         order.by = covid_subset$date)
        # time series plot of vaccination vs time (time series)
        subset.xts %>% 
            dygraph(main = "Vaccination Trend") %>% 
            dyRangeSelector() %>% 
            dyHighlight()
    })
    
    ## vaccination time lag vector reactive
    timeLag_vec = reactive({
        # validate that user input, to avoid error message if nothing is passed on
        validate(
            need(input$countries_lag, "")
        )
        countries_lag = input$countries_lag
        start_date = "2021-02-01"
        end_date = "2021-08-01"
        
        # subset dataset into selected variable and time, by countries (location)
        lag_covid = covid_subset_lag() %>% 
            filter(date >= start_date & date < end_date) %>% 
            # turn all NAs into 0
            mutate(
                across(varname_lag,
                       ~replace(., is.na(.), 0))
            )
        
        # calculate time lag value
        lag_vector <- c()
        z = 1
        # loop through each country
        while (z <= length(countries_lag)){
            # only select records for certain country and only select 1st and 2nd vaccine columns
            lagCovid_filtered = filter(lag_covid, location == countries_lag[z])
            combined_matrix <- cbind(lagCovid_filtered[,3], lagCovid_filtered[,4])
            
            # In the dataset, there are missing values. Will replace these missing values (0) with the value from the date before. Do it for both 1st and 2nd vaccine columns
            
            for (i in 1:nrow(combined_matrix)){
                if (i == 1){
                } else{
                    if (combined_matrix[i,1] == 0){
                        combined_matrix[i,1] = combined_matrix[i-1, 1]
                    }
                }
            }
            
            for (j in 1:nrow(combined_matrix)){
                if (j == 1){
                } else{
                    if (combined_matrix[j,2] == 0){
                        combined_matrix[j,2] = combined_matrix[j-1, 2]
                    }
                }
            }
            
            # Apply smoothing function to 1st and 2nd vaccine columns. f = 0.15 is an arbitrary value
            
            combined_matrix_smooth<- as.matrix(apply(combined_matrix, 2, Lowess, f = 0.15))
            
            # Store each column separately as individual matrices
            FirstDose_matrix = as.matrix(combined_matrix_smooth[,1])
            SecondDose_matrix = as.matrix(combined_matrix_smooth[,2])
            
            # Graph the 1st and 2nd vaccine percentages as a figure of interest. Need to convert back to dataframe.
            # X axis is in days
            combined_matrix_smooth_df = as.data.frame(combined_matrix_smooth)
            matplot(cbind(combined_matrix_smooth_df[,1], combined_matrix_smooth_df[,2]), type ="l", lty = 1, ylab = "Percentage of Population", xlab = countries_lag[z])
            legend("topleft", c("At Least 1 Vaccine", "2 Vaccines"), lty = 1, col=1:2)
            
            # Input the individual matrices into the lagValue function to find the lag between the 1st and 2nd dose for a particular country
            lag <- lagValue(FirstDose_matrix, SecondDose_matrix, windowsize=100)
            #store value of lag
            lag_vector <- c(lag_vector, lag)
            z = z + 1
        }
        # label the lag values with the corresponding country
        names(lag_vector) <- countries_lag
        lag_vector
        
    })
    
    ## convert time lag vector, output time lag table for selected countries
    output$timeLag_dtable <- renderDT({
        # Apply function to each countries Time lag value 
        lag_df = mapply(lagType, lag = timeLag_vec(), windowsize = 100)
        # Visualise Time lags
        t(lag_df)
    })
    
    ## text for vaccine prediction
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
    
    ## barplot? for vaccine rollout, grouped by stages
    output$rollout_barPlot <- renderPrint({
        "dummy text"
    })
    
    output$rollout_scatterPlot <- renderPlotly({
        varname_rollout = "new_vaccinations_smoothed"
        varname2_rollout = "new_deaths"
        covid_data %>% 
            filter(location == input$rollout_country) %>% 
            ggplot() +
            aes(x = date) +
            geom_point(aes(y = varname_rollout)) +
            geom_line(aes(y = varname2_rollout / population * varname_rollout, colour = "death_rate"))
            labs(x = "Date", y = "New vaccination",
                 title = input$rollout_country) +
            scale_y_continuous("value of max and SD", 
                               sec.axis = sec_axis(~./population, name = "number of zero-crossings"))
    })

})
