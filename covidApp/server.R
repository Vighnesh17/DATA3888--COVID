# data loadin and cleaning ------------------------------------------------

## load data from global.R

shinyServer(function(input, output) {
    
    # world map of covid data
    output$covid_map <- renderLeaflet({
        ## For Choropleths
        # selectedVar = input$num_var_map
        # pal = colorNumeric(palette = "OrRd",
        #                    domain = covid_data$selectedVar)
        
        ## Fashion into leaflet map
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
                        "VRI (0~1): ", format(countries$vri_scaled, digits = 2)
                    ), HTML
                ),
                labelOptions = labelOptions(direction = "auto")
            ) %>%
            addEasyButton(easyButton(
                icon="fa-globe", title="Zoom to Level 1",
                onClick=JS("function(btn, map){ map.setZoom(1); }")))
    })
    
    output$vri_dtable <- renderDT({
        countries@data %>% select(ADMIN, vri_scaled) %>% 
            filter(!is.na(vri_scaled)) %>% 
            arrange(-vri_scaled) %>% 
            rename("Location" = ADMIN, "VRI (0-1)" = vri_scaled) %>% 
            datatable() %>% formatRound("VRI (0-1)", digits = 4)
    })
    
    click_countryname = reactive({
        iso = input$covid_map_shape_click$id
        countries$ADMIN[countries$ADM0_A3 == iso]
    })
    
    click_countryname_location = reactive({
        iso = input$covid_map_shape_click$id
        countries$location[countries$ADM0_A3 == iso]
    })
    
    output$clickInfo <- renderPrint({
        # return the country name on click, for heading of plots
        click_countryname()
    })
    
    ## the time series plot under the map, reactive to click on map
    logistic_timePlot <- reactive({
        # validate that user input, to avoid error message if nothing is passed on
        validate(
            need(input$covid_map_shape_click, "Please click on a country.")
        )
        
        iso = input$covid_map_shape_click$id
        country_name = click_countryname()
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
    
    asymptotic_timePlot <- reactive({
        # validate that user input, to avoid error message if nothing is passed on
        validate(
            need(input$covid_map_shape_click, "Please click on a country.")
        )
        
        iso = input$covid_map_shape_click$id
        country_name = click_countryname()
        validate(
            need( (iso %in% r_list2$iso_code), paste0("There was no vaccination data for ",country_name,".") )
        )
        
        idx = which(r_list2$iso_code == iso)
        
        # create time series
        subset.xts = xts(cbind( log(r_list2$y.real[[idx]]), fitted(r_list2$fit[[idx]]) ), 
                         order.by = r_list2$date[[idx]])
        names(subset.xts) = c("Real", "Fitted")
        
        # time series plot of var vs time (time series)
        subset.xts %>% 
            dygraph(main = paste0("People Vaccinated Log-scaled Trend (",country_name,")")) %>% 
            dySeries("Fitted", color = "red", strokePattern = "dashed") %>% 
            dyRangeSelector() %>% 
            dyHighlight()
    })
    
    output$timePlot_click <- renderDygraph({
        if (input$model_dropdown == "Logistic Regression") {
            logistic_timePlot()
        } else {
            asymptotic_timePlot()
        }
    })
    
    ## barplot? for vaccine rollout speed, each policy stage
    output$policy_barPlot <- renderPlotly({
        # validate that user input, to avoid error message if nothing is passed on
        validate(
            need(input$covid_map_shape_click, "Please click on a country.")
        )
        
        iso = input$covid_map_shape_click$id
        country_name = click_countryname()
        
        # validate the iso code of country clicked is have data on people_vaccinated and policy
        validate(
            need( (iso %in% r_list$iso_code), paste0("There was no vaccination data for ",country_name,".") ),
            need( (iso %in% policy$Code), paste0("There was no policy data for ",country_name,".") )
        )
        
        policy_subset = covid_policy %>% 
            filter(iso_code == iso)
        
        # initiate people_vaccinated per stage column
        policy_dates = policy_subset %>% 
            group_by(vaccination_policy) %>% 
            summarise(
                start_date = min(date),
                end_date = max(date)
            ) %>% 
            filter( (vaccination_policy != 0) & !is.na(vaccination_policy) )
        
        stage = speed = c()
        for (i in policy_dates$vaccination_policy) {
            idx = which(policy_dates$vaccination_policy == i)
            lm_list = policy_subset %>% 
                filter(date > policy_dates$start_date[idx] & date <=  policy_dates$end_date[idx]) %>% 
                ct_model(model = "linear")
            stage = c(stage, i)
            speed = c(speed, ifelse(is.null(lm_list$r), NA, lm_list$r))
        }
        policy_speed = tibble(stage = as.factor(stage), speed = speed)
        
        gg = policy_speed %>% 
            remove_missing() %>% 
            ggplot(aes(x = stage, y = speed)) +
            geom_col(aes(fill = stage), position = "dodge") +
            labs(x = "Vaccine Rollout Policy stage",
                 y = "Rate of people vaccinated (per day)",
                 title = paste0("Vaccination Uptake Rate (",country_name,")")) +
            scale_fill_discrete(name = "Stage")
        ggplotly(gg, tooltip = c("x", "y")) %>% 
            layout(title = list(xanchor = "center", x = 0.5),
                   hoverlabel = list(align = "left"))
    })
    
    ## time series plot for vaccination time lag
    output$timeLag_timePlot <- renderDygraph({
        # validate that user input, to avoid error message if nothing is passed on
        validate(
            need(input$covid_map_click, "Please select a country.")
        )
        
        iso = input$covid_map_shape_click$id
        country_name = click_countryname()
        # filter based on country selected
        covid_subset = covid_data %>% 
            select(iso_code, location, date, people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred) %>% 
            filter(iso_code %in% iso)
        
        validate(
            need( ( nrow(covid_subset) != 0 ) & ( !all(is.na(covid_subset$people_vaccinated_per_hundred), is.na(covid_subset$people_fully_vaccinated_per_hundred)) ) , 
                  paste0("There was no vaccination data for ",country_name,".") )
        )
        
        # pivot data into wider form and apply smoother
        covid_subset = covid_subset %>% 
            pivot_wider(names_from = location,
                        values_from = c(people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred)) %>% 
            # smoothing
            mutate(
                across(c(-date, -iso_code),
                       ~lowess(x = date, y = ., f = 0.2)$y)
            )
        
        # create time series
        subset.xts = xts(select(covid_subset, -date, -iso_code), 
                         order.by = covid_subset$date)
        # time series plot of vaccination vs time (time series)
        first_dose1 = names(subset.xts)[1]
        second_dose1 = names(subset.xts)[2]
        subset.xts %>% 
            dygraph(main = paste0("Vaccination Uptake: First Dose vs Second Dose (", country_name,")")) %>% 
            dyAxis("y", label = "People vaccinated per hundred") %>%
            dySeries(first_dose1, label = paste("1 Dose at least"), strokePattern = "dashed") %>% 
            dySeries(second_dose1, label = paste("2 Doses")) %>% 
            dyRangeSelector() %>% 
            dyHighlight()
    })
    
    ## time lag table value for selected country, below time plot
    output$timeLag_value <- renderTable({
        # validate user input, to avoid error message if nothing is passed on
        validate(
            need(input$covid_map_shape_click, "")
        )
        click_countryname_location = click_countryname_location()
        # validate if the clicked country is in the list of filtered countries
        validate(
            need(click_countryname_location %in% countries_lag, "Not enough data to calculate time lag between 1st and 2nd dose.")
        )
        total_lag %>% 
            as_tibble(rownames = "location") %>% 
            filter(location %in% click_countryname_location) %>% 
            select("location", "LagType", "Lag: Euclidean distance") %>% 
            rename("Lag (days): Euclidean distance" = "Lag: Euclidean distance")
    })
    
    ## time lag dtable for all countries
    output$timeLag_dtable <- renderDT({
        total_lag %>% 
            as_tibble(rownames = "location") %>% 
            select(-V3, -V5, -LagType)
    }, options = list(autoWidth = TRUE)
    )
    
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

})
