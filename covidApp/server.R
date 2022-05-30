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
            addPolygons(
                layerId = ~ADM0_A3,
                group = "VRI Overlay",
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
                        "<strong>",countries$ADMIN,"</strong>","<br>",
                        "VRI (0~1): ",format(countries$vri_scaled, digits = 2),"<br>",
                        "Time Lag (1st dose to 2nd dose): ",countries$`Lag: Euclidean distance`
                    ), HTML
                ),
                labelOptions = labelOptions(direction = "auto")
            ) %>% 
            addPolygons(
                layerId = ~ADMIN,
                group = "Time Lag Overlay",
                weight = 0.5,
                smoothFactor = 0.2,
                dashArray = "",
                color = "#B3B6B7",
                opacity = 1,
                fillColor = ~pal2(`Lag: Euclidean distance`),
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
                        "<strong>",countries$ADMIN,"</strong>","<br>",
                        "VRI (0~1): ",format(countries$vri_scaled, digits = 2),"<br>",
                        "Time Lag (1st dose to 2nd dose): ",countries$`Lag: Euclidean distance`
                    ), HTML
                ),
                labelOptions = labelOptions(direction = "auto")
            ) %>% 
            addLayersControl(
                baseGroups = c("VRI Overlay", "Time Lag Overlay"),
                options = layersControlOptions(collapsed = FALSE),
                position = "bottomright"
            ) %>% 
            addEasyButton(easyButton(
                icon="fa-globe", title="Zoom to Level 1",
                onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>% 
            # add vri legend (default)
            leaflet::addLegend(
                group = "VRI Overlay",
                pal = pal,
                values = ~vri_scaled,
                title = "VRI (0~1)",
                position = "topright",
                opacity = 0.7
            ) %>%
            # leaflet::addLegend(
            #     group = "Time Lag Overlay",
            #     pal = pal2,
            #     values = ~`Lag: Euclidean distance`,
            #     title = "Time Lag<br>(1st dose to 2nd dose)",
            #     position = "topright",
            #     opacity = 0.7
            # ) %>% 
            hideGroup(c("VRI Overlay", "Time Lag Overlay")) %>% 
            showGroup("VRI Overlay")
    })
    
    # update legend according to layer group chosen
    observeEvent(input$covid_map_groups, {
        proxy <- leafletProxy("covid_map", data = countries) %>% clearControls()
        req(input$covid_map_groups)
        
        if (input$covid_map_groups[2] == "VRI Overlay") {
            proxy %>% 
                # add vri legend (default)
                leaflet::addLegend(
                    group = "VRI Overlay",
                    pal = pal,
                    values = ~vri_scaled,
                    title = "VRI (0~1)",
                    position = "topright",
                    opacity = 0.7
                )
        } else if (input$covid_map_groups[2] == "Time Lag Overlay") {
            proxy %>%
                leaflet::addLegend(
                    group = "Time Lag Overlay",
                    pal = pal2,
                    values = ~`Lag: Euclidean distance`,
                    title = "Time Lag<br>(1st dose to 2nd dose)",
                    position = "topright",
                    opacity = 0.7
                )
        }
    })
    
    ## temporary
    output$inputEvents <- renderPrint({reactiveValuesToList(input)})
    
    ## relate country selection (drop-down) to map click
    observeEvent(input$covid_map_shape_click, {
        if (input$covid_map_shape_click$group == "VRI Overlay") {
            selected_country = countries$ADMIN[countries$ADM0_A3 == input$covid_map_shape_click$id]
        } else if (input$covid_map_shape_click$group == "Time Lag Overlay") {
            selected_country = input$covid_map_shape_click$id
        }
        updateSelectizeInput(inputId = "select_country",
                             label = "Country for graphs:",
                             choices = countries$ADMIN,
                             selected = selected_country)
    })
    
    ## VRI table
    output$vri_dtable <- renderDT({
        countries@data %>% select(ADMIN, vri_scaled) %>% 
            filter(!is.na(vri_scaled)) %>% 
            arrange(-vri_scaled) %>% 
            rename("Location" = ADMIN, "VRI (0-1)" = vri_scaled) %>% 
            datatable(options = list(scrollY = 200)) %>% 
            formatRound("VRI (0-1)", digits = 4)
    })
    
    click_iso = reactive({
        countries$ADM0_A3[countries$ADMIN == input$select_country]
    })
    
    # country name as in countries geo dataset
    click_countryname = reactive({
        input$select_country
    })
    
    # country name as in covid dataset
    click_countryname_location = reactive({
        countries$location[countries$ADMIN == input$select_country]
    })
    
    ## the time series plot to show r fit
    logistic_timePlot <- reactive({
        # validate that user input, to avoid error message if nothing is passed on
        validate(
            need(input$select_country, "Please select/click on a country.")
        )
        
        iso = click_iso()
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
    
    # render time series plot under map, right panel
    output$timePlot_click <- renderDygraph({
        logistic_timePlot()
    })
    
    ## barplot? for vaccine rollout speed, each policy stage
    output$policy_barPlot <- renderPlotly({
        # validate that user input, to avoid error message if nothing is passed on
        validate(
            need(input$select_country, "Please select/click on a country.")
        )
        
        iso = click_iso()
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
            need(input$select_country, "Please select/click on a country.")
        )
        
        iso = click_iso()
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
    
    ## time lag value (in table) for selected country, above time plot
    output$timeLag_value <- renderTable({
        # validate user input, to avoid error message if nothing is passed on
        validate(
            need(input$select_country, "")
        )
        click_countryname_location = click_countryname_location()
        # validate if the clicked country is in the list of filtered countries
        validate(
            need(click_countryname_location %in% countries_lag, "Not enough data to calculate time lag between 1st and 2nd dose.")
        )
        total_lag %>% 
            filter(location %in% click_countryname_location) %>% 
            select("location", "LagType", "Lag: Euclidean distance") %>% 
            rename("Lag (days): Euclidean distance" = "Lag: Euclidean distance")
    }, digits = 0)
    
    ## time lag dtable for all countries
    output$timeLag_dtable <- renderDT({
        total_lag %>% 
            select(-V3, -V5, -LagType) %>% 
            datatable(options = list(autoWidth = TRUE, scrollY = 250))
    })

})
