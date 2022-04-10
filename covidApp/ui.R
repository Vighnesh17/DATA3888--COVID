# Dashboard for COVID data


# Libraries and setup -----------------------------------------------------

library(shiny)
library(tidyverse)
library(lubridate)
library(kableExtra)
library(leaflet)
library(bslib)

# Define UI for application that draws a histogram
shinyUI(
    
    navbarPage(
        # Application title
        "DATA3888 COVID-19 Data Analysis", id = "navbar",
        theme = bs_theme(version = 4, bootswatch = "minty"),
        
        
        # Map tab page ------------------------------------------------------------
        
        tabPanel(
            "Interactive World Map",
            class = "outer",
            
            # fillPage(
                # the underlying map, takes up the whole page
                # here map output
                leafletOutput("covid_map"),
                
                # the floating window for user input on top of map
                absolutePanel(
                    id = "map_userInputs",
                    class = "panel panel-default",
                    top = 0,
                    right = 0,
                    width = "auto",
                    height = "auto",
                    style = 'opacity: 0.9',
                    position = "relative",
                    wellPanel("Variables to map",
                              # eg. display numerical variable on heat map
                              selectInput(inputId = "num_var_map",
                                          label = "Numerical Variable",
                                          choices = num_vars)
                    )
                )
            # )
            
        ),
        

        # Analysis plots tab page -------------------------------------------------
        
        tabPanel(
            "Analysis Plots", 
            # here analysis plots + variable selections, value inputs, etc
            sidebarLayout(
                # user inputs/selections
                sidebarPanel(
                    "User Inputs",
                    selectInput(inputId = "plot_var",
                                label = "Variable to plot",
                                choices = num_vars),
                    selectizeInput(inputId = "plot_countries",
                                   label = "Countries",
                                   choices = iso_all,
                                   multiple = TRUE,
                                   options = list(maxItems = 4))
                ),
                
                # plot area
                mainPanel(
                    # here time series plot
                    dygraphOutput("time_plot")
                )
            )
        )
        
    )
    
    
)
