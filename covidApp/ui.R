# Dashboard for COVID data


# Libraries and setup -----------------------------------------------------

library(shiny)
library(tidyverse)
library(lubridate)
library(kableExtra)
library(leaflet)
library(bslib)
library(plotly)
library(dygraphs)
library(xts)
library(DT)

# Define UI for application that draws a histogram
shinyUI(
    
    navbarPage(
        # Application title
        "DATA3888 COVID-19 Data Analysis", id = "navbar",
        theme = bs_theme(version = 4, bootswatch = "minty"),
        
        # Map tab page ------------------------------------------------------------
        
        tabPanel(
            "Interactive World Map",
            
            fluidPage(
                
                ## top panel for map
                fluidRow(
                    style = "position: relative;",
                    
                    # the underlying map, takes up the whole page
                    # here map output
                    leafletOutput("covid_map"),
                    
                    # the floating window for user input on top of map
                    absolutePanel(
                        id = "map_userInputs",
                        top = 10,
                        right = 10,
                        width = "auto",
                        height = "auto",
                        draggable = FALSE,
                        wellPanel("Variables to map",
                                  style = "opacity: 0.8; background-color: #ffff;",
                                  # eg. display numerical variable on heat map
                                  selectInput(inputId = "timePlot_click_var",
                                              label = "Numerical Variable",
                                              choices = num_vars)
                        )
                    )
                ), ## END fluidRow 1
                
                hr(), ## horizontal line
                
                ## bottom panel
                fluidRow(
                    # recommendation text on the left
                    column(width = 3,
                           strong("Recommendations"),
                           textOutput("recommendation"),
                           verbatimTextOutput("clickInfo"),
                           style = 'border-right: 1px solid #DDDDDD'
                    ),
                    
                    # plot on the right
                    column(width = 9,
                           dygraphOutput("timePlot_click")
                    )
                    
                )## END fluidRow 2
                
            )## END fluidPage
            
        ), ## END tabPanel
        

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
