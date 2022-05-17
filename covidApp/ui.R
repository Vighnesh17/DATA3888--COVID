# Dashboard for COVID data


# Libraries and setup -----------------------------------------------------

library(shiny)
library(tidyverse)
library(lubridate)
library(shinyWidgets)
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
                    
                    # here map output
                    leafletOutput("covid_map"),
                    
                    # the floating window for user input on top of map
                    # absolutePanel(
                    #     id = "map_userInputs",
                    #     top = 10,
                    #     right = 10,
                    #     width = "auto",
                    #     height = "auto",
                    #     draggable = FALSE,
                    #     wellPanel("Variables to map",
                    #               style = "opacity: 0.8; background-color: #ffff;",
                    #               # eg. display numerical variable on heat map
                    #               selectInput(inputId = "timePlot_click_var",
                    #                           label = "Numerical Variable",
                    #                           choices = num_vars)
                    #     )
                    # )
                ), ## END fluidRow 1
                
                br(),
                
                ## time slider
                fluidRow(
                    div(style = "margin:auto; width:80%;",
                        sliderTextInput("vriDate",
                                        label = NULL,
                                        choices = vriDate_choices_char,
                                        selected = vriDate_choices_char[9],
                                        grid = TRUE,
                                        width = "100%")
                    )
                ),
                
                hr(), ## horizontal line
                
                ## bottom panel
                fluidRow(
                    # input panel on the left
                    column(width = 3,
                           wellPanel(
                               p("dummy text")
                           )
                    ),
                    
                    # output panel in the middle
                    column(width = 4,
                           p("user input table"),
                           p("calculated output table/value + short explanation"),
                           verbatimTextOutput("clickInfo"),
                           verbatimTextOutput("vriOutput"),
                           style = 'border-right: 1px solid #DDDDDD'
                    ),
                    
                    # plot on the right
                    column(width = 5,
                           dygraphOutput("timePlot_click")
                    )
                    
                )## END fluidRow 2
                
            )## END fluidPage
            
        ), ## END tabPanel - world map
        

        # Vaccine Time Lag tab page -----------------------------------------------
        
        tabPanel(
            "Vaccination Time Lag", 
            # here analysis plots + variable selections, value inputs, etc
            sidebarLayout(
                # user inputs/selections
                sidebarPanel(
                    strong("Select a Location"),
                    hr(),
                    selectizeInput(inputId = "countries_lag",
                                   label = "Location(s): max 3",
                                   choices = loc_all,
                                   multiple = TRUE,
                                   options = list(maxItems = 3))
                ),
                
                # plot area
                mainPanel(
                    # here time series plot
                    dygraphOutput("timeLag_timePlot"),
                    br(),
                    # time lag datatable
                    DTOutput("timeLag_dtable")
                    
                )
            )
        ), ## END tabPanel - time lag
        

        # Vaccine Visualisation and prediction ------------------------------------

        tabPanel(
            "Vaccine Visualisation & Prediction",
            sidebarLayout(
                # here controls/user inputs for vaccine rollout
                sidebarPanel(
                    strong("Predictors:"),
                    hr(),
                    # allow multiple country selection? for comparison?
                    selectizeInput(inputId = "plot_countries",
                                   label = "Location(s)",
                                   choices = loc_all,
                                   multiple = TRUE,
                                   options = list(maxItems = 4)),
                    # initial values made up, need fixing
                    numericInput(inputId = "population",
                                 label = "Country population",
                                 value = 1e6),
                    numericInput(inputId = "gdp",
                                 label = "GDP per capita",
                                 value = 2000),
                    numericInput(inputId = "vacc_available",
                                 label = "Vaccines available",
                                 value = 5000),
                ),
                
                # Predictions (bottom) and time series plots of vaccination (up)
                mainPanel(
                    # people vaccinated trend, plot on top
                    dygraphOutput("time_plot"),
                    hr(),
                    # prediction of vaccination? under time series plot
                    verbatimTextOutput("prediction")
                )
            )
        ), ## END tabPanel
        

        # Vaccine rollout and Availability ----------------------------------------

        tabPanel(
            "Vaccine Rollout Stages",
            fluidPage(
                h1("Heading"),
                fluidRow(
                    verbatimTextOutput("rollout_barPlot")
                )
            )
        ) ## END tabPanel - rollout
         
    )
    
    
)
