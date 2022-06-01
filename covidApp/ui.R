# Dashboard for COVID data

# Define UI for application
shinyUI(
    
    navbarPage(
        # Application title
        "DATA3888 COVID-19 Data Analysis (Group C7)", id = "navbar",
        theme = bs_theme(version = 4, bootswatch = "minty"),
        
        # Map tab page ------------------------------------------------------------
        
        tabPanel(
            "Interactive World Map",
            
            fluidPage(
                
                ## top panel for map
                fluidRow(
                    style = "height:400px;",
                    
                    column(width = 8,
                           # here map output
                           leafletOutput("covid_map"),
                           # the floating window for user input on top of map
                           absolutePanel(
                               id = "select_country_panel",
                               bottom = 85,
                               left = 20,
                               width = 200,
                               height = 50,
                               draggable = FALSE,
                               wellPanel(style = "opacity: 0.8; background-color: #ffff;",
                                         # let user to select country
                                         selectInput(inputId = "select_country",
                                                     label = "Country for graphs:",
                                                     choices = countries$ADMIN,
                                                     selected = "Australia"),
                                         style = "z-index:1002;")
                               )
                    ),
                    
                    column(width = 4,
                           # here VRI table
                           DTOutput("vri_dtable"),
                           # verbatimTextOutput("inputEvents"),
                           style = "height:400px;" #overflow-y:scroll;",
                    )
                    
                ), ## END fluidRow 1
                
                hr(), ## horizontal line
                
                ## bottom panel
                fluidRow(
                    
                    # LEFT: time lag panel
                    column(width = 6,
                           tabsetPanel(
                               type = "tabs",
                               tabPanel(
                                   "Time Lag Graph (1st vs 2nd Dose)",
                                   column(12,
                                          align = "center",
                                          br(),
                                          tableOutput("timeLag_value"),
                                          dygraphOutput("timeLag_timePlot")
                                          )
                               ),
                               tabPanel(
                                   "Time Lag Data Table",
                                   h4("Time Lag (days) of All Countries"),
                                   br(),
                                   DTOutput("timeLag_dtable")
                               )
                           ),
                           style = 'border-right: 1px solid #DDDDDD'
                    ),
                    
                    # RIGHT: fit vs real people_vaccinated; stages barplot
                    column(width = 6,
                           tabsetPanel(
                               # header = renderText("clickInfo"),
                               type = "tabs",
                               tabPanel(
                                   "Vaccination trend",
                                   column(12,
                                          align = "center",
                                          br(),
                                          textOutput("warning_logistic"),
                                          br(),
                                          dygraphOutput("timePlot_click")
                                          )
                               ),
                               tabPanel(
                                   "Rollout policy stages",
                                   br(),
                                   plotlyOutput("policy_barPlot")
                               )
                           )
                           
                    )
                    
                )## END fluidRow 2
                
            )## END fluidPage
            
        ), ## END tabPanel - world map
        

        # About page ----------------------------------------
        
        tabPanel(
            "About",
            fluidPage(
                h2("Additional information"),
                strong("Disclaimer:"),
                tags$div("All data, including country and region boundaries, are from third parties and does not represent any political affiliation of the members of this group."),
                br(),
                strong("Data Source:"),
                tags$div("Our covid data, vaccination policy data and satisfaction 2021 data are from Our World in Data. Corruption data is from Transparency International 2021 and GHS data is from 2021 Global Health. The national borders data are from Natural Earth and do not represent any political position of the group."),
                br(),
                strong("Vaccination Rollout Policy Stages:"),
                p("The tab in our Shiny App ",em("Rollout Policy Stages"),"shows the coverage for the chosen country. The data for this plot is taken from our additional dataset Vaccine Rollout Stages and this shows the coverage of vaccines for the chosen country. ",

                    "0 - No availability", br(),
                    "1 - Availability for ONE of the following: key workers/ clinically vulnerable groups / elderly groups", br(),
                    "2 - Availability for TWO of the following: key workers/ clinically vulnerable groups / elderly groups", br(),
                    "3 - Availability for ALL the following: key workers/ clinically vulnerable groups / elderly groups", br(),
                    "4 - Availability for all three, plus partial additional availability (select broad groups/ages", br(),
                    "5 - Universal availability"
                ),
                strong("Corruption Perceptions Index: "),
                p("1. 0 Signifies Highest Corruption Perception"),
                p("2. 100 Signifies the Lowest Corruption Perception "),
                
                strong("Distance Metrics Used for Time Lag: "),
                withMathJax(),
                p("In our analysis, we use distance metrics to determine the difference between two time series attributes.The values for the 3 different metrics used can be viewed in the tab", em("Time Lag Table"),"for each country"),
                helpText("1. Euclidean Distance - Used to calculate the distance between two points on a plane (x1,y1) and (x2,y2).)
                  The formula is: \\(\\sqrt{(x1-x2)^2 - (y1-y2)^2}\\) "),
                
                helpText("2. Manhattan Distance-  It is an another distance metric which is generally preferred to find the distance between two points when in a grid like plane.
                         We use the Manhattan distance since studies show that it is the better measure for higher dimension data and helps our analysis see whether we are being affected by the curse of dimensionality
                         The formula is : \\(d =  \\Sigma_{i=1}^{N} \\lvert xi - yi\\rvert\\)"),
                
                strong("Vaccine Rollout Index:  "),
                helpText("The VRI is defined as \\(vri=r\\cdot \\frac{d}{N}\\), whereby, 
                  r is the vaccination uptake rate,
                  d is the number of people vaccinated, 
                  and N is the population[no.]. 
                  The d/N measures the coverage of vaccination while the r measures the speed. 
                  ",br(), "Vaccine Uptake or 'r' in our calculation is defined as the number of people vaccination during a time period as a fraction of the country's population and this rate has been estimated using 
                         a Logistic Growth Model and Asymptotic Regression which can be viewed in the Shiny app under the tab "), em("Vaccination Trend"),
                br(),
                
                
                
                
                
                
                hr(),
                h2("References"),
                tags$ul(
                    tags$li(
                        tags$a(href = "https://ourworldindata.org/", "Our World Data")
                    ),
                    tags$li(
                        tags$a(href="https://www.transparency.org/en/cpi/2021", "Transparency International")
                    ),
                    tags$li(
                        tags$a(href="https://www.ghsindex.org/", "2021 Global Health Security Index")
                    )
                    ,
                    tags$li(
                        tags$a(href="https://medium.com/@kunal_gohrani/different-types-of-distance-metrics-used-in-machine-learning-e9928c5e26c7", "Distance Metrics")
                    )
                )
            )
            
        ) ## END tabPanel - About
         
    )
    
    
)
