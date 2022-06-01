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
                p("All data, including country and region boundaries, are from third parties and does not represent any political affiliation of the members of this group."),

                strong("Data Sources:"),
                br(),
                p("Our COVID-19 data is from the", tags$a(href = "https://github.com/owid/covid-19-data/tree/master/public/data", "Our World in Data (OWID) GitHub repository"), "and the policy data is from the", tags$a(href = "https://ourworldindata.org/covid-vaccination-policy", "OWID website."), "The regional boundaries in the interactive world map are from", tags$a(href = "https://www.naturalearthdata.com/downloads/50m-cultural-vectors/50m-admin-0-countries-2/", "Natural Earth"), "and do not represent any political position of the group."),

                strong("Vaccination Rollout Policy Stages:"),
                br(),
                p("The tab in our Shiny App ",em("Rollout Policy Stages"),"shows the vaccnation speed (people per day) for each vaccine rollout policy (availability) stage in the chosen country. The stages are:"),
                  tags$ul(
                      tags$li("0 - No availability (not shown in graph)"),
                      tags$li("1 - Availability for ONE of the following: key workers/ clinically vulnerable groups / elderly groups"),
                      tags$li("2 - Availability for TWO of the following: key workers/ clinically vulnerable groups / elderly groups"),
                      tags$li("3 - Availability for ALL the following: key workers/ clinically vulnerable groups / elderly groups"),
                      tags$li("4 - Availability for all three, plus partial additional availability (select broad groups/ages)"),
                      tags$li("5 - Universal availability")
                  ),
                
                strong("Distance Metrics Used for Time Lag: "),
                withMathJax(),
                p("In our analysis, we use distance metrics to determine the difference between two time series attributes. The values for the 3 different metrics used can be viewed in the tab", em("Time Lag Table"),"for each country."),
                tags$ol(
                    tags$li("Euclidean Distance - Used to calculate the distance between two points on a plane \\((x_1,y_1)\\) and \\((x_2,y_2)\\).", br(),
                            "The formula is: \\(\\sqrt{(x_1-x_2)^2 - (y_1-y_2)^2}\\)"),
                    br(),
                    tags$li("Manhattan Distance - Generally preferred for finding the distance between two points when in a grid like a plane.", br(),
                            "We use the Manhattan distance since studies show that it is the better measure for higher dimension data and helps our analysis see whether we are being affected by the curse of dimensionality.", br(),
                            "The formula is: \\(d =  \\Sigma_{i=1}^{N} \\lvert x_i - y_i\\rvert\\)")
                ),
                
                strong("Vaccine Rollout Index:"),
                p("The equations used to calculate VRI are from", tags$a(href = "https://www.mdpi.com/2076-393X/10/2/194/html", "Kazemi, M., Bragazzi, N. L., & Kong, J. D. (2022) on MDPI"), ", where VRI is defined as
                  $$VRI=r\\cdot \\frac{d}{N} \\quad,$$
                  where",
                  tags$ul(
                      tags$li("\\(r\\) is the vaccination uptake rate, taken from a logsitic regression model as explained below"),
                      tags$li("\\(d\\) is the total number of people vaccinated at the end of the time period"),
                      tags$li("\\(N\\) is the total population")
                      
                  ),
                  "Therefore, the \\(d/N\\) measures the coverage of vaccination while the \\(r\\) measures the speed."),
                
                p("Vaccine Uptake or \\(r\\) in our calculation is estimated from a Logistic Growth Model
                  $$c(t) = \\frac{K}{1 + ae^{-rt}} \\quad,$$
                  where",
                  tags$ul(
                      tags$li("\\( c(t) \\) is the people vaccinated as a function of time \\(t\\) (days from the first date there is data for people vaccinated in a that country)"),
                      tags$li("\\( K \\) is the y-asymptote (max people vaccinated if \\(c(t)\\) were to reach a plateau"),
                      tags$li("\\( a \\) is a constant")
                  ),
                  "The fittness of the model is visualised in the this app under the tab ", em("Vaccination Trend.")),
                
                
                ## References
                hr(),
                h2("References"),
                tags$ul(
                    tags$li("Main COVID-19 data source:",
                            tags$a(href = "https://ourworldindata.org/", "Our World in Data (OWID)")
                    ),
                    tags$li("Distance types description:",
                            tags$a(href="https://medium.com/@kunal_gohrani/different-types-of-distance-metrics-used-in-machine-learning-e9928c5e26c7", "Different Types of Distance Metrics used in Machine Learning")
                    ),
                    tags$li("VRI:",
                            tags$a(href = "https://www.mdpi.com/2076-393X/10/2/194/html", "Assessing Inequities in COVID-19 Vaccine Roll-Out Strategy Programs: A Cross-Country Study Using a Machine Learning Approach")
                    )
                )
            )
            
        ) ## END tabPanel - About
         
    )
    
    
)
