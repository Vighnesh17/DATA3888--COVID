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
                                   selectInput("model_dropdown",
                                               label = "Regression model to fit",
                                               choices = c("Logistic Regression",
                                                           "Asymptotic Regression"),
                                               selected = "Logistic Regression"),
                                   dygraphOutput("timePlot_click")
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
                p(em("Vaccination Rollout Policy Stages:")),
                p(
                    "0 - No availability", br(),
                    "1 - Availability for ONE of the following: key workers/ clinically vulnerable groups / elderly groups", br(),
                    "2 - Availability for TWO of the following: key workers/ clinically vulnerable groups / elderly groups", br(),
                    "3 - Availability for ALL the following: key workers/ clinically vulnerable groups / elderly groups", br(),
                    "4 - Availability for all three, plus partial additional availability (select broad groups/ages", br(),
                    "5 - Universal availability"
                ),
                
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
                )
            )
            
        ) ## END tabPanel - About
         
    )
    
    
)
