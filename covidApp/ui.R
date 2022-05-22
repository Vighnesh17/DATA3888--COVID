# Dashboard for COVID data

# Define UI for application
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
                    style = "height:400px;",
                    
                    column(width = 8,
                           # here map output
                           leafletOutput("covid_map")
                    ),
                    
                    column(width = 4,
                           # here VRI table
                           DTOutput("vri_dtable"),
                           style = "height:400px; overflow-y:scroll;",
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
                                   dygraphOutput("timeLag_timePlot"),
                                   br(),
                                   tableOutput("timeLag_value")
                               ),
                               tabPanel(
                                   "Time Lag Data",
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
                h1("Additional information"),
                hr(),
                h1("References"),
                hr(),
            )

        ) ## END tabPanel - About
         
    )
    
    
)
