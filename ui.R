library(shinyjs)

source('helpers.R')


shinyUI(fluidPage(
    useShinyjs(),
    inlineCSS(appCSS),
    
    div( 
        id = "login_page",
        titlePanel("Welcome to the experiment!"),
        br(),
        sidebarLayout(
            
            sidebarPanel(
                h2("Login"),
                p("Welcome to today's experiment. Please use the user name provided on the instructions to login into the experiment."),
                hidden(
                    div(
                        id = "login_error",
                        span("Your user name is invalid. Please check for typos and try again.", style = "color:red")
                    )
                )
            ),
            
            mainPanel(
                textInput("user", "User", ""),
                textInput("password", "Password", ""),
                actionButton("login", "Login", class = "btn-primary")
            )
            
        )
    ),
    
     hidden(
      div( 
        id = "judgement_task",
        titlePanel("Judgement Task"),
        
        sidebarLayout(
          sidebarPanel(
            helpText("Please estimate the time required for the rover to complete its current task.")
            
          ),
          
          mainPanel(
            plotOutput("plot_judgement", width = "600px", height = "250px"),
            # Simple integer interval
            actionButton("Judgement_Yes", "Yes", class = "btn-success" ),
            actionButton("Judgement_No", "No", class = "btn-danger" )
            #actionButton("estimate_confirm", label = "Estimate")
          )
        )
      )
    ),
    hidden(
      div( 
        id = "judgement_conf",
        titlePanel("Judgement Task"),
        
        sidebarLayout(
          sidebarPanel(
            helpText("Please rate how confident you were in your judgement.")
            
          ),
          
          mainPanel(
            # Simple integer interval - Confidence
            sliderInput("judge_conf", label = NULL, min=0, max=100, width = "600px",  value=0, round = FALSE),
            actionButton("judge_conf_submit", label = "Submit")
          )
        )
      )
    ),
    hidden(
      div( 
        id = "estimation_task",
        titlePanel("Estimation Task"),
        
        sidebarLayout(
          sidebarPanel(
            helpText("Please estimate the time required for the rover to complete its current task.")
            
            ),
          
          mainPanel(
            plotOutput("plot_estimate", width = "600px", height = "250px"),
            # Simple integer interval
            sliderInput("estimate", label = NULL, min=0, max=150, width = "600px",  value=0, round = FALSE),
            uiOutput('EstimateButton')
            #actionButton("estimate_confirm", label = "Estimate")
          )
        )
      )
    ),
    hidden(
      div( 
        id = "estimation_conf",
        titlePanel("Estimation Task"),
        
        sidebarLayout(
          sidebarPanel(
            helpText("Please rate how confident you were in your estimate.")
            
          ),
          
          mainPanel(
            # Simple integer interval - Confidence
            sliderInput("estimate_conf", label = NULL, min=0, max=100, width = "600px",  value=0, round = FALSE),
            actionButton("estimate_conf_submit", label = "Submit")
          )
        )
      )
    ),

    hidden(
        div(
            id = "Final_screen",
            titlePanel("End of experiment"),
            sidebarLayout(
                sidebarPanel(
                    h4("Thank you for your participation. You have reached the end of the experiment."),
                    br(),
                    p("You can review your answers in the table on the right."),
                    uiOutput("round"),
                    width = 6
                ),
                
                mainPanel(
                    
                    width = 6
                )
            )
        )
    )
)
)