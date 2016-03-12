library(shiny)
require(digest)
require(dplyr)
require(vioplot)


setwd("d:/Dropbox/R_Visualization_Experiment")

source('helpers.R')



# Temp
#current_trial$sample <- scenario_samples[,2]
#current_trial_parameters$VIS_TYPE <- 'Violin'
#current_trial$JUDGEMENT <- -0.3


shinyServer(
    function(input, output, session) {
        
        # When the Login button is clicked, check whether user name is in list
        observeEvent(input$login, {
            
            # User-experience stuff
            shinyjs::disable("login")
            
            # Check whether user name is correct
            # Fix me: test against a session-specific password here, not username
            user_ok <- input$password==session_password
            
            # If credentials are valid push user into experiment
            if(user_ok){
                initialize_newexperiment()
                advance_page()
                #shinyjs::hide("login_page")
                #shinyjs::show("estimation_task")
                #shinyjs::show('judgement_task')
                #shinyjs::show('estimation_conf')  
              # Save username to write into data file
                output$username <- renderText({input$user})
            } else {
            # If credentials are invalid throw error and prompt user to try again
                shinyjs::reset("login_page")
                shinyjs::show("login_error")
                shinyjs::enable("login")
            }

        })
        

        ### Functions for Estimate Task
        estimateValue <- reactive({
          input$estimate
        }) 
        
        observeEvent(input$Estimate_Confirm, {
          current_trial_parameters$Estimate_Response <<- isolate(input$estimate)
          advance_page()
        })
        

        output$plot_estimate <- renderPlot({
          par(mai = c(1,0.1,0.0,0.1))
          current_trial$sample
          with (current_trial_parameters,{
            if (VIS_TYPE == 'Violin'){
              plot(0,type='n', xlim = c(0,150), ylim = c(0,2),yaxt = 'n', yaxs = "i", yaxs = "i", ylab = "", xlab = "")
              vioplot(current_trial$sample, col = "transparent", ylim = c(0,150), horizontal = TRUE, add = TRUE)
              
            } else if (VIS_TYPE == 'Boxplot'){
              boxplot(current_trial$sample, ylim = c(0,150), range = 0, horizontal=TRUE, yaxs = "i", xaxs = "i")  
            } else if (VIS_TYPE == 'Mean_SD'){
              plot(0,type='n', xlim = c(0,150), ylim = c(0,2),yaxt = 'n', yaxs = "i", yaxs = "i", ylab = "", xlab = "")
              segments(mean(current_trial$sample)-sd(current_trial$sample), 1, mean(current_trial$sample)+sd(current_trial$sample), 1)
              points(mean(current_trial$sample), 1, pch = 16 , cex = 1.5)
            } else {
              boxplot(median(current_trial$sample), ylim = c(0,150), range = 0, horizontal=TRUE, yaxs = "i", xaxs = "i") 
            }
            segments(estimateValue(),-1,estimateValue(),3,lwd = 4, col = "blue")  
          })
        })
        output$EstimateButton <- renderUI({
          actionButton("Estimate_Confirm", label = paste("Estimate: ",estimateValue()), width = "150px")
        })
        
        ### Functions for Judgement Task
        
        output$plot_judgement <- renderPlot({
          par(mai = c(1,0.1,0.0,0.1))
          current_trial$sample
          with (current_trial_parameters,{
            if (VIS_TYPE == 'Violin'){
              plot(0,type='n', xlim = c(0,150), ylim = c(0,2),yaxt = 'n', yaxs = "i", yaxs = "i", ylab = "", xlab = "")
              vioplot(current_trial$sample, col = "transparent", ylim = c(0,150), horizontal = TRUE, add = TRUE)
              
            } else if (VIS_TYPE == 'Boxplot'){
              boxplot(current_trial$sample, ylim = c(0,150), range = 0, horizontal=TRUE, yaxs = "i", xaxs = "i")  
            } else if (VIS_TYPE == 'Mean_SD'){
              plot(0,type='n', xlim = c(0,150), ylim = c(0,2),yaxt = 'n', yaxs = "i", yaxs = "i", ylab = "", xlab = "")
              segments(mean(current_trial$sample)-sd(current_trial$sample), 1, mean(current_trial$sample)+sd(current_trial$sample), 1)
              points(mean(current_trial$sample), 1, pch = 16 , cex = 1.5)
            } else {
              boxplot(median(current_trial$sample), ylim = c(0,150), range = 0, horizontal=TRUE, yaxs = "i", xaxs = "i") 
            }
          
            # Draw judgement cut-off point
            segments(round(quantile(current_trial$sample,c(.5+JUDGEMENT_DIRECTION),names=FALSE)),0,round(quantile(current_trial$sample,c(.5+JUDGEMENT_DIRECTION),names=FALSE)),2,lwd = 4, col = "red")
          })
        })
        observeEvent(input$Judgement_Yes, {
          current_trial_parameters$Judgement_Response <<- 1
          advance_page()
        })
        observeEvent(input$Judgement_No, {
          current_trial_parameters$Judgement_Response <<- 0
          advance_page()
        })
        
        ### Estimate and Judgement Confidence buttons
        observeEvent(input$estimate_conf_submit, {
          current_trial_parameters$Estimate_Conf_Response <<- isolate(input$estimate_conf)
          advance_page()
        })
        observeEvent(input$judge_conf_submit, {
          current_trial_parameters$Judgement_Conf_Response <<- isolate(input$judge_conf)
          advance_page()
        })
    }
)