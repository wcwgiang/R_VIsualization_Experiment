# Globals
experiment_parameters <- NULL
current_trial <- reactiveValues(i = 0, samples = NULL)
current_trial_parameters <- NULL
current_page <- NULL
current_task <- 1
experiment_data <- data.frame()

SCENARIO_SAMPLES <- read.csv('scenario_samples.csv')
BLOCK1 <- read.csv('graphical_block1.csv')
BLOCK2 <- read.csv('graphical_block2.csv')
SCENARIOS <- rbind(BLOCK1,BLOCK2)

### Function to generate Random Trial List
# Or take in my scenario list and then randomize the presentation... <- Easier 
# use sample(vector) reorders it
#run_order <- sample(rep(1:length(block1[,1])))
#block1[run_order[1],]



page_names <- c('login_page','form','judgement_task','judgement_conf',
                'estimation_task', 'estimation_conf', 'thankyou_msg', 'go_on', 'Final_screen')


# Define the mandatory fiedls here
# which fields get saved 
fieldsAll <- c("user", "guess")

# which fields are mandatory
fieldsMandatory <- c("guess")

responsesDir <- file.path("responses")

# Password to login for this session
session_password <- "password"

### Generate data here
### 
### 
### 
set.seed(1906)
n_rounds <- 2
n_flips <- 7
probs <- c(0.6,0.4)
prize <- 1
true_state <- "Heads"
show_up <- 10

flips <- sapply(1:n_rounds, function(x) sample(c("Heads", "Tails"), n_flips, replace = TRUE, prob = probs))
n.heads <- apply(flips, 2, function(x) length(which(x == "Heads")))
plot_data <- data.frame(n.coin.flips = rep(10, n_rounds), 
                        heads = n.heads, 
                        tails = n_flips - n.heads
                        )

# add an asterisk to an input label
labelMandatory <- function(label) {
    tagList(
        label,
        span("*", class = "mandatory_star")
    )
}

# CSS to use in the app
appCSS <-  ".mandatory_star { color: red; }
.shiny-input-container { margin-top: 25px; }
#submit_msg { margin-left: 15px; }
#error { color: red; }
#adminPanel { border: 4px solid #aaa; padding: 0 20px 20px; }"

# Helper functions
humanTime <- function() format(Sys.time(), "%d-%m-%Y-%H-%M-%S")

saveData <- function(data) {
    fileName <- sprintf("%s_%s.csv",
                        humanTime(),
                        digest::digest(data))
    
    write.csv(x = data, file = file.path(responsesDir, fileName),
              row.names = FALSE, quote = TRUE)
}

payoffRound <- function(user){
    set.seed(user)
    out <- sample(seq(1, n_rounds), 1)
    return(out)
}

epochTime <- function() {
    as.integer(Sys.time())
}


### Experiment Logic
initialize_newexperiment <- function(){
  set.seed(as.integer(Sys.time()))
  # Variable for determining when to exit out of experiment loop 
  experiment_parameters$finished <<- FALSE
  # Run Order (Blocked by feedback, need to run the first 64 before the second 64)
  experiment_parameters$run_order <<- c(sample (rep(1:64)),sample(rep(64+1:64*2)))
  # Participant ID
  experiment_parameters$participant_id <<- 1 #NEED TO FIGURE OUT HOW TO GET THIS (or use participant)
  
  # Which task the participant is currently on
  current_task <<- 1
}

load_next_trial <- function(){
  i_temp <- isolate(current_trial$i)
  # If we aren't at the first trial, save the last trial results to the dataframe
  if (i_temp != 0){
    if (i_temp == 1){
      experiment_data <<- data.frame(current_trial_parameters)
    } else {
      experiment_data <<- rbind(experiment_data, current_trial_parameters)
    }
  } 
  # Check if we're done the experiment and should save the data
  if (i_temp == length(SCENARIO[,1])){
    saveData(experiment_data)
    experiment_parameters$finished <<- TRUE
  } else {
    # Load the information for the next trial
    # Incremement trial counter
    i_temp <- i_temp + 1
    cat("\nNew Trial: ",i_temp,"\n")
    current_trial_parameters <<-  c(SCENARIOS[experiment_parameters$run_order[i_temp],])
    isolate({current_trial$i <<- i_temp})
    current_trial$sample <<- SCENARIO_SAMPLES[,current_trial_parameters$SCENARIO_NUM]
  }
}

advance_page <- function(){ # Should also write some data saving stuff here
  # Check if we need to advance page because we are at a terminal point in the task
  # Otherwise advance to next page
  
  if (is.null(current_page)){
    load_next_trial()
    current_task <<- 1
    if(current_trial_parameters$JUDGEMENT == 'Pre'){
      current_page <<- 'judgement_task'
    } else {
      current_page <<- 'estimation_task'
    }
  } else if (current_page == 'estimation_conf'){
    if (current_task == 2){
      load_next_trial()
      current_task <<- 1
      if(current_trial_parameters$JUDGEMENT == 'Pre'){
        current_page <<- 'judgement_task'
      } else {
        current_page <<- 'estimation_task'
      }
    } else {
      current_page <<- 'judgement_task'
      current_task <<- 2
    }    
  } else if (current_page == 'judgement_conf'){
    if (current_task == 2){
      load_next_trial()
      current_task <<- 1
      if(current_trial_parameters$JUDGEMENT == 'Pre'){
        current_page <<- 'judgement_task'
      } else {
        current_page <<- 'estimation_task'
      }
    } else {
      current_page <<- 'estimation_task'
      current_task <<- 2
    }
  } else if (current_page == 'estimation_task'){
    # Advance to confidence
    current_page <<- 'estimation_conf'
  } else if (current_page == 'judgement_task'){
    current_page <<- 'judgement_conf'
  }
  if (experiment_parameters$finished){
    show_page("Final_screen")
  } else {
    cat("Current Task:",current_task,'\n')
    cat("Current_page:", current_page,'\n')
    show_page(current_page)
  }
}

show_page <- function(page){
  # Hides all other pages, then shows the page we want
  for (p in page_names){
    shinyjs::reset(p)
    shinyjs::hide(p)
  }
  shinyjs::show(page)
  cat('Now showing: ', page, '\n')
}

### http://shiny.rstudio.com/gallery/server-to-client-custom-messages.html <-- for having java script that redirects the page (https://groups.google.com/forum/#!topic/shiny-discuss/QXInjsqejLY)