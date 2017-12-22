## Programming Assignment 3

best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  ##heartAttackData <- data
  
  ## list of all states
  states <- unique(data$State)
  
  ## validate state parameter
  validState <- is.element(state, states)
  if(!validState) {
    stop("invalid state")
  }
  
  # validate outcome parameter
  validOutcome <- is.element(outcome, c("heart attack", "heart failure", "pneumonia"))
  if(!validOutcome) {
    stop("invalid outcome")
  }
  
  outcomeCol <- if(outcome == "heart attack") {
    outcomeCol = 11
  } else if(outcome == "heart failure") {
    outcomeCol = 17
  } else {
    outcomeCol = 23
  }
  
  best <- data[,]
  
  data <- data[which(data$State == state),]
  ## suppress the NAs being introduced warning.
  data <- suppressWarnings(data[which.min(data[,outcomeCol]),])
  
  ## Sort result alphabetically by hospital name
  sort(data[,2])
  
  ## Return hospital name in that state with lowest 30-day death rate
  ## In the case of a tie only return the first hospital 
  head(data[,2], 1)
  
}