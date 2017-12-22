
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  ## list of all states
  states <- unique(data$State)
  
  ## validate state parameter
  validState <- is.element(state, states)
  if(!validState) {
    stop("invalid state")
  }
  
  ## validate outcome parameter
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
  
  data <- data[which(data$State == state),]
  
  ## suppress the NAs being introduced warning.
  data <- suppressWarnings(data[which.min(data[,outcomeCol]),])
  
  ## Sort result alphabetically by hospital name
  ## sort(data[,2])
  
  if(typeof(num) == "character") {
    
    if(num == "best") {
      ## result <- head[data[,2], 1]
      data <- suppressWarnings(data[which.max(data[,outcomeCol]),])
      
    }
    
    if(num == "worst") {
      data <- suppressWarnings(data[which.min(data[,outcomeCol]),])
    }
  }
  
  if(typeof(num) == "integer" || typeof(num) == "double") {
    ## result <- head[data[,2], n=num]
    data <- suppressWarnings(data[which.min(data[,outcomeCol]),])
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  ## In the case of a tie only return the first hospital 
  head(data[,2], 1)
  
}