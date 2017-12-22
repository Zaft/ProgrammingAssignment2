
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
  
  ## list of all hospitals in date
  hospitals <- unique(data$Hospital.Name)
  
  ## validate state parameter
  validState <- is.element(state, states)
  if(!validState) {
    stop("invalid state")
  }
  
  ## trim data to only columns we care about
  data <- data[,c(2,outcomeCol)]
  
  ## Make sure column in numeric
  data[,2] <- suppressWarnings(as.numeric(data[,2]))
  
  ## Sort result mortality rate then, alphabetically by hospital name
  data <- data[order(data[,2], data[,1]),]
  
  ## filter to only complete cases
  data <- data[complete.cases(data),]
  
  if(typeof(num) == "character") {
    
    if(num == "best") {
      ## result <- head[data[,2], 1]
      ## result <- suppressWarnings(data[which.max(data[,2]),])
      result <- head(data[,1], n=1)
    }
    
    if(num == "worst") {
      ##result <- suppressWarnings(data[which.min(data[,2]),])
      result <- tail(data[,1], n=1)
    }
    
  }
  
  if(typeof(num) == "integer" || typeof(num) == "double") {
    ## result <- head[data[,2], n=num]
    ##data <- suppressWarnings(data[which.min(data[,outcomeCol]),])
    ##result <- data[num,]
    result <- data[num,][1,1]
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  ## In the case of a tie only return the first hospital 
  result
  
}