
rankall <- function(outcome, num="best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
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
  
  ## trim data to only columns we care about
  data <- data[,c(2, 7, outcomeCol)]
  
  result <- setNames(data.frame(character(), character()), c("hospital", "state"))
  
  ## list of all states
  states <- unique(data$State)
  
  datalist = list()
  
  for(i in states) {
    stateData <- data[which(data$State == i),]
    
    ## Make sure column is numeric
    stateData[,3] <- suppressWarnings(as.numeric(stateData[,3]))
    
    ## Sort result mortality rate then, alphabetically by hospital name
    stateData <- stateData[order(stateData[,3], stateData[,1]),]
    
    if(typeof(num) == "character") {
      
      if(num == "best") {
        name <- head(stateData[,1], n=1)
      }
      
      if(num == "worst") {
        name <- tail(stateData[,1], n=1)
      }
      
    }
    
    if(typeof(num) == "integer" || typeof(num) == "double") {
      name <- stateData[num,][1,1]
    }
    
    ### TODO: Figure out how to combine data!!! 
    dat <- data.frame(hospital = name, state = i)
    ##row <- c(state=i, hospital=name)
    
    result <- rbind(result, dat)
  }
  
  result
}

