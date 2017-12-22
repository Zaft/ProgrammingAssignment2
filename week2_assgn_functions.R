
## directory: a character vector of length 1 indicating the location of the csv files
## pollutant: a character vector of length 1 indicating the name of the pollutant to 
## calculate the mean for (either sulfate or nitrate)
## id: a vector of monitor id numbers 
pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  ## list of all files in passed in directory
  files_full <- list.files(directory, full.names = TRUE)
  
  ## create a new empty data frame to concatenate with additional data frames
  data <- data.frame()
  
  ## concatenate all of the files into a single data frame
  for(i in id){
    data <- rbind(data, read.csv(files_full[i]))
  }
  
  mean(data[,pollutant], na.rm = TRUE)
}

complete <- function(directory, id = 1:332) {
  
  ## list of all files in passed in directory
  files_full <- list.files(directory, full.names = TRUE)
  
  ## create a new empty data frame to concatenate with additional data frames
  result <- data.frame("id" = integer(0), "nobs" = integer(0))
  
  ## concatenate all of the files into a single data frame
  for(i in id){
    
    data <- read.csv(files_full[i])
    
    #get count of complete cases
    count <- nrow(data[complete.cases(data),])
    result[nrow(result) + 1,] <- c(i, count)
  }
  
  result
}

corr <- function(directory, threshold = 0) {
  
  ## list of all files in passed in directory
  files_full <- list.files(directory, full.names = TRUE)
  
  ## create a new empty data frame to concatenate with additional data frames
  ## result <- data.frame("id" = integer(0), "nobs" = integer(0))
  result <- vector("numeric")
  
  file_count <- length(files_full)
  
  ## concatenate all of the files into a single data frame
  for(i in 1:file_count){
    
    data <- read.csv(files_full[i])
    
    #get count of complete cases
    count <- nrow(data[complete.cases(data),])
    
    if(count > threshold) {
      
      ## filter data to complete cases
      ##data <- data[complete.cases(data),]
      
      cor <- cor(data$nitrate, data$sulfate, use="complete.obs")
      
      result <- c(result, cor)
    } 
    
  }
  
  result
}

