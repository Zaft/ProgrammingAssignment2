## Week 2 Looping

## Looping Functions Covered
## lapply
## apply
## mapply
## tapply
## split

## lapply always returns a list
## Create a matrix and then uses the lapply method to extract
## the first column of the matrix
lapplyWithMatrix <- function() {
  x <- list(a = matrix(1:4, 2, 2))
  
  ## extract the first column
  firstCol <- lapply(x, function(elt)elt[,1])
  
  ## extract the first row
  firstRow <- lapply(x, function(elt)elt[1,])
  
  message(sprintf("First Row: %s ", firstRow))
  message(sprintf("First Col: %s ", firstCol))
  x
}

##sapply is variant of lapply, that will try to simplify output to a vector or matrix
## If it can't figure out how to simplify itwill just return a list
sapplyExample <- function()  {
  x <- list(a=1:4, b=rnorm(10), c=rnorm(20, 1), d=rnorm(100, 5))
  lapply(X, mean)
  
}

#apply - Image you want to take the mean of the first column of a matrix
# x <- apply()



