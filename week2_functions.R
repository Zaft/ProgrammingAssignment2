 add2 <- function(x, y){
   x + y
 }
 
 above10 <- function(x){
   use <- x > 10
   x[use]
 }
 
 above <- function(x, n = 10){
   use <- x > n
   x[use]
 }
 
 columnMean <- function(x, removeNA = TRUE) {
   nc <- ncol(x)
   means <- numeric(nc)
   for(i in 1:nc) {
     means[i] <- mean(x[,i], na.rm = removeNA)
   }
   means
 }
 
f <- function(x) {
        g <- function(y) {
                y + z
        }
        z <- 4
        x + g(x)
}

x <- 5
y <- if(x < 3) {
        NA
} else {
        10
}