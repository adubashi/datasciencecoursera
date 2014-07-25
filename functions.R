add2 <- function(a,b) {
        x + y
}

above10 <- function(a,b) {
    use <- x > 10
    x[use]
}

above <- function(x,n = 10) {
  use <- x > n
  x[use]
}

columnmean <- function(y) {
    nc <- ncol(y)
    means <- numeric(nc)
    for(i in 1:nc){
      means[i] <- mean(y[, i])
    }
    means
}