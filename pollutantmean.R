pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  ## Get a list of filenames
  filenames <- list.files(path=directory, pattern="*.csv")
  
  ## Initialize a vector to hold values
  vals <- vector()
  
  ## Loop over the passed id's
  for(i in id) {
    
    
    filename <- sprintf("%03d.csv", i)
    filepath <- paste(directory, filename, sep="/")
    
    
    data <- read.csv(filepath)
    
    
    d <- data[,pollutant]
    d <- d[!is.na(d)]
    
    vals <- c(vals, d)
  }
  
  ## Return the value rounded to 3 dec places
  round(mean(vals), 3)
}

