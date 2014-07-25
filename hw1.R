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

complete <- function(directory, id = 1:332) {
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  # get the length of id vector
  id_len <- length(id)
  complete_data <- rep(0, id_len)
  
  
  # find all files in the specdata folder
  all_files <- as.character( list.files(directory) )
  file_paths <- paste(directory, all_files, sep="")
  
  x <- 1 
  for (i in id) {
    current_file <- read.csv(file_paths[i], header=T, sep=",")
    complete_data[i] <- sum(complete.cases(current_file))
    x <- x + 1
  }
  result <- data.frame(id = id, nobs = complete_data)
  return(result)
}

corr <- function(directory, threshold = 0) {
  completes <- complete(directory, 1:332)
  completes <- subset(completes, nobs > threshold )
  
  ## Initialize variables
  correlations <- vector()
  
  ## Loop over the passed id's
  for(i in completes$id ) {
    
    ## Pad the i to create a filename
    filename <- sprintf("%03d.csv", i)
    filepath <- paste(directory, filename, sep="/")
    
    ## Load the data
    data <- read.csv(filepath)
    
    ## Calculate and store the count of complete cases
    completeCases <- data[complete.cases(data),]
    count <- nrow(completeCases)
    
    ## Calculate and store the count of complete cases
    ## if threshhold is reached
    if( count >= threshold ) {
      correlations <- c(correlations, cor(completeCases$nitrate, completeCases$sulfate) )
    }
  }
  
  ## Return a numeric vector of correlations
  correlations
}
