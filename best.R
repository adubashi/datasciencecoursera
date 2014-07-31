best <- function(state, outcome) {
  
  outcomes = c("heart attack", "heart failure", "pneumonia")
  if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  
  
  ## Check that state and outcome are valid
  
  data <- data[c(2, 7, 11, 17, 23)]
  names(data)[1] <- "name"
  names(data)[2] <- "state"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
  states <- data[, 2]
  states <- unique(states)
  if( state %in% states == FALSE ) stop("invalid state")
  ## Return hospital name in that state with lowest 30-day death
  
  data <- data[data$state==state & data[outcome] != 'Not Available', ]
  vals <- data[, outcome]
  rowNum <- which.min(vals)
  
  data[rowNum, ]$name
  ## rate
}
