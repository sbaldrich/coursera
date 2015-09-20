hospital <- function(data, col, state){
  data <- data[data$State == state,]
  outcome <- data[,col]
  min_index <- which(outcome == min(outcome, na.rm = TRUE))
  return(data[min_index,2])
}

best <- function(state, outcome){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  valid_outcome <- c("heart attack", "heart failure", "pneumonia")
  if(!state %in% data$State)
    stop("invalid state")
  if(!outcome %in% valid_outcome)
    stop("invalid outcome")
  for(i in c(11,17,23))
    data[,i] <- as.numeric(data[,i])
  data <- data[data$State == state,]
  if(outcome == "heart attack")
    return(hospital(data, 11, state))
  else if(outcome == "heart failure")
    return(hospital(data, 17, state))
  else return(hospital(data, 23, state))
}