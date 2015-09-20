rank_it <- function(data, col, state, num){
  data <- data[data$State == state,]
  outcome <- data[,col]
  min_index <- which(outcome == min(outcome, na.rm = TRUE))
  return(data[min_index,2])
}

rankhospital <- function(state, outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  valid_outcome <- c("heart attack", "heart failure", "pneumonia")
  if(!state %in% data$State)
    stop("invalid state")
  if(!outcome %in% valid_outcome)
    stop("invalid outcome")
  for(i in c(11,17,23))
    data[,i] <- as.numeric(data[,i])
  if(num == best)
    return(best(state, outcome))
  else{
    if(outcome == "heart attack")
      return(rank_it(data, 11, state, num))
    else if(outcome == "heart failure")
      return(rank_it(data, 17, state, num))
    else
      return(rank_it(data, 23, state, num))
  }
}