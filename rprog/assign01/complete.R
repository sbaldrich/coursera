complete <- function(directory, id = 1:332){
  nobs <- numeric()
  for(i in id){
    csv <- read.csv(paste(directory, sprintf("%03d.csv", i), sep="/"))
    nobs <- c(nobs, nrow(csv[complete.cases(csv),]))
  }
  data.frame(id, nobs)
}