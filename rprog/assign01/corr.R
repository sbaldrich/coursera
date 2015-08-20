corr <- function(directory, threshold = 0){
  ans <- numeric()
  for(file in list.files(path=directory)){
    csv <- read.csv(paste(directory, file, sep="/"))
    csv <- csv[complete.cases(csv),]
    if(nrow(csv) > threshold){
      ans <- c(ans, cor(csv$sulfate, csv$nitrate))
    }
  }
  ans
}