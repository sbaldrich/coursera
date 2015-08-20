pollutantmean <- function(directory, pollutant, id = 1:332) {
  data <- do.call("rbind", lapply(list.files(directory, pattern = "*.csv"),
          function(x) read.csv(paste("specdata", x, sep="/"))))
  mean(data[data$ID %in% c(id), pollutant], na.rm = TRUE)
}