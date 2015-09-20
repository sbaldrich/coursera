# Register an application with the Github API here https://github.com/settings/applications. 
# Access the API to get information on your instructors repositories 
# (hint: this is the url you want "https://api.github.com/users/jtleek/repos"). 
# Use this data to find the time that the datasharing repo was created.
# What time was it created? This tutorial may be useful
# (https://github.com/hadley/httr/blob/master/demo/oauth2-github.r).
# You may also need to run the code in the base R package and not R studio.

q1 <- function(){
  library(httr)
  library(jsonlite)
  library(dplyr)
  
  client_id = "c l i e n t  i d"
  client_secret = "c l i e n t  s e c r e t"
  
  app <- oauth_app("github", key = client_id, secret = client_secret)
  github_token <- oauth2.0_token(oauth_endpoints("github"), app)
  
  gtoken <- config(token = github_token)
  req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
  stop_for_status(req)
  response <- content(req)
  data <- fromJSON(toJSON(response), flatten = T) #Get the response JSON into a useful data frame.
  data %>% filter(name == 'datasharing') %>% select(created_at)
  
}

# The sqldf package allows for execution of SQL commands on R data frames.
# We will use the sqldf package to practice the queries we might send with the 
# dbSendQuery command in RMySQL. Download the American Community Survey data and
# load it into an R object called
#  acs
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv 
# Which of the following commands will select only the data for the probability weights 
# pwgtp1 with ages less than 50?

# Using the same data frame you created in the previous problem, what is the
# equivalent function to unique(acs$AGEP)


q2q3 <- function(){
  library(sqldf)
  if(!file.exists("data"))
    dir.create("data")
  destFile = "data/acs.csv"
  fileUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
  download.file(fileUrl, destfile = destFile)
  acs <- read.csv(destFile)
  sqldf("SELECT pwgtp1 FROM acs WHERE AGEP < 50")
  
  identical(unique(acs$AGEP), sqldf("SELECT DISTINCT(AGEP) FROM acs")$AGEP)
  
}

#How many characters are in the 10th, 20th, 30th and 100th lines of HTML from this page: 
#  http://biostat.jhsph.edu/~jleek/contact.html 
# (Hint: the nchar() function in R may be helpful)
q4 <- function(){
  library(httr)
  html <- readLines(url("http://biostat.jhsph.edu/~jleek/contact.html"))
  sapply(html[c(10,20,30,100)], nchar)
}

# Read this data set into R and report the sum of the numbers in the fourth of 
# the nine columns. 
# https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for 
# Original source of the data: http://www.cpc.ncep.noaa.gov/data/indices/wksst8110.for 
# (Hint this is a fixed width file format)

q5 <- function(){
  data <- read.fwf(file=url(fileUrl), skip=4,
                   widths = c(12, 7, 4, 9, 4, 9, 4, 9, 4))
  sum(data[,4])
}