#The American Community Survey distributes downloadable data about United States communities. Download the 2006 microdata survey about housing for the state of Idaho using download.file() from here: 
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv 
#and load the data into R. The code book, describing the variable names is here: 
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf 
# How many properties are worth $1,000,000 or more?#

q1 <- function(){
  if(!file.exists("data"))
    dir.create("data")
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
  download.file(url, destfile = "./data/communities.csv")
  data <- read.csv("./data/communities.csv")
  property_values <- data$VAL
  na <- is.na(property_values) 
  property_values <- property_values[!na]
  length(property_values[property_values == 24])
}

#Download the Excel spreadsheet on Natural Gas Aquisition Program here: 
#https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx 
#Read rows 18-23 and columns 7-15 into R and assign the result to a variable called:
#  dat 
#What is the value of:
#  sum(dat$Zip*dat$Ext,na.rm=T) 

q3 <- function(){
  library(xlsx)
  if(!file.exists("data"))
    dir.create("data")
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
  filename <- "./data/GAP.xlsx"
  download.file(url, destfile = filename, method="wb")
  colIndex <- 7:15
  rowIndex <- 18:23 
  dat <- read.xlsx(filename, sheetIndex = 1, colIndex=colIndex, rowIndex = rowIndex, header = T)
  sum(dat$Zip*dat$Ext,na.rm=T)
}

#Read the XML data on Baltimore restaurants from here: 
#https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml 
#How many restaurants have zipcode 21231?

q4 <- function(){
  library(XML)
  if(!file.exists("data"))
    dir.create("data")
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
  filename <- "./data/restaurants.xml"
  download.file(url, destfile = filename)
  doc <- xmlTreeParse(filename, useInternalNodes = T)
  root <- xmlRoot(doc)
  xmlName(root)
  zipcodes <- xpathSApply(root,"//zipcode",xmlValue)
  length(zipcodes[zipcodes == 21231])
}

#The American Community Survey distributes downloadable data about United States communities. Download the 2006 microdata survey about housing for the state of Idaho using download.file() from here: 
#  https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv 
#using the fread() command load the data into an R object
#  DT 
#Which of the following is the fastest way to calculate the average value of the variable
#  pwgtp15 
#  broken down by sex using the data.table package?

q5 <- function(){
  library(data.table)
  if(!file.exists("data"))
    dir.create("data")
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
  filename <- "./data/pid.csv"
  download.file(url, destfile = filename)
  DT <- fread(filename)
  
  times <- 100
  approach2 <- replicate(times, system.time({mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)})["user.self"])
  approach3 <- replicate(times, system.time({sapply(split(DT$pwgtp15,DT$SEX),mean)})["user.self"])
  approach4 <- replicate(times, system.time({DT[,mean(pwgtp15),by=SEX]})["user.self"])
  approach6 <- replicate(times, system.time({tapply(DT$pwgtp15,DT$SEX,mean)})["user.self"])
  
  approach2_av <- cumsum(approach2) / seq_along(approach2)
  approach3_av <- cumsum(approach3) / seq_along(approach3)
  approach4_av <- cumsum(approach4) / seq_along(approach4)
  approach6_av <- cumsum(approach6) / seq_along(approach6)
  
  topY = max(approach2, approach3, approach4, approach6) 
  lowY = min(approach2, approach3, approach4, approach6) 
  plot(approach2_av, type="l", col="#FF000099", ylim=c(lowY,topY), xlab="Experiments", ylab="Average time")
  lines(approach3_av, col="#00FF0099")
  lines(approach4_av, col="#0000FF99")
  lines(approach6_av, col="#00000099")
}
