#The American Community Survey distributes downloadable data about United States communities. Download the 2006 microdata survey about housing for the state of Idaho using download.file() from here: 
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv 
#and load the data into R. The code book, describing the variable names is here: 
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf 
#Apply strsplit() to split all the names of the data frame on the characters "wgtp". What is the value of the 123 element of the resulting list?

ensure_data_dir <- function(){
  if(!file.exists("data"))
    dir.create("data")
}

q1 <- function(){
  ensure_data_dir()
  url = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
  filename = "./data/housing.csv"
  download.file(url, destfile = filename)
  hid <- read.csv(filename)
  strsplit(names(hid), "wgtp")[123]
}

# Load the Gross Domestic Product data for the 190 ranked countries in this data set: 
#  https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv 
# Remove the commas from the GDP numbers in millions of dollars and average them. What is the average? 
# Original data sources: http://data.worldbank.org/data-catalog/GDP-ranking-table

q2 <- function(){
  ensure_data_dir()
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
  filename = "data/GDP.csv"
  download.file(url, destfile = filename)
  # Read the data. Thanks to bquast @ Github
  GDP <- read.csv(filename, skip=4, nrows=190)
  gdp_in_millions <- as.numeric(gsub(',','',GDP$X.4))
  mean(gdp_in_millions)
}

#In the data set from Question 2 what is a regular expression that would allow you to count the number 
#of countries whose name begins with "United"? Assume that the variable with the country names in it 
#acs["AGS"]is named countryNames. How many countries begin with United?
q3 <- function(){
  ensure_data_dir()
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
  filename = "data/GDP.csv"
  download.file(url, destfile = filename)
  # Read the data. Thanks to bquast @ Github
  GDP <- read.csv(filename, skip=4, nrows=190)
  length(grep("^United", GDP$X.3))
}

# Load the Gross Domestic Product data for the 190 ranked countries in this data set: 
#  https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv 
# Load the educational data from this data set: 
#  https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv 
# 
# Match the data based on the country shortcode. Of the countries for which the end of the fiscal year is 
# available, how many end in June? 
# Original data sources: 
# http://data.worldbank.org/data-catalog/GDP-ranking-table 
# http://data.worldbank.org/data-catalog/ed-stats# 

q4 <- function(){
  ensure_data_dir()
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
  filename = "data/GDP.csv"
  download.file(url, destfile = filename)
  # Read the data. Thanks to bquast @ Github
  GDP <- read.csv(filename, skip=4, nrows=190)data.table:
  url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv'
  filename = "data/edu.csv"
  download.file(url, destfile = filename)
  edu <- read.csv(filename)
  joined <- merge(GDP, edu, by.x = "X", by.y = "CountryCode")
  length(grep("Fiscal year end: June",joined$Special.Notes))
}

# You can use the quantmod (http://www.quantmod.com/) package to get historical stock prices for publicly  traded companies on the NASDAQ and NYSE. 
# Use the following code to download data on Amazon's stock price and get the times the data was sampled.
#
#  library(quantmod)
#  amzn = getSymbols("AMZN",auto.assign=FALSE)
#  sampleTimes = index(amzn) 
#
# How many values were collected in 2012? How many values were collected on Mondays in 2012?

q5 <- function(){
  library(quantmod)
  library(data.table)
  amzn = getSymbols("AMZN",auto.assign=FALSE)
  sampleTimes = index(amzn)
  sampleTimes = data.table(date = sampleTimes, year = format(sampleTimes, "%Y"), day = format(sampleTimes, "%a"))
  year2012 <- sampleTimes$year == 2012 # A logical vector with 2012 dates.
  mondays <- sampleTimes$day == "lun"
  table(year2012, mondays)
  
}