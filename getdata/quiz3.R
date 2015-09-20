# The American Community Survey distributes downloadable data about United States communities. Download the 2006 microdata survey about housing for the state of Idaho using download.file() from here: 
#  https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv 
# and load the data into R. The code book, describing the variable names is here: 
#  https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf 
# Create a logical vector that identifies the households on greater than 10 acres who sold more than $10,000 worth of agriculture products. Assign that logical vector to the variable agricultureLogical. Apply the which() function like this to identify the rows of the data frame where the logical vector is TRUE. which(agricultureLogical) What are the first 3 values that result?

q1 <- function(){
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
  if(!file.exists("data"))
    dir.create("data")
  fileName <- "./data/acs.csv"
  download.file(url, destfile = fileName)
  acs <- read.csv(fileName)
  acres <- acs["ACR"] == 3
  sales <- acs["AGS"] == 6
  agricultureLogical <- acres & sales
  which(agricultureLogical)
}

# Using the jpeg package read in the following picture of your instructor into R 
#  https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg 
# Use the parameter native=TRUE. What are the 30th and 80th quantiles of the resulting data? (some Linux systems may 
# produce an answer 638 different for the 30th quantile)

q2 <- function(){
  if(!file.exists("data"))
    dir.create("data")
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
  destFile <- "data/jeff.jpg"
  download.file(fileUrl, destfile = destFile)
  img <- readJPEG(destFile, native = TRUE)
  quantile(img,probs = c(30,80)/100) 
}

# Load the Gross Domestic Product data for the 190 ranked countries in this data set: 
#  https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv 
# Load the educational data from this data set: 
#  https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv 
# Match the data based on the country shortcode. How many of the IDs match? 
# Sort the data frame in descending order by GDP rank (so United States is last). 
# What is the 13th country in the resulting data frame? 
#
# Original data sources: 
#   http://data.worldbank.org/data-catalog/GDP-ranking-table 
# http://data.worldbank.org/data-catalog/ed-stats

# What is the average GDP ranking for the "High income: OECD" and "High income: nonOECD" group?

# Cut the GDP ranking into 5 separate quantile groups. Make a table versus 
# Income.Group. How many countries are Lower middle income but among the 38 nations with 
# highest GDP?

q3q4q5 <- function(){
  library(data.table)
  gdp <- data.table(read.csv(url("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv "),
                  skip = 4, nrows = 190))
  setnames(gdp, c("X","X.1", "X.3", "X.4"), c("CountryCode", "GDPRank", "Name", "GDP"))
  
  edu <- data.table(read.csv(url("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv")))
  merged <- data.table(merge(gdp, edu, by = "CountryCode", all=T ))
  
  sum(!is.na(unique(merged$GDPRank)))
  #189
  merged[order(GDPRank, decreasing = T), list(CountryCode, Name)][13]
  ## KNA St. Kitts and Nevis
  
  merged[, mean(GDPRank, na.rm = T), by = Income.Group]
  
  breaks <- quantile(merged$GDPRank, probs = seq(0, 1, 0.2), na.rm = T)
  merged$gdpQuantile <- cut(merged$GDPRank, breaks = breaks)
  merged[Income.Group == "Lower middle income", .N, by = c("Income.Group", "gdpQuantile")]
  
}