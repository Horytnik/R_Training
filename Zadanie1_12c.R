install.packages("devtools")

install.packages("httr")
install.packages("jsonlite")

library(httr)
library(jsonlite)
library(devtools)


apiLink<-"https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=IBM&interval=5min&apikey=demo"
stockData <- GET(apiLink)
stockText <- content(stockData,"text")
stockJson <- fromJSON(stockText, flatten = TRUE)
stockDF <- as.data.frame(stockJson)


View(stockDF)

compName <- stockDF[2]
openVal <- stockDF[7]

View(compName)
View(openVal)
