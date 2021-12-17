setwd("/Users/macbookpro13/Faks/UI/projektna")
data <- read.csv("dataSem1.txt", stringsAsFactors = T)
data$datum <- as.Date(data$datum)
summary(data);


#nov atribut letni časi
getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter 
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring 
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer 
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall 
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}
sez <- getSeason(data$datum)
data$letni_cas <- sez

data$letni_cas <- as.factor(data$letni_cas)

#nov atribut vikend ali delavnik
library(chron)
tf <- is.weekend(data$datum)
tf[tf == "FALSE"] <- "delavnik"
tf[tf == "TRUE"] <- "vikend"
data$delavnik_ali_vikend <- tf

data$delavnik_ali_vikend <- as.factor(data$delavnik_ali_vikend)

# nov atribut povprečna poraba prejsnega dne
povp_prejsni_dan <- NULL
for (i in 1:nrow(data)) {
  povp_prejsni_dan[i] <- mean(as.numeric(data$poraba[data$stavba == data$stavba[i] & data$datum == data$datum[i]-1]))
  
}

data$povp_prejsni_dan <- NULL
data$povp_prejsni_dan <- povp_prejsni_dan

















