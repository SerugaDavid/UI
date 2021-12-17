# Povprecje porabe v poslovnih objektih čez teden vs čez vikend
# opazimo, da ni neke velike razlike. Povprecimo vse meritve vikenda in vse meritve delovnih dni med sabo neodvisno.
weekend <- data[data$namembnost=="poslovna" & is.weekend(data$datum),]
workday <- data[data$namembnost=="poslovna" & !is.weekend(data$datum),]

barplot(c(mean(workday$poraba), mean(weekend$poraba)), 
        names.arg = c("cez teden", "cez vikend"), 
        main="Primerjava povprecne porabe med tednom in vikendom",
        ylab="povprecna poraba (kWh)", 
        col="#69b3a2")

# poraba elektrike skozi leto v izobrazevalnih stavbah, pricakujem manjso porabo poleti
#install.packages("lubridate")
library(lubridate)
izobrazevalne <- data[data$namembnost=="izobrazevalna",]
ag <- aggregate(x = data$poraba,
                by = list(month(data$datum)),
                FUN = "mean")

## vidimo lahko, da ni nizje porabe poleti
barplot(ag[,2], 
        main = "Povp. poraba elektrike za izobraževalne stavbe", 
        xlab="meseci leta", 
        ylab="povprecna poraba skozi vsa leta (kWh)", 
        col="#69b3a2", 
        names.arg = c("jan", "feb", "mar", "apr", "maj", "jun", "jul", "avg", "sept", "okt", "nov", "dec"))

# primerjalna povprecna poraba vsakega tipa izgradb
ag <- aggregate(x = data$poraba,
                by = list(data$namembnost),
                FUN = "mean")

barplot(ag[,2],
        main = "Povp. poraba glede na tip stavbe",
        xlab = "tipi stavb",
        ylab = "povprecna poraba (kWh)",
        col = "#69b3a2",
        names.arg = ag[,1])

# porazdelitev temperature zraka
hist(data$temp_zraka,
     main = "Porazdelitev vseh temperatur zraka",
     xlab = "temperatura (v °C)",
     ylab = "frekvenca pojavitev",
     col = "#69b3a2")

# porazdelitev temperature rosisca
hist(data$temp_rosisca,
     main = "Porazdelitev vseh temperatur rosišča",
     xlab = "temperatura (v °C)",
     ylab = "frekvenca pojavitev",
     col = "#69b3a2")