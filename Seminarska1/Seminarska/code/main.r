ucni_podatki <- read.table(file="ucnaSem1.txt", sep=",", header = TRUE)
testni_podatki <- read.table(file="testnaSem1.txt", sep=",", header = TRUE)

ucni_podatki$datum <- as.Date(ucni_podatki$datum, format = "%Y-%m-%d")
ucni_podatki$regija <- as.factor(ucni_podatki$regija)
ucni_podatki$namembnost <- as.factor(ucni_podatki$namembnost)
ucni_podatki$oblacnost <- as.factor(ucni_podatki$oblacnost)

testni_podatki$datum <- as.Date(testni_podatki$datum, format = "%Y-%m-%d")
testni_podatki$regija <- as.factor(testni_podatki$regija)
testni_podatki$namembnost <- as.factor(testni_podatki$namembnost)
testni_podatki$oblacnost <- as.factor(testni_podatki$oblacnost)


#############################################################################
##############  NALOGA 1 - GRAFI ############################################
#############################################################################

# grafi priprava
leta <- unique(ucni_podatki$leto_izgradnje)
stavbe <- unique(ucni_podatki$stavba)
leta_stavb <- apply(data.frame(stavbe), 1, function(x) ucni_podatki$leto_izgradnje[ucni_podatki$stavba == x][1])
povprecja_stavb <- apply(data.frame(stavbe), 1, function(x) mean(ucni_podatki$poraba[ucni_podatki$stavba == x]))
namembnost <- unique(ucni_podatki$namembnost)
povprecja_namembnosti <- apply(data.frame(namembnost), 1, function(x) mean(ucni_podatki$poraba[ucni_podatki$namembnost == x]))

# grafi izris
# plot: poraba za vsako stavbo označeno, kdaj je bila zgrajena
png("grafi/poraba_stavbe.png")
plot(
  x=leta_stavb,
  y=povprecja_stavb,
  log="y",
  xlab = "Leto izgradnje", ylab = "Povprečna poraba")
dev.off()

# barplot: povprečna poraba na namembnost
png("grafi/poraba_namembnost.png", res=600, width=4800, height=4800)
barplot(povprecja_namembnosti, names.arg = namembnost, ylab = "Povprečna poraba")
dev.off()

# piechart: delež stavb po namembnosti
png("grafi/stavbe_namembnost.png")
namembnosti_stavb <- apply(data.frame(stavbe), 1, function(x) ucni_podatki$namembnost[ucni_podatki$stavba == x][1])
pie(table(namembnosti_stavb))
dev.off()

# hist: stavbe po letih
png("grafi/stavbe_leta.png")
leta_stavb <- apply(data.frame(unique(ucni_podatki$stavba)), 1, function(x) ucni_podatki$leto_izgradnje[ucni_podatki$stavba == x][1])
hist(leta_stavb, xlab="Leto", ylab="St. stavb", main="")
dev.off()

# plot: poraba glede na površino stavbe
png("grafi/poraba_pvrsina.png")
povrsina_stavb <- apply(data.frame(stavbe), 1, function(x) ucni_podatki$povrsina[ucni_podatki$stavba == x][1])
plot(x=povrsina_stavb, y=povprecja_stavb, log="y", xlab = "povrsina", ylab = "poraba")
dev.off()

#barplot: površina stavbe na namembnost
png("grafi/povrsine_namembnosti.png", res=600, width=4800, height=4800)
povprecja_povrsin <- apply(data.frame(namembnost), 1, function(x) mean(ucni_podatki$povrsina[ucni_podatki$namembnost == x]))
barplot(povprecja_povrsin, names.arg = namembnost)
dev.off()


# barplot: povprečna poraba glede na regijo
png("grafi/poraba_regije.png")
regije <- unique(ucni_podatki$regija)
povprecja_regije <- apply(data.frame(regije), 1, function(x) mean(ucni_podatki$poraba[ucni_podatki$regija == x]))
barplot(povprecja_regije, names.arg = regije)
dev.off()

# piechart: primerjava porazdelitve satvb po vzhodni in zahodni regiji
stavbe_zahodna <- ucni_podatki$stavba[ucni_podatki$regija == "zahodna"]
stavbe_vzhodna <- ucni_podatki$stavba[ucni_podatki$regija == "vzhodna"]
namembnosti_stavb_zahodna <- apply(data.frame(stavbe_zahodna), 1, function(x) ucni_podatki$namembnost[ucni_podatki$stavba == x][1])
namembnosti_stavb_vzhodna <- apply(data.frame(stavbe_vzhodna), 1, function(x) ucni_podatki$namembnost[ucni_podatki$stavba == x][1])
png("grafi/regije_namembnost_zahod.png")
pie(table(namembnosti_stavb_zahodna), main = "Zahod")
dev.off()
png("grafi/regije_namembnost_vzhod.png")
pie(table(namembnosti_stavb_vzhodna), main = "Vzhod")
dev.off()





#############################################################################
##########  NALOGA 2 - OCENJEVANJE ATRIBUTOV IN DODAJANJE  ##################
#############################################################################

library(CORElearn)
sort(attrEval(namembnost ~ ., ucni_podatki, "InfGain"), decreasing = TRUE)
sort(attrEval(namembnost ~ ., ucni_podatki, "Gini"), decreasing = TRUE)
sort(attrEval(namembnost ~ ., ucni_podatki, "GainRatio"), decreasing = TRUE)
sort(attrEval(namembnost ~ ., ucni_podatki, "MDL"), decreasing = TRUE)
sort(attrEval(namembnost ~ ., ucni_podatki, "ReliefFequalK"), decreasing = TRUE)



library(chron)
# dodajanje atributov - ucni_podatki
# vikend
ucni_podatki$vikend <- is.weekend(ucni_podatki$datum)
ucni_podatki$vikend <- as.factor(ucni_podatki$vikend)
# mesec
ucni_podatki$mesec <- as.numeric(format(ucni_podatki$datum, "%m"))
ucni_podatki$mesec <- as.factor(ucni_podatki$mesec)
# letni casi
for (i in 1:nrow(ucni_podatki)) {
  if (ucni_podatki$mesec[i] == 12 || ucni_podatki$mesec[i] == 1 || ucni_podatki$mesec[i] == 2) {
    ucni_podatki$letni_cas[i] <- "zima"
  } else if (ucni_podatki$mesec[i] == 3 || ucni_podatki$mesec[i] == 4 || ucni_podatki$mesec[i] == 5) {
    ucni_podatki$letni_cas[i] <-"pomlad"
  } else if (ucni_podatki$mesec[i] == 6 || ucni_podatki$mesec[i] == 7 || ucni_podatki$mesec[i] == 8) {
    ucni_podatki$letni_cas[i] <- "poletje"
  } else {
    ucni_podatki$letni_cas[i] <- "jesen"
  }
}
ucni_podatki$letni_cas <- as.factor(ucni_podatki$letni_cas)
# povprečna poraba prejsnega dne
povp_prejsni_dan <- NULL
for (i in 1:nrow(ucni_podatki)) {
  povp_prejsni_dan[i] <- mean(as.numeric(ucni_podatki$poraba[ucni_podatki$stavba == ucni_podatki$stavba[i] & (ucni_podatki$datum < ucni_podatki$datum[i] | ucni_podatki$datum > ucni_podatki$datum[i]-2)]))
}
ucni_podatki$povp_prejsni_dan <- NULL
ucni_podatki$povp_prejsni_dan <- povp_prejsni_dan
# povprečna poraba prejsnega tedna
povp_prejsni_teden <- NULL
for (i in 1:nrow(ucni_podatki)) {
  povp_prejsni_teden[i] <- mean(as.numeric(ucni_podatki$poraba[ucni_podatki$stavba == ucni_podatki$stavba[i] & (ucni_podatki$datum < ucni_podatki$datum[i] | ucni_podatki$datum > ucni_podatki$datum[i]-8)]))
}
ucni_podatki$povp_prejsni_teden <- NULL
ucni_podatki$povp_prejsni_teden <- povp_prejsni_teden



# dodajanje atributov - testni_podatki
# vikend
testni_podatki$vikend <- is.weekend(testni_podatki$datum)
testni_podatki$vikend <- as.factor(testni_podatki$vikend)
# mesec
testni_podatki$mesec <- as.numeric(format(testni_podatki$datum, "%m"))
testni_podatki$mesec <- as.factor(testni_podatki$mesec)
# letni casi
for (i in 1:nrow(testni_podatki)) {
  if (testni_podatki$mesec[i] == 12 || testni_podatki$mesec[i] == 1 || testni_podatki$mesec[i] == 2) {
    testni_podatki$letni_cas[i] <- "zima"
  } else if (testni_podatki$mesec[i] == 3 || testni_podatki$mesec[i] == 4 || testni_podatki$mesec[i] == 5) {
    testni_podatki$letni_cas[i] <-"pomlad"
  } else if (testni_podatki$mesec[i] == 6 || testni_podatki$mesec[i] == 7 || testni_podatki$mesec[i] == 8) {
    testni_podatki$letni_cas[i] <- "poletje"
  } else {
    testni_podatki$letni_cas[i] <- "jesen"
  }
}
testni_podatki$letni_cas <- as.factor(testni_podatki$letni_cas)
# povprečna poraba prejsnega dne
povp_prejsni_dan <- NULL
for (i in 1:nrow(testni_podatki)) {
  povp_prejsni_dan[i] <- mean(as.numeric(testni_podatki$poraba[testni_podatki$stavba == testni_podatki$stavba[i] & (testni_podatki$datum < testni_podatki$datum[i] | testni_podatki$datum > testni_podatki$datum[i]-2)]))
  
}
testni_podatki$povp_prejsni_dan <- NULL
testni_podatki$povp_prejsni_dan <- povp_prejsni_dan
# povprečna poraba prejsnega dne
povp_prejsni_teden <- NULL
for (i in 1:nrow(testni_podatki)) {
  povp_prejsni_teden[i] <- mean(as.numeric(testni_podatki$poraba[testni_podatki$stavba == testni_podatki$stavba[i] & (testni_podatki$datum < testni_podatki$datum[i] | testni_podatki$datum > testni_podatki$datum[i]-8)]))
  
}
testni_podatki$povp_prejsni_teden <- NULL
testni_podatki$povp_prejsni_teden <- povp_prejsni_teden



#############################################################################
#################  NALOGA 3 - NAPOVEDOVANJE  ################################
#############################################################################

# testne funkcije
set.seed(2)
# regresija
# srednja absolutna napaka
mae <- function(obs, pred)
{
  mean(abs(obs - pred))
}

# srednja kvadratna napaka
mse <- function(obs, pred)
{
  mean((obs - pred)^2)
}

# relativna srednja absolutna napaka
rmae <- function(obs, pred, mean.val) 
{  
  sum(abs(obs - pred)) / sum(abs(obs - mean.val))
}

# relativna srednja kvadratna napaka
rmse <- function(obs, pred, mean.val) 
{  
  sum((obs - pred)^2)/sum((obs - mean.val)^2)
}



# klasifikacija
# CA
CA <- function(obs, pred)
{
  tab <- table(obs, pred)

  sum(diag(tab)) / sum(tab)
}

# Brier Score
brier.score <- function(obsMat, predMat)
{
  sum((obsMat - predMat) ^ 2) / nrow(predMat)
}

# Information Score
inf.score <- function(trainClass, testClass, predMat)
{
  result <- 0
  
  priors <- table(trainClass)/length(trainClass)
  
  for (i in 1:nrow(predMat))
  {
    p.prior <- priors[[testClass[i]]]
    p.posterior <- predMat[i, testClass[i]]
    
    if (p.posterior >= p.prior)
      result <- result - log2(p.prior) + log2(p.posterior)
    else
      result <- result + log2(1-p.prior) - log2(1-p.posterior)				
  }
  
  result/nrow(predMat)
}




###############   NAPOVED KLASIFIKACIJA   ###################################
# večinski klasifikator
# če pogledamo summary za učne podatke vidimo da je najbolj pogosta vrednost za namembnost: izobraževalna
summary(ucni_podatki)
vec_klas <- sum(ucni_podatki$namembnost == "izobrazevalna") / length(ucni_podatki$namembnost)
print(vec_klas)
vec_klas <- sum(testni_podatki$namembnost == "izobrazevalna") / length(testni_podatki$namembnost)
print(vec_klas)

# priprava podatkov
set.seed(2)
train <- ucni_podatki
test <- testni_podatki
observed <- test$namembnost
library(nnet)
obsMat <- class.ind(observed)


# Odločitveno drevo
# celotna množica podatkov
set.seed(2)
library(rpart)
dt <- rpart(namembnost ~ ., data = train)
predicted <- predict(dt, test, type="class")
CA(observed, predicted) # 0.4966555
predMat <- predict(dt, test, type = "prob")
brier.score(obsMat, predMat) # 0.9950272
inf.score(train$namembnost, test$namembnost, predMat) # 0.5367325

# izbrani atributi
set.seed(2)
dt <- rpart(namembnost ~ stavba + povrsina + poraba, data = train)
predicted <- predict(dt, test, type="class")
CA(observed, predicted) # 0.5434783
predMat <- predict(dt, test, type = "prob")
brier.score(obsMat, predMat) # 0.8767163
inf.score(train$namembnost, test$namembnost, predMat) # 0.7091357

# rezanje z izbranimi atributi
set.seed(2)
dt <- rpart(namembnost ~ stavba + povrsina + poraba, data=train, cp=0)
# rpart.plot(dt)
# rpart med gradnjo drevesa interno ocenjuje njegovo kvaliteto 
#printcp(dt)
tab <- printcp(dt)
# izberemo vrednost parametra cp, ki ustreza minimalni napaki internega presnega preverjanja
row <- which.min(tab[,"xerror"])
th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))
# porezemo drevo z izbrano nastavitvijo
dt <- prune(dt, cp=th)
# rpart.plot(dt)

predicted <- predict(dt, test, type="class")
CA(observed, predicted) # 0.5570234

predMat <- predict(dt, test, type = "prob")
brier.score(obsMat, predMat) # 0.885451
inf.score(train$namembnost, test$namembnost, predMat) # 0.7775018


# Random forest
# vsi podakti
set.seed(2)
library(CORElearn)
rf <- CoreModel(namembnost ~ ., data = train, model="rf")
predicted <- predict(rf, test, type="class")
CA(observed, predicted) # 0.5547659

predMat <- predict(rf, test, type = "prob")
brier.score(obsMat, predMat) # 0.6294118


# izbrani atributi
set.seed(2)
library(CORElearn)
rf <- CoreModel(namembnost ~ stavba + povrsina + poraba + povp_prejsni_dan, data = train, model="rf")
predicted <- predict(rf, test, type="class")
CA(observed, predicted) # 0.5726589

predMat <- predict(rf, test, type = "prob")
brier.score(obsMat, predMat) # 0.6619525



# Naivni Bayes
# vsi podatki
set.seed(2)
library(CORElearn)
nb <- CoreModel(namembnost ~ ., data = train, model="bayes")
predicted <- predict(nb, test, type="class")
CA(observed, predicted) # 0.4388796

predMat <- predict(nb, test, type = "prob")
brier.score(obsMat, predMat) # 0.8094718


# izbrani atributi
set.seed(2)
library(CORElearn)
nb <- CoreModel(namembnost ~ stavba + povrsina + poraba + povp_prejsni_dan, data = train, model="bayes")
predicted <- predict(nb, test, type="class")
CA(observed, predicted) # 0.5263796

predMat <- predict(nb, test, type = "prob")
brier.score(obsMat, predMat) # 0.6823086




###############   NAPOVED REGRESIJA   ###################################
# priprava podatkov
set.seed(2)
train <- ucni_podatki
test <- testni_podatki
observed <- test$poraba

# LINEARNA REGRESIJA
# vsi atributi
model <- lm(poraba ~ ., train)
predicted <- predict(model, test)
for (i in 1:length(predicted)) {
  if (is.nan(predicted[i])) {
    predicted[i] = mean(train$poraba)
  }
}
plot(observed)
points(predicted, col="green")
mae(observed, predicted) # 36.64551
mse(observed, predicted) # 4817.991
rmae(observed, predicted, mean(train$poraba)) # 0.2293964
rmse(observed, predicted, mean(train$poraba)) # 0.1065846


# izbrani atributi na roke
model <- lm(poraba ~ stavba + povrsina + poraba + povp_prejsni_dan, train)
predicted <- predict(model, test)
for (i in 1:length(predicted)) {
  if (is.nan(predicted[i])) {
    predicted[i] = mean(train$poraba)
  }
}
plot(observed)
points(predicted, col="green")
mae(observed, predicted) # 33.21662
mse(observed, predicted) # 5127.323
rmae(observed, predicted, mean(train$poraba)) # 0.207932
rmse(observed, predicted, mean(train$poraba)) # 0.1134277


# WRAPPER
# source("wrapper.R")
# trainfn_tree <- function(formula, traindata){
#   lm(formula, traindata)
# }
# predictfn <- function(model, testdata){
#   predict(model, testdata)
# }
# evalfn <- function(predicted, observed, trained){
#   sum((observed - predicted)^2)/sum((observed - mean(trained))^2)
# }
# wrapper(poraba ~ ., train, trainfn_tree, predictfn, evalfn, cvfolds=10)


# izbrani atributi glede na wrapper (izbere vse, in glede na rmse lahko atribute zmanjšamo na enega)
model <- lm(poraba ~ poraba ~ povp_prejsni_dan, train)
predicted <- predict(model, test)
for (i in 1:length(predicted)) {
  if (is.nan(predicted[i])) {
    predicted[i] = mean(train$poraba)
  }
}
plot(observed)
points(predicted, col="green")
mae(observed, predicted) # 36.64551
mse(observed, predicted) # 4817.991
rmae(observed, predicted, mean(train$poraba)) # 0.2293964
rmse(observed, predicted, mean(train$poraba)) # 0.1065846



# REGRESIJSKO DREVO
library(rpart)
library(rpart.plot)
# vsi atributi brez rezanja drevesa
rt.model <- rpart(poraba ~ ., data=train)
rpart.plot(rt.model)
predicted <- predict(rt.model, test)
mae(observed, predicted) # 53.87275
mse(observed, predicted) # 7289.31
rmae(observed, predicted, mean(train$poraba)) # 0.3372368
rmse(observed, predicted, mean(train$poraba)) # 0.1612556

# rezanje drevesa
rt.model <- rpart(poraba ~ ., data=train, cp=0)
#rpart.plot(rt.model)
tab <- printcp(rt.model)
row <- which.min(tab[,"xerror"])
th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))
rt.model <- prune(rt.model, cp=th)
#rpart.plot(rt.model)
predicted <- predict(rt.model, test)
mae(observed, predicted) # 36.16087
mse(observed, predicted) # 5895.206
rmae(observed, predicted, mean(train$poraba)) # 0.2263626
rmse(observed, predicted, mean(train$poraba)) # 0.1304149



# NAKLJUČNI GOZD
library(randomForest)
# vsi atributi
rf.model <- randomForest(poraba ~ ., train)
predicted <- predict(rf.model, test)
mae(observed, predicted) # 32.73794
mse(observed, predicted) # 4248.439
rmae(observed, predicted, mean(train$poraba)) # 0.2049355
rmse(observed, predicted, mean(train$poraba)) # 0.09398484



# K-NAHJBLIŽJIH SOSEDOV
library(kknn)
# vsi atributi
knn.model <- kknn(poraba ~ ., train, test, k = 5)
predicted <- fitted(knn.model)
mae(observed, predicted) # 55.49995
mse(observed, predicted) # 7238.434
rmae(observed, predicted, mean(train$poraba)) # 0.3474229
rmse(observed, predicted, mean(train$poraba)) # 0.1601301
