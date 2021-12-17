## OPOZORILO: deli kode, ki trajajo predolgo (10min+ ) so zakomentirani, da se ne zazenejo

# 1 vizualizacija

# import
df <- read.table(file="dataSem1.txt", sep=",", header=TRUE)
summary(df)

# konverzija podatkov
df$datum <- as.Date(df$datum, format = "%Y-%m-%d")
df$regija <- as.factor(df$regija)
df$stavba <- as.factor(df$stavba)
df$namembnost <- as.factor(df$namembnost)
df$norm_poraba <- as.factor(df$norm_poraba)

# sortiraj po datumu za lazjo obdelavo podatkov
df <- df[order(df$datum),]

summary(df)


# grafi
# povprecna poraba za stavbo 
leta <- unique(df$leto_izgradnje)
stavbe <- unique(df$stavba)
leta_stavb <- apply(data.frame(stavbe), 1, function(x) df$leto_izgradnje[df$stavba == x][1])
povprecja_stavb <- apply(data.frame(stavbe), 1, function(x) mean(df$poraba[df$stavba == x]))
png("graphs/leto_poraba.png")
plot(
    x=leta_stavb,
    y=povprecja_stavb,
    log="y",
    xlab = "Leto izgradnje", ylab = "Poraba")
dev.off()

# delez stavb po namembnosti
png("graphs/namembnost.png") 
namembnosti_stavb <- apply(data.frame(stavbe), 1, function(x) df$namembnost[df$stavba == x][1])
pie(table(namembnosti_stavb))
dev.off()

# porazdelitev startosti stavb
leta_stavb <- apply(data.frame(unique(df$stavba)), 1, function(x) df$leto_izgradnje[df$stavba == x][1])
png("graphs/histogram_leto.png") 
hist(leta_stavb, xlab="Leto", ylab="St. stavb", main="")
dev.off()

# temp. zraka in poraba
png("graphs/temperatura_poraba.png") 
plot(
    x=df$temp_zraka,
    y=df$poraba,
    xlab = "Temperatura zraka", ylab = "Poraba")
dev.off()


# povrsina in poraba
png("graphs/povrsina_poraba.png") 
plot(
    x=df$povrsina,
    y=df$poraba,
    xlab = "Površina stavbe", ylab = "Poraba")
dev.off()

# povrsina in poraba
png("graphs/sqrt_povrsina_poraba.png") 
plot(
    x=sqrt(df$povrsina),
    y=df$poraba,
    xlab = "sqrt(Površina stavbe)", ylab = "Poraba")
dev.off()

# povrsina in poraba
ure <- unique(df$ura)
ure <- sort(ure)
povprecja_ur <- apply(data.frame(ure), 1, function(x) mean(df$poraba[df$ura == x]))
png("graphs/ura_poraba.png")
barplot(
    povprecja_ur,
    names.arg=ure,
    xlab = "Ura meritve", ylab = "Poraba")
dev.off()

# poraba po namembnosti
stavbe <- unique(df$stavba)
porabe <- apply(data.frame(stavbe), 1, function(x) mean(df$poraba[df$stavba == x]))
namembnosti_stavb <- apply(data.frame(stavbe), 1, function(x) df$namembnost[df$stavba == x][1])
namembnosti <- unique(df$namembnost)
povprecja <- apply(data.frame(namembnosti), 1, function(x) mean(porabe[namembnosti_stavb == x]))
png("graphs/namembnost_poraba.png", width=640)
barplot(
    povprecja,
    names.arg=namembnosti,
    xlab = "Namembnost", ylab = "Poraba")
dev.off()

# povrsina po namembnosti
stavbe <- unique(df$stavba)
povrsine <- apply(data.frame(stavbe), 1, function(x) df$povrsina[df$stavba == x][1])
namembnosti_stavb <- apply(data.frame(stavbe), 1, function(x) df$namembnost[df$stavba == x][1])
namembnosti <- unique(df$namembnost)
povprecja <- apply(data.frame(namembnosti), 1, function(x) mean(povrsine[namembnosti_stavb == x]))
png("graphs/namembnost_povrsina.png", width=640)
barplot(
    povprecja,
    names.arg=namembnosti,
    xlab = "Namembnost", ylab = "Površina")
dev.off()


# 2 atributi

summary(df)

# atribut vikend
#install.packages("chron")
library(chron)
df$vikend <- is.weekend(df$datum)
summary(df)

# tekoce povprecje 7 dni za isto uro in isto stavbo
print("Tekoce povprecje ma7")
n <- nrow(df)
min_search <- 1 # optimizacija
for (i in 1:nrow(df)) { #1:nrow(df)
    if (i %% as.integer(n/200) == 0) {
        print(sprintf("%.1f%%", (i/n)*100)) # proggress (traja nekaj minut)
    }
    window <- df[min_search:i,]
    # filtriraj po datumu (7 dni nazaj, brez trenutnega dneva)
    datum_sel <-
        window$datum >= df$datum[i]-7 &
        window$datum < df$datum[i]
    if (any(datum_sel)) min_search <- min_search + (min(which(datum_sel))-1)
    sel <-
        datum_sel &
        window$ura == df$ura[i] &
        window$stavba == df$stavba[i]
    # povprecje zadnjih 7 dni (kolikor pac je podatkov v tem obdobju)
    df$ma7[i] <- mean(window$poraba[sel])
    if (is.nan(df$ma7[i])) {
        df$ma7[i] <- 0
    }
}
print("ma7 done")
df[ df$ura == 23 & df$stavba == 1,]$ma7[1:21]
df[ df$ura == 23 & df$stavba == 1,]$poraba[1:21]
summary(df)

# letni cas
for (i in 1:nrow(df)) {
    mesec <- as.numeric(format(df$datum[i], "%m"))
    if (mesec == 12 || mesec == 1 || mesec == 2){
        df$letni_cas[i] <- "zima"
    } else if (mesec == 3 || mesec == 3 || mesec == 5){
        df$letni_cas[i] <- "pomlad"
    } else if (mesec == 6 || mesec == 7 || mesec == 8){
        df$letni_cas[i] <- "poletje"
    } else if (mesec == 9 || mesec == 10 || mesec == 11){
        df$letni_cas[i] <- "jesen"
    }
}
df$letni_cas <- as.factor(df$letni_cas)
summary(df)


library(CORElearn)
sort(attrEval(norm_poraba ~ ., df, "InfGain"), decreasing = TRUE)
#        stavba         poraba            ma7 leto_izgradnje     namembnost 
#  1.0122433832   0.1353764256   0.1075041557   0.0576120974   0.0548670491 
#      povrsina            ura   temp_rosisca         vikend         regija 
#  0.0240757138   0.0235681488   0.0111544847   0.0065996882   0.0059742970 
#     letni_cas          datum     temp_zraka        pritisk     smer_vetra 
#  0.0051871838   0.0036369087   0.0026924803   0.0017944931   0.0014230198 
#     oblacnost  hitrost_vetra       padavine 
#  0.0005750612   0.0001852129   0.0001569170


# 3 modeliranje
# install.packages(c("CORElearn", "e1071", "randomForest", "kernlab", "nnet", "kknn"))
# REGRESIJA

set.seed(0)
sel <- sample(1:nrow(df), as.integer(nrow(df) * 0.7), F)
train <- df[sel,]
test <- df[-sel,]
train$norm_poraba <- NULL
train$datum <- NULL
summary(train)
summary(test)

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

# za primerjavo
observed <- test$poraba

# linerana regresija
linreg <- lm(poraba ~ . , train)
predicted <- predict(linreg, test)
png("graphs/reg_linearna.png")
plot(observed)
points(predicted, col="red")
dev.off()
rmse(observed, predicted, mean(train$poraba))
#### RES: 0.06117085

# regresijsko drevo
library(rpart)
rt.model <- rpart(poraba ~ ., data=train)
predicted <- predict(rt.model, test)
png("graphs/reg_drevo.png")
plot(observed)
points(predicted, col="red")
dev.off()
rmse(observed, predicted, mean(train$poraba))
#### RES: 0.07995266


# nakljucni gozd (prepocasno)

# svm (prepocasno tudi samo z enim atributom)

# k-najblizjih sosedov

library(kknn)

# optimizacija
#minkInNN <- 1
#maxkInNN <- 10
#est <- vector()
#for (val in minkInNN:maxkInNN)
#{
#    knn.model <- kknn(poraba ~ ., train, test, k = val)
#    predicted <- fitted(knn.model)
#
#	est <- append(est, rmse(observed, predicted, mean(train$poraba)))
#}
#
#names(est) <- minkInNN:maxkInNN
#est
#### RES:
# est
# [1] 0.05800082 0.04978216 0.04448959 0.04164631 0.04006745 0.03917250 0.03876744
# [8] 0.03865635 0.03869005 0.03883658
# lokalni minimum je 8 --> to vrednost uporabimo za k

#knn.model <- kknn(poraba ~ ., train, test, k = 8)
#predicted <- fitted(knn.model)
#png("graphs/reg_knn.png")
#plot(observed)
#points(predicted, col="red")
#dev.off()
#rmse(observed, predicted, mean(train$poraba))
#### RES: 0.03865635

# nevronska mreza (neuporabno zaradi prevelike napake - grej RES rmse)
library(nnet)
set.seed(0)
nn.model <- nnet(poraba ~ . -stavba, train, size = 5, decay = 0.0001, maxit = 10000, linout = T)
# Error in nnet.default(x, y, w, ...) : too many (1076) weights
predicted <- predict(nn.model, test)
png("graphs/reg_nnet.png")
plot(observed)
points(predicted, col="red")
dev.off()
rmse(observed, predicted, mean(train$poraba))
#### RES: 0.296223


# KLASIFIKACIJA

set.seed(0)
sel <- sample(1:nrow(df), as.integer(nrow(df) * 0.7), F)
train <- df[sel,]
test <- df[-sel,]
train$poraba <- NULL
train$datum <- NULL
summary(train)
test$poraba <- NULL
test$datum <- NULL
summary(test)

CA <- function(obs, pred)
{
	t <- table(obs, pred)

	sum(diag(t)) / sum(t)
}

Sensitivity <- function(obs, pred, pos.class)
{
	tab <- table(obs, pred)

	tab[pos.class, pos.class] / sum(tab[pos.class,])
}

Specificity <- function(obs, pred, pos.class)
{
	tab <- table(obs, pred)
	neg.class <- which(row.names(tab) != pos.class)

	tab[neg.class, neg.class] / sum(tab[neg.class,])
}

brier.score <- function(obsMat, predMat)
{
	sum((obsMat - predMat) ^ 2) / nrow(predMat)
}


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

observed <- test$norm_poraba
library(nnet)
obsMat <- class.ind(observed)


# odlocitveno drevo 
library(CORElearn)
dt <- CoreModel(norm_poraba ~ ., data = train, model="tree")
predicted <- predict(dt, test, type="class")
CA(observed, predicted)
predMat <- predict(dt, test, type = "prob")
brier.score(obsMat, predMat)
inf.score(train$norm_poraba, test$norm_poraba, predMat)
#### RES:
# > CA(observed, predicted)
# [1] 0.8339835
# > brier.score(obsMat, predMat)
# [1] 0.2520723
# > inf.score(train$norm_poraba, test$norm_poraba, predMat)
#  SREDNJA 
# 1.639806


# naivni bayes
library(CORElearn)
nb <- CoreModel(norm_poraba ~ . , data = train, model="bayes")
predicted <- predict(nb, test, type="class")
CA(observed, predicted)
predMat <- predict(nb, test, type = "prob")
brier.score(obsMat, predMat)
inf.score(train$norm_poraba, test$norm_poraba, predMat)
#### RES:
# > CA(observed, predicted)
# [1] 0.6172867
# > brier.score(obsMat, predMat)
# [1] 0.4964085
# > inf.score(train$norm_poraba, test$norm_poraba, predMat)
#  SREDNJA 
# 1.017203


## k-najbljizjih sosedov (SLOW)
#library(CORElearn)
#knn <- CoreModel(norm_poraba ~ ., data = train, model="knn", kInNN = 5)
#predicted <- predict(knn, test, type="class")
#CA(observed, predicted)
#
#predMat <- predict(knn, test, type = "prob")
#brier.score(obsMat, predMat)
#inf.score(train$norm_poraba, test$norm_poraba, predMat)
#### RES:
# > CA(observed, predicted)
# [1] 0.5949932
# > brier.score(obsMat, predMat)
# [1] 0.5495777
# > inf.score(train$norm_poraba, test$norm_poraba, predMat)
#   SREDNJA 
# 0.9575382

# nevronska mreza
library(nnet)

#scale
summary(train)
# vrstice, ki se ne skalirajo
class <- which(
    names(train) == "regija" |
    names(train) == "namembnost" |
    names(train) == "norm_poraba" |
    names(train) == "vikend" |
    names(train) == "stavba" |
    names(train) == "letni_cas")
max_train <- apply(train[,-class], 2, max)
min_train <- apply(train[,-class], 2, min)
train_scaled <- scale(train[,-class], center = min_train, scale = max_train - min_train)
train_scaled <- data.frame(train_scaled)
train_scaled$regija <- train$regija
train_scaled$namembnost <- train$namembnost
train_scaled$norm_poraba <- train$norm_poraba
train_scaled$vikend <- train$vikend
train_scaled$letni_cas <- train$letni_cas
test_scaled <- scale(test[,-class], center = min_train, scale = max_train - min_train)
test_scaled <- data.frame(test_scaled)
test_scaled$regija <- test$regija
test_scaled$namembnost <- test$namembnost
test_scaled$norm_poraba <- test$norm_poraba
test_scaled$vikend <- test$vikend
test_scaled$letni_cas <- test$letni_cas

#set.seed(0)
#nn <- nnet(norm_poraba ~ ., data = train_scaled, size = 4, decay = 0.0001, maxit = 10000)
#predicted <- predict(nn, test_scaled, type = "class")
#CA(observed, predicted)
#predMat <- predict(nn, test_scaled, type = "raw")
#brier.score(obsMat, predMat)
#inf.score(train$norm_poraba, test$norm_poraba, predMat)
#### BREZ SKALIRANJA, vse - stavba, size=10:
# > CA(observed, predicted)
# [1] 0.7479367
# > brier.score(obsMat, predMat)
# [1] 0.3724259
# > inf.score(train$norm_poraba, test$norm_poraba, predMat)
# [1] 1.314596
#### PO SKALIRANJU PODATKOV, vse - stavba, size=10 (manjsa izboljsava):
# > CA(observed, predicted)
# [1] 0.7859307
# > brier.score(obsMat, predMat)
# [1] 0.3260516
# > inf.score(train$norm_poraba, test$norm_poraba, predMat)
# [1] 1.418725
#### PO SKALIRANJU PODATKOV, vse, size=4, ZELO ZAMUDNO:
# > CA(observed, predicted)
# [1] 0.7856406
# > brier.score(obsMat, predMat)
# [1] 0.3110587
# > inf.score(train$norm_poraba, test$norm_poraba, predMat)
# [1] 1.45724

# PRIMERJAVA ATRIBUTOV
source("wrapper.r")

# ovrednotenje atributov za odlocitveno drevo
trainfn_tree <- function(formula, traindata){
    CoreModel(formula, data = traindata, model="tree")
}
trainfn_bayes <- function(formula, traindata) {
    CoreModel(formula, data = traindata, model="bayes")
}
predictfn <- function(model, testdata){
	predict(model, testdata, type="class")
}
evalfn <- function(predicted, observed, trained){
	1.0 - CA(observed, predicted)	
}

set.seed(0)
#wrapper(norm_poraba ~ ., train, trainfn_tree, predictfn, evalfn, cvfolds=10)
#### RES:
#selected attribute:  stavba
#selected attribute:  ma7
#selected attribute:  vikend
#[1]    28131 killed     R
# zmanjsamo cvfolds na 2, da ne zmanjka resourcev:
#wrapper(norm_poraba ~ ., train, trainfn_tree, predictfn, evalfn, cvfolds=2)
#### RES:
#selected attribute:  stavba
#selected attribute:  ma7 
#selected attribute:  vikend
#selected attribute:  ura
#selected attribute:  temp_zraka
#selected attribute:  povrsina
#selected attribute:  leto_izgradnje
#selected attribute:  temp_rosisca
#selected attribute:  regija
#selected attribute:  oblacnost
#selected attribute:  padavine
#selected attribute:  hitrost_vetra
#selected attribute:  namembnost
#selected attribute:  letni_cas
#selected attribute:  pritisk
#selected attribute:  smer_vetra 
#best model: estimated error =  0.1741497 , selected feature subset =  norm_poraba ~ stavba + ma7 + vikend + ura + temp_zraka + povrsina + leto_izgradnje + temp_rosisca
dt <- CoreModel(norm_poraba ~ stavba + ma7 + vikend + ura + temp_zraka + povrsina + leto_izgradnje + temp_rosisca, data = train, model="tree")
predicted <- predict(dt, test, type="class")
CA(observed, predicted)
predMat <- predict(dt, test, type = "prob")
brier.score(obsMat, predMat)
inf.score(train$norm_poraba, test$norm_poraba, predMat)
#### RES:
# > CA(observed, predicted)
# [1] 0.8342092
# > brier.score(obsMat, predMat)
# [1] 0.2512269
# > inf.score(train$norm_poraba, test$norm_poraba, predMat)
#  SREDNJA 
# 1.636165

#wrapper(norm_poraba ~ ., train, trainfn_bayes, predictfn, evalfn, cvfolds=10)
#### RES:
#selected attribute:  stavba 
#selected attribute:  ura 
#selected attribute:  vikend 
#selected attribute:  letni_cas 
#selected attribute:  smer_vetra 
#selected attribute:  padavine 
#selected attribute:  hitrost_vetra 
#selected attribute:  oblacnost 
#selected attribute:  pritisk 
#selected attribute:  temp_zraka 
#selected attribute:  temp_rosisca 
#selected attribute:  namembnost 
#selected attribute:  regija 
#selected attribute:  ma7 
#selected attribute:  povrsina 
#selected attribute:  leto_izgradnje 
#best model: estimated error =  0.3318365 , selected feature subset =  norm_poraba ~ stavba + ura + vikend + letni_cas + smer_vetra + padavine
nb <- trainfn_bayes(norm_poraba ~ stavba + ura + vikend + letni_cas + smer_vetra + padavine, train)
predicted <- predict(nb, test, type="class")
CA(observed, predicted)
predMat <- predict(nb, test, type = "prob")
brier.score(obsMat, predMat)
inf.score(train$norm_poraba, test$norm_poraba, predMat)
#### RES:
# > CA(observed, predicted)
# [1] 0.6681443
# > brier.score(obsMat, predMat)
# [1] 0.4982034
# > inf.score(train$norm_poraba, test$norm_poraba, predMat)
#   SREDNJA 
# 0.8700488

# se z optimizacijo brier-ja
predictfnprob <- function(model, testdata) {
	predict(model, testdata, type="prob")
}
evalbrier <- function(predicted, observed, trained) {
	obsMat <- model.matrix(~observed-1)
	sum((obsMat - predicted) ^ 2) / nrow(predicted)	
}
#wrapper(norm_poraba ~ ., train, trainfn_bayes, predictfnprob, evalbrier, cvfolds=10)
#### RES:
#selected attribute:  stavba
#selected attribute:  ma7
#selected attribute:  ura
#selected attribute:  vikend
#selected attribute:  povrsina
#selected attribute:  temp_rosisca
#selected attribute:  temp_zraka
#selected attribute:  oblacnost
#selected attribute:  namembnost
#selected attribute:  letni_cas
#selected attribute:  smer_vetra
#selected attribute:  hitrost_vetra 
#selected attribute:  padavine 
#selected attribute:  pritisk 
#selected attribute:  regija 
#selected attribute:  leto_izgradnje 
#best model: estimated error =  0.4820053 , selected feature subset =  norm_poraba ~ stavba + ma7 + ura + vikend + povrsina + temp_rosisca + temp_zraka + oblacnost + namembnost + letni_cas + smer_vetra
nb <- trainfn_bayes(norm_poraba ~ stavba + ma7 + ura + vikend + povrsina + temp_rosisca + temp_zraka + oblacnost + namembnost + letni_cas + smer_vetra)
predicted <- predict(nb, test, type="class")
CA(observed, predicted)
predMat <- predict(nb, test, type = "prob")
brier.score(obsMat, predMat)
inf.score(train$norm_poraba, test$norm_poraba, predMat)
#### RES:
# > CA(observed, predicted)
# [1] 0.6681443
# > brier.score(obsMat, predMat)
# [1] 0.4982034
# > inf.score(train$norm_poraba, test$norm_poraba, predMat)
#   SREDNJA 
# 0.8700488

# CA: norm_poraba ~ stavba + ura + vikend + letni_cas + smer_vetra + padavine, train
# brier: norm_poraba ~ stavba + ma7 + ura + vikend + povrsina + temp_rosisca + temp_zraka + oblacnost + namembnost + letni_cas + smer_vetra
# rocno izbrano iz rezultatov: norm_poraba ~ stavba + ma7 + ura +vikend + povrsina + letni_cas + temp_zraka
###### TEST ROCNE IZBIRE ATRIBUTOV
# odlocitveno drevo 
dt <- CoreModel(norm_poraba ~ stavba + ma7 + ura +vikend + povrsina + letni_cas + temp_zraka, data = train, model="tree")
predicted <- predict(dt, test, type="class")
CA(observed, predicted)
predMat <- predict(dt, test, type = "prob")
brier.score(obsMat, predMat)
inf.score(train$norm_poraba, test$norm_poraba, predMat)
#### RES:
# > CA(observed, predicted)
# [1] 0.833919
# > brier.score(obsMat, predMat)
# [1] 0.2507866
# > inf.score(train$norm_poraba, test$norm_poraba, predMat)
#  SREDNJA 
# 1.637371


# naivni bayes
nb <- CoreModel(norm_poraba ~ stavba + ma7 + ura +vikend + povrsina + letni_cas + temp_zraka, data = train, model="bayes")
predicted <- predict(nb, test, type="class")
CA(observed, predicted)
predMat <- predict(nb, test, type = "prob")
brier.score(obsMat, predMat)
inf.score(train$norm_poraba, test$norm_poraba, predMat)
#### RES:
# > CA(observed, predicted)
# [1] 0.6371301
# > brier.score(obsMat, predMat)
# [1] 0.4825814
# > inf.score(train$norm_poraba, test$norm_poraba, predMat)
#   SREDNJA 
# 0.9837011



# UTEZENO GLASOVANJE

voting_train <- function(traindata) {
    class <- which(
    names(traindata) == "regija" |
    names(traindata) == "namembnost" |
    names(traindata) == "norm_poraba" |
    names(traindata) == "vikend" |
    names(traindata) == "stavba" |
    names(traindata) == "letni_cas")
    max_train <- apply(traindata[,-class], 2, max)
    min_train <- apply(traindata[,-class], 2, min)
    train_scaled <- scale(traindata[,-class], center = min_train, scale = max_train - min_train)
    train_scaled <- data.frame(train_scaled)
    train_scaled$regija <- traindata$regija
    train_scaled$namembnost <- traindata$namembnost
    train_scaled$norm_poraba <- traindata$norm_poraba
    train_scaled$vikend <- traindata$vikend
    train_scaled$letni_cas <- traindata$letni_cas

    model.nn <- nnet(norm_poraba ~ ., data = train_scaled, size = 10, decay = 0.0001, maxit = 10000)
    model.dt <- CoreModel(norm_poraba ~ stavba + ma7 + vikend + ura + temp_zraka + povrsina + leto_izgradnje + temp_rosisca, data = traindata, model="tree")
    model.nb <- CoreModel(norm_poraba ~ stavba + ura + vikend + letni_cas + smer_vetra + padavine, data = traindata, model="bayes")

    list(model.nn, model.dt, model.nb, max_train, min_train, class)
}

voting_predict <- function(model, testdata) {
    model.nn <- model[[1]]
    model.dt <- model[[2]]
    model.nb <- model[[3]]
    max_train <- model[[4]]
    min_train <- model[[5]]
    class <- model[[6]]

    test_scaled <- scale(testdata[,-class], center = min_train, scale = max_train - min_train)
    test_scaled <- data.frame(test_scaled)
    test_scaled$regija <- testdata$regija
    test_scaled$namembnost <- testdata$namembnost
    test_scaled$norm_poraba <- testdata$norm_poraba
    test_scaled$vikend <- testdata$vikend
    test_scaled$letni_cas <- testdata$letni_cas

    prob.nn <- predict(model.nn, test_scaled, type="raw")
    prob.dt <- predict(model.dt, testdata, type="prob")
    prob.nb <- predict(model.nb, testdata, type="prob")

    prob.sum <- prob.nn + prob.dt + prob.nb

    predClass <- colnames(prob.sum)[max.col(prob.sum)]
    predicted <- factor(predClass, levels(testdata$norm_poraba))

    predicted
}

voting_prob <- function(model, testdata) {
    model.nn <- model[[1]]
    model.dt <- model[[2]]
    model.nb <- model[[3]]
    max_train <- model[[4]]
    min_train <- model[[5]]
    class <- model[[6]]

    test_scaled <- scale(testdata[,-class], center = min_train, scale = max_train - min_train)
    test_scaled <- data.frame(test_scaled)
    test_scaled$regija <- testdata$regija
    test_scaled$namembnost <- testdata$namembnost
    test_scaled$norm_poraba <- testdata$norm_poraba
    test_scaled$vikend <- testdata$vikend
    test_scaled$letni_cas <- testdata$letni_cas

    prob.nn <- predict(model.nn, test_scaled, type="raw")
    prob.dt <- predict(model.dt, testdata, type="prob")
    prob.nb <- predict(model.nb, testdata, type="prob")

    prob.sum <- prob.nn + prob.dt + prob.nb

    prob.sum/3
}

#model <- voting_train(train)
#predicted <- voting_predict(model, test)
#CA(observed, predicted)
#predMat <- voting_prob(model, test)
#brier.score(obsMat, predMat)
#inf.score(train$norm_poraba, test$norm_poraba, predMat)
#### RES:
# > CA(observed, predicted)
# [1] 0.8344348
# > brier.score(obsMat, predMat)
# [1] 0.2745228
# > inf.score(train$norm_poraba, test$norm_poraba, predMat)
# [1] 1.403834

# samo odlocitveno drevo (benchmark, ker je najboljsi):
# > CA(observed, predicted)
# [1] 0.8342092
# > brier.score(obsMat, predMat)
# [1] 0.2512269
# > inf.score(train$norm_poraba, test$norm_poraba, predMat)
#  SREDNJA 
# 1.636165

#### CONCLUSION: v tem primeru kombiniranje ne pripomore k boljsemu rezultatu (brier in inf sta celo slabsa pri kombiniranju)


# GLASOVANJE 

voting_simple_predict <- function(model, testdata) {
    model.nn <- model[[1]]
    model.dt <- model[[2]]
    model.nb <- model[[3]]
    max_train <- model[[4]]
    min_train <- model[[5]]
    class <- model[[6]]

    test_scaled <- scale(testdata[,-class], center = min_train, scale = max_train - min_train)
    test_scaled <- data.frame(test_scaled)
    test_scaled$regija <- testdata$regija
    test_scaled$namembnost <- testdata$namembnost
    test_scaled$norm_poraba <- testdata$norm_poraba
    test_scaled$vikend <- testdata$vikend
    test_scaled$letni_cas <- testdata$letni_cas

    pred.nn <- as.factor(predict(model.nn, test_scaled, type="class"))
    pred.dt <- predict(model.dt, testdata, type="class")
    pred.nb <- predict(model.nb, testdata, type="class")

    predictions <- data.frame(pred.nn, pred.dt, pred.nb)

    predClass <- vector()
  	for (i in 1:nrow(predictions)) {
		vec <- unlist(predictions[i,])
    	predClass[i] <- names(which.max(table(vec)))
  	}

    predicted <- factor(predClass, levels=levels(testdata$norm_poraba))
    predicted
}

predicted <- voting_simple_predict(model, test)
CA(observed, predicted)
#### RES: 0.8192824

#### CONCLUSION: ponovno ni boljsi od benchmark-a (odlocitveno drevo)



# EVALVACIJA MODELOV
# odlocitveno drevo
for (i in 1:11) {
    train <- df[df$datum < as.Date(paste("2016", i+1, "1", sep="-")),]
    sel <- df$datum > as.Date(paste("2016", i+1, "1", sep="-"))-1
    if (i < 11) {
        sel <- sel & df$datum < as.Date(paste("2016", i+2, "1", sep="-"))
    }
    test <- df[sel,]

    observed <- test$norm_poraba
    obsMat <- class.ind(observed)

    dt <- CoreModel(norm_poraba ~ stavba + ma7 + vikend + ura + temp_zraka + povrsina + leto_izgradnje + temp_rosisca, data = train, model="tree")
    predicted <- predict(dt, test, type="class")
    predMat <- predict(dt, test, type = "prob")
    print(paste("Mesec", i, "CA", CA(observed, predicted), "brier", brier.score(obsMat, predMat), "inf.score", inf.score(train$norm_poraba, test$norm_poraba, predMat)))
	flush.console()
}
# "Mesec 1 CA 0.825469094922737 brier 0.27195202983834 inf.score 1.58926760921557"
# "Mesec 2 CA 0.821079379108435 brier 0.28670822029553 inf.score 1.59215566759593"
# "Mesec 3 CA 0.817818157163198 brier 0.289125928669687 inf.score 1.57420071727937"
# "Mesec 4 CA 0.759457092819615 brier 0.380752705701434 inf.score 1.50039302858179"
# "Mesec 5 CA 0.759082946149745 brier 0.390147890972171 inf.score 1.48607316873312"
# "Mesec 6 CA 0.794813732651571 brier 0.316937035369174 inf.score 1.59568908688928"
# "Mesec 7 CA 0.785833440266906 brier 0.333063845437263 inf.score 1.58314856602651"
# "Mesec 8 CA 0.803218928399477 brier 0.300931962812643 inf.score 1.60064674947118"
# "Mesec 9 CA 0.786028835133016 brier 0.336415614748635 inf.score 1.51939133600808"
# "Mesec 10 CA 0.804927742241175 brier 0.307235597688323 inf.score 1.55239803026943"
# "Mesec 11 CA 0.793251205141939 brier 0.311447151311095 inf.score 1.56166241636167"
res.ca <- c(0.825469094922737, 0.821079379108435, 0.817818157163198, 0.759457092819615, 0.759082946149745, 0.794813732651571, 0.785833440266906, 0.803218928399477, 0.786028835133016, 0.804927742241175, 0.793251205141939)
res.brier <- c(0.27195202983834, 0.28670822029553, 0.289125928669687, 0.380752705701434, 0.390147890972171, 0.316937035369174, 0.333063845437263, 0.300931962812643, 0.336415614748635, 0.307235597688323, 0.311447151311095)
res.inf <- c(1.58926760921557, 1.59215566759593, 1.57420071727937, 1.50039302858179, 1.48607316873312, 1.59568908688928, 1.58314856602651, 1.60064674947118, 1.51939133600808, 1.55239803026943, 1.56166241636167)
png("graphs/meseci_drevo.png")
plt_data <- matrix(c(res.ca, res.brier, res.inf), ncol=3)
matplot(plt_data, type = c("b"),pch=1,col = 1:3)
legend("topleft", legend = c("CA", "birer", "inf"), col=1:4, pch=1) # optional legend
dev.off()

# bayes
for (i in 1:11) {
    train <- df[df$datum < as.Date(paste("2016", i+1, "1", sep="-")),]
    sel <- df$datum > as.Date(paste("2016", i+1, "1", sep="-"))-1
    if (i < 11) {
        sel <- sel & df$datum < as.Date(paste("2016", i+2, "1", sep="-"))
    }
    test <- df[sel,]

    observed <- test$norm_poraba
    obsMat <- class.ind(observed)

    nb <- CoreModel(norm_poraba ~ stavba + ura + vikend + letni_cas + smer_vetra + padavine, data = train, model="tree")
    predicted <- predict(nb, test, type="class")
    predMat <- predict(nb, test, type = "prob")
    print(paste("Mesec", i, "CA", CA(observed, predicted), "brier", brier.score(obsMat, predMat), "inf.score", inf.score(train$norm_poraba, test$norm_poraba, predMat)))
	flush.console()
}
# "Mesec 1 CA 0.82275570272259 brier 0.26821686381063 inf.score 1.56253055470729"
# "Mesec 2 CA 0.779870739656411 brier 0.326119544011102 inf.score 1.48884530565664"
# "Mesec 3 CA 0.754220625127127 brier 0.366087831550607 inf.score 1.42546444293958"
# "Mesec 4 CA 0.699299474605954 brier 0.436342740022153 inf.score 1.3692811741531"
# "Mesec 5 CA 0.584431411379389 brier 0.594301149056359 inf.score 1.12524833477131"
# "Mesec 6 CA 0.75427319211103 brier 0.36635902777329 inf.score 1.49836768985324"
# "Mesec 7 CA 0.743102784550237 brier 0.39863117252754 inf.score 1.45922960304855"
# "Mesec 8 CA 0.722195474241695 brier 0.399978456236395 inf.score 1.40097454421941"
# "Mesec 9 CA 0.655426192917898 brier 0.508374515896721 inf.score 1.24563073044299"
# "Mesec 10 CA 0.706941483060886 brier 0.414130092084484 inf.score 1.32590007214014"
# "Mesec 11 CA 0.689921442599536 brier 0.452266087904988 inf.score 1.26793149105131"
res.ca <- c(0.82275570272259, 0.779870739656411, 0.754220625127127, 0.699299474605954, 0.584431411379389, 0.75427319211103, 0.743102784550237, 0.722195474241695, 0.655426192917898, 0.706941483060886, 0.689921442599536)
res.brier <- c(0.26821686381063, 0.326119544011102, 0.366087831550607, 0.436342740022153, 0.594301149056359, 0.36635902777329, 0.39863117252754, 0.399978456236395, 0.508374515896721, 0.414130092084484, 0.452266087904988)
res.inf <- c(1.56253055470729, 1.48884530565664, 1.42546444293958, 1.3692811741531, 1.12524833477131, 1.49836768985324, 1.45922960304855, 1.40097454421941, 1.24563073044299, 1.32590007214014, 1.26793149105131)
png("graphs/meseci_bayes.png")
plt_data <- matrix(c(res.ca, res.brier, res.inf), ncol=3)
matplot(plt_data, type = c("b"),pch=1,col = 1:3)
legend("topleft", legend = c("CA", "birer", "inf"), col=1:4, pch=1) # optional legend
dev.off()


# knn
for (i in 1:11) {
    train <- df[df$datum < as.Date(paste("2016", i+1, "1", sep="-")),]
    sel <- df$datum > as.Date(paste("2016", i+1, "1", sep="-"))-1
    if (i < 11) {
        sel <- sel & df$datum < as.Date(paste("2016", i+2, "1", sep="-"))
    }
    test <- df[sel,]

    observed <- test$norm_poraba
    obsMat <- class.ind(observed)

    knn <- CoreModel(norm_poraba ~ ., data = train, model="knn", kInNN = 5)
    predicted <- predict(knn, test, type="class")
    predMat <- predict(knn, test, type = "prob")
    print(paste("Mesec", i, "CA", CA(observed, predicted), "brier", brier.score(obsMat, predMat), "inf.score", inf.score(train$norm_poraba, test$norm_poraba, predMat)))
	flush.console()
}
# "Mesec 1 CA 0.743147534952171 brier 0.361747608535688 inf.score 1.36959612904857"
# "Mesec 2 CA 0.765674197646799 brier 0.351513008893554 inf.score 1.42775732841852"
# "Mesec 3 CA 0.744931859787104 brier 0.379556580107126 inf.score 1.39332283136114"
# "Mesec 4 CA 0.706217162872154 brier 0.438185639229422 inf.score 1.35562692413412"
# "Mesec 5 CA 0.672404600502704 brier 0.47277934343819 inf.score 1.30996768267703"
# "Mesec 6 CA 0.746749452154858 brier 0.357516435354273 inf.score 1.47320766403723"
# "Mesec 7 CA 0.730014115231618 brier 0.402581804183241 inf.score 1.44757004461627"
# "Mesec 8 CA 0.734644748607194 brier 0.380477336818213 inf.score 1.4223689827606"
# "Mesec 9 CA 0.706219460698558 brier 0.417962236834168 inf.score 1.30545753192597"
# "Mesec 10 CA 0.723525230987918 brier 0.399382136934376 inf.score 1.33845776026451"
# "Mesec 11 CA 0.699473308337797 brier 0.442338868059275 inf.score 1.31122917399681"
res.ca <- c(0.743147534952171, 0.765674197646799, 0.744931859787104, 0.706217162872154, 0.672404600502704, 0.746749452154858, 0.730014115231618, 0.734644748607194, 0.706219460698558, 0.723525230987918, 0.699473308337797)
res.brier <- c(0.361747608535688, 0.351513008893554, 0.379556580107126, 0.438185639229422, 0.47277934343819, 0.357516435354273, 0.402581804183241, 0.380477336818213, 0.417962236834168, 0.399382136934376, 0.442338868059275)
res.inf <- c(1.36959612904857, 1.42775732841852, 1.39332283136114, 1.35562692413412, 1.30996768267703, 1.47320766403723, 1.44757004461627, 1.4223689827606, 1.30545753192597, 1.33845776026451, 1.31122917399681)
png("graphs/meseci_knn.png")
plt_data <- matrix(c(res.ca, res.brier, res.inf), ncol=3)
matplot(plt_data, type = c("b"),pch=1,col = 1:3)
legend("topleft", legend = c("CA", "birer", "inf"), col=1:4, pch=1) # optional legend
dev.off()



# REGIJA
set.seed(0)
sel <- sample(1:nrow(df), as.integer(nrow(df) * 0.7), F)
train <- df[sel,]
test <- df[-sel,]
train$poraba <- NULL
train$datum <- NULL
test$poraba <- NULL
test$datum <- NULL
observed <- test$norm_poraba
obsMat <- class.ind(observed)

# za ucno mnozico regije se vzame iz skupne ucne, testna je ista
train_vzhod <- train[train$regija == "vzhodna",]
train_zahod <- train[train$regija == "zahodna",]


observed <- test$norm_poraba
obsMat <- class.ind(observed)

dt_general <- CoreModel(norm_poraba ~ stavba + ma7 + vikend + ura + temp_zraka + povrsina + leto_izgradnje + temp_rosisca, data = train, model="tree")
dt_vzhod <- CoreModel(norm_poraba ~ stavba + ma7 + vikend + ura + temp_zraka + povrsina + leto_izgradnje + temp_rosisca, data = train_vzhod, model="tree")
dt_zahod <- CoreModel(norm_poraba ~ stavba + ma7 + vikend + ura + temp_zraka + povrsina + leto_izgradnje + temp_rosisca, data = train_zahod, model="tree")
predicted_general <- predict(dt_general, test, type="class")
predicted_vzhod <- predict(dt_vzhod, test, type="class")
predicted_zahod <- predict(dt_zahod, test, type="class")
print(paste(
    "CA general", CA(observed, predicted_general),
    "CA vzhod", CA(observed, predicted_vzhod),
    "CA zahod", CA(observed, predicted_zahod)))
flush.console()
#### RES: CA general 0.836127409891031 CA vzhod 0.514733380617706 CA zahod 0.60609968405442


nb_general <- CoreModel(norm_poraba ~ stavba + ura + vikend + letni_cas + smer_vetra + padavine, data = train, model="tree")
nb_vzhod <- CoreModel(norm_poraba ~ stavba + ura + vikend + letni_cas + smer_vetra + padavine, data = train_vzhod, model="tree")
nb_zahod <- CoreModel(norm_poraba ~ stavba + ura + vikend + letni_cas + smer_vetra + padavine, data = train_zahod, model="tree")
predicted_general <- predict(nb_general, test, type="class")
predicted_vzhod <- predict(nb_vzhod, test, type="class")
predicted_zahod <- predict(nb_zahod, test, type="class")
print(paste(
    "CA general", CA(observed, predicted_general),
    "CA vzhod", CA(observed, predicted_vzhod),
    "CA zahod", CA(observed, predicted_zahod)))
flush.console()
#### RES: [1] CA general 0.78252949900058 CA vzhod 0.474369720807273 CA zahod 0.572570765362048

