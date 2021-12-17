
set.seed(0)


CA <- function(observed, predicted)
{
  mean(observed == predicted)
}
brier.score <- function(observedMatrix, predictedMatrix)
{
  sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix)
}


#izbrani - povp_prejsni_dan + povrsina + namembnost + leto_izgradnje + temp_zraka + temp_rosisca



#v primeru testiranja na 70% modelov v učni množici, 30% v testni
sel <- sample(1:nrow(data), size=as.integer(nrow(data) * 0.7), replace=F)
train <- data[sel,]
test <- data[-sel,]

#odločitveno drevo
library(rpart) 
dt <- rpart(norm_poraba ~ ., data = train)
observed <- test$norm_poraba
predicted <- predict(dt, test, type="class")
CA(observed, predicted)
predMat <- predict(dt, test, type = "prob")
brier.score(obsMat, predMat)

#rezanje drevesa
dt <- rpart(norm_poraba ~ ., data=train, cp=0)
tab <- printcp(dt)
row <- which.min(tab[,"xerror"])
th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))
dt <- prune(dt, cp=th)

predicted <- predict(dt, test, type="class")
CA(observed, predicted)
predMat <- predict(dt, test, type = "prob")
brier.score(obsMat, predMat)

#KNN
obsMat <- class.ind(test$norm_poraba)

library(CORElearn)
knn <- CoreModel(norm_poraba ~  ., data = train, model="knn", kInNN = 2)
predicted <- predict(knn, test, type="class")
print(CA(observed, predicted))

predMat <- predict(knn, test, type = "prob")
print(brier.score(obsMat, predMat))



# NAKLJUCNI GOZD

library(randomForest)

rf <- randomForest(norm_poraba ~ ., data = train)
predicted <- predict(rf, test, type="class")
CA(observed, predicted)
predMat <- predict(rf, test, type = "prob")
brier.score(obsMat, predMat)

# bayes
library(e1071)
library(nnet)
obsMat <- class.ind(test$norm_poraba)
observed <- test$norm_poraba
nb <- naiveBayes(norm_poraba ~ ., data = train)
predicted <- predict(nb, test, type="class")
CA(observed, predicted)
predMat <- predict(nb, test, type = "raw")
brier.score(obsMat, predMat)


# evalvacija
library(lubridate)
for (j in 1:11) {
  print(paste("Iteracija", j))
  train <- data[month(data$datum) %in% seq(1,j),]
  test <- data[month(data$datum) %in% seq(j+1,12),]
  
  train$poraba <- NULL
  test$poraba <- NULL
  
  # koda za vsak model
  
}

#########################################################################################################################################################
source("wrapper.R")
# wrapper za rpart
library(rpart)


modelFull <- rpart(norm_poraba ~ ., train)
predicted <- predict(modelFull, test, type="class")
mean(test$norm_poraba == predicted)

myTrainFunc <- function(formula, traindata)
{
  rpart(formula, traindata)	
}

myPredictFunc <- function(model, testdata)
{
  predict(model, testdata, type="class")
}

myEvalFunc <- function(predicted, observed, trained)
{
  1.0 - mean(observed == predicted)	
}

set.seed(0)
wrapper(norm_poraba ~ ., train, myTrainFunc, myPredictFunc, myEvalFunc, cvfolds=10)
# best model: estimated error =  0.3983168 , selected feature subset =  
# norm_poraba ~ 
# povp_prejsni_dan + povrsina + namembnost + ura + regija + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost 




#########################################################################################################################################################



