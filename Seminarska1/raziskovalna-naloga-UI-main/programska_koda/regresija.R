set.seed(0)

mae <- function(obs, pred)
{
  mean(abs(obs - pred))
}
mse <- function(obs, pred)
{
  mean((obs - pred)^2)
}
rmae <- function(obs, pred, mean.val) 
{  
  sum(abs(obs - pred)) / sum(abs(obs - mean.val))
}
rmse <- function(obs, pred, mean.val) 
{  
  sum((obs - pred)^2)/sum((obs - mean.val)^2)
}

# izbrani -  povrsina + leto_izgradnje + namembnost + povp_prejsni_dan 



#LINEARNA
model <- lm(poraba ~ ., train)
predicted <- predict(model, test)
observed <- test$poraba
mae(observed, predicted)
mse(observed, predicted)
meanVal <- mean(train$poraba)
predTrivial <- rep(meanVal, nrow(test))
mae(observed, predTrivial)
mse(observed, predTrivial)
rmae(observed, predicted, mean(train$poraba))
rmse(observed, predicted, mean(train$poraba))


# regresijsko drevo
library(rpart)
library(rpart.plot)
rt.model <- rpart(poraba ~ ., data=train)
predicted <- predict(rt.model, test)
mae(test$poraba, predicted)
rmae(test$poraba, predicted, mean(train$poraba))
# rezanje drevesa
rt.model <- rpart(poraba ~ ., data=train, cp=0)
tab <- printcp(rt.model)
row <- which.min(tab[,"xerror"])
th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))
rt.model <- prune(rt.model, cp=th)
predicted <- predict(rt.model, test)
mae(test$poraba, predicted)
rmae(test$poraba, predicted, mean(train$poraba))


# nakljucni gozd
library(randomForest)
rf.model <- randomForest(poraba ~ ., train)
predicted <- predict(rf.model, test)
mae(test$poraba, predicted)
rmae(test$poraba, predicted, mean(train$poraba))


#
# k-najblizjih sosedov
#

# evalvacija

library(kknn)

knn.model <- kknn(poraba ~ ., train, test, k = 5)
predicted <- fitted(knn.model)
mae(test$poraba, predicted)
rmae(test$poraba, predicted, mean(train$poraba))

library(lubridate)
for (j in 1:11) {
  print(paste("Iteracija", j))
  train <- data[month(data$datum) %in% seq(1,j),]
  test <- data[month(data$datum) %in% seq(j+1,12),]
  
  train$norm_poraba <- NULL
  test$norm_poraba <- NULL
  
  #koda za vsak model
  
}





