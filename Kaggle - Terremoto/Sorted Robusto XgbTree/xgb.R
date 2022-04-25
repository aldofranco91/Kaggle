rm(list=ls())

library(caret)
library(dplyr)
library(telegram)
library(Metrics)
library(lubridate)

send_telegram_message = function(text){ 
  bot <- TGBot$new(token = "606732762:AAHvtOmBt5AG6Gv3ED2ddxW2RgncI0zdmGY")
  bot$sendMessage(text = text, chat_id = "469455650") }

data = read.csv(file = "SortedRobustTraining.csv", header = FALSE)

nzv = nearZeroVar(data, saveMetrics = TRUE)
dropList_nearZeroVar = rownames(nzv[nzv$nzv == "TRUE",])

Grid = expand.grid(nrounds = seq(10,length.out = 10,by = 50),
                   max_depth = seq(1:10),
                   eta = seq(0,1,length = 3), #range: [0,1]
                   gamma = seq(0.01:1,length.out = 5), #range: [0,∞]
                   colsample_bytree = seq(0.1,1,length = 5), #range: (0, 1]
                   min_child_weight = 1, #range: [0,∞]
                   subsample = 0.8) #range: (0,1]

MAE_summary = function(data, lev = NULL, model = NULL) {
  out = Metrics::mae(actual = data[, "obs"],
                     predicted = data[, "pred"])
  names(out) = c("MAE")
  out
}

TrainingParameters = trainControl(method = "cv",
                                  number = 5,
                                  verboseIter = TRUE,
                                  summaryFunction = MAE_summary,
                                  search = "grid")

Modelo = train(V91 ~ ., data = data,
               method = "xgbTree",
               trControl = TrainingParameters,
               tuneGrid = Grid,
               na.action = na.omit,
               verboseIter = TRUE,
               metric = "MAE",
               maximize = FALSE)

Modelos = Modelo$results

Test = read.csv(file = "SortedRobustTesting.csv", header = FALSE)

Forecast = predict(object = Modelo, newdata = Test)

summary(Forecast)

write.csv(Forecast, file = paste("Submission",Modelo$method,".csv"),row.names=FALSE)

send_telegram_message("Script finalizado")

save.image(file = paste(Modelo$method,today(),"_.RData"))


