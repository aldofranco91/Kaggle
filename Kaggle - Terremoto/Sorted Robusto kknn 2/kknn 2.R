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

MAE_summary = function(data, lev = NULL, model = NULL) {
  out = Metrics::mae(actual = data[, "obs"],
                     predicted = data[, "pred"])
  names(out) = c("MAE")
  out
}

caret::modelLookup("kknn")
caret::getModelInfo("kknn")$kknn$grid

Grid = expand.grid(kmax = seq(5,100,5),
                   distance = seq(1,4,1),
                   kernel = c("rectangular", "triangular", "epanechnikov", "biweight",
                              "triweight","cos", "inv", "gaussian"))

TrainingParameters = trainControl(method = "cv",
                                  number = 5,
                                  verboseIter = TRUE,
                                  summaryFunction = MAE_summary,
                                  search = "grid",
                                  preProc = c("center", "scale"))

Modelo = train(V91 ~ ., data = data,
               method = "kknn",
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


