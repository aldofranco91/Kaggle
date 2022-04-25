rm(list=ls())

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(telegram))
suppressPackageStartupMessages(library(Metrics))
library(lubridate)

send_telegram_message = function(text){ 
  bot <- TGBot$new(token = "606732762:AAHvtOmBt5AG6Gv3ED2ddxW2RgncI0zdmGY")
  bot$sendMessage(text = text, chat_id = "469455650") }

data = read.csv(file = "Terremotos.csv", header = FALSE)

nzv = nearZeroVar(data, saveMetrics = TRUE)
dropList_nearZeroVar = rownames(nzv[nzv$nzv == "TRUE",])

Grid = expand.grid(nrounds = seq(10,length.out = 10,by = 50),
                   max_depth = seq(2:10),
                   eta = seq(0.01:1,length.out = 5),
                   gamma = seq(0.01:1,length.out = 5),
                   colsample_bytree = seq(0.1:0,9,by = 0.3),
                   min_child_weight = 1,
                   subsample = 0.8)

MAE_summary = function(data, lev = NULL, model = NULL) {
  out = Metrics::mae(actual = data[, "obs"],
                     predicted = data[, "pred"])
  names(out) = c("MAE")
  out
}

TrainingParameters = trainControl(method = "cv",
                                  number = 5,
                                  p = 0.5,
                                  verboseIter = TRUE,
                                  summaryFunction = MAE_summary,
                                  search = "grid")


Modelo = train(V91 ~ ., data = data,
               method = "xgbTree",
               trControl = TrainingParameters,
               tuneGrid = Grid,
               # tuneLength = 15,
               na.action = na.omit,
               verboseIter = TRUE,
               metric = "MAE",
               maximize = FALSE)

Modelo

Test = read.csv(file = "TestTerremotos.csv", header = FALSE)

Forecast = predict(object = Modelo, newdata = Test)

Submission = read.csv(file = "sample_submission.csv", header = TRUE)

Submission$time_to_failure = Forecast

write.csv(Submission, file = paste("Submission",Modelo$method,".csv"),row.names=FALSE)

send_telegram_message("Script finalizado")

save.image(file = paste(Modelo$method,today(),"_.RData"))


