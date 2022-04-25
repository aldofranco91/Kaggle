rm(list=ls())

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(telegram))
suppressPackageStartupMessages(library(Metrics))

send_telegram_message = function(text){ 
  bot <- TGBot$new(token = "606732762:AAHvtOmBt5AG6Gv3ED2ddxW2RgncI0zdmGY")
  bot$sendMessage(text = text, chat_id = "469455650") }

RMSLE_summary = function(data, lev = NULL, model = NULL) {
  out = Metrics::rmsle(actual = data[, "obs"],
                    predicted = data[, "pred"])
  names(out) = c("RMSLE")
  out
}

Train = read.csv("train.csv", stringsAsFactors = T)
Test = read.csv("test.csv", stringsAsFactors = T)

Train = Train %>% select(-Id)

NA_in_Train = colnames(Train)[colSums(is.na(Train)) > 0]
NA_in_Test = colnames(Test)[colSums(is.na(Test)) > 0]

nzv = nearZeroVar(Train, saveMetrics = TRUE)
dropList_nearZeroVar = rownames(nzv[nzv$nzv == "TRUE",])

variables_drop = c(dropList_nearZeroVar,NA_in_Train,NA_in_Test)
variables_drop = unique(variables_drop)

Train = Train %>% select(-variables_drop)

Test = Test %>% select(-variables_drop)

# Grid = expand.grid(nrounds = seq(10,length.out = 20,by = 50),
#                    max_depth = seq(2:10),
#                    eta = seq(0.01:1,length.out = 10),
#                    gamma = seq(0.01:1,length.out = 10),
#                    colsample_bytree = seq(0.1:0,9,by = 0.1),
#                    min_child_weight = 1,
#                    subsample = 0.8)

TrainingParameters = trainControl(method = "cv",
                                  number = 3,
                                  p = 0.8,
                                  verboseIter = TRUE,
                                  summaryFunction = RMSLE_summary,
                                  search = "grid")

Modelo = train(SalePrice ~ ., data = Train,
               method = "xgbTree",
               trControl = TrainingParameters,
               na.action = na.omit,
               tuneLenght = 30,
               verboseIter = TRUE,
               metric = "RMSLE",
               maximize = FALSE)

Modelo

Forecast = predict(object = Modelo, newdata = Test)

Submission = cbind(Test$Id, Forecast)

colnames(Submission) = c("Id","SalePrice")

write.csv(Submission, file = "Submission.csv",row.names=FALSE)

send_telegram_message("Script finalizado")