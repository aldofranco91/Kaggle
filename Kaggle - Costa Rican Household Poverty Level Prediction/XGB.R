rm(list=ls())

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(Metrics))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(caret))

F1_Score_summary = function(data, lev = NULL, model = NULL) {
  out = Metrics::f1(actual = data[, "obs"],
                    predicted = data[, "pred"])
  names(out) = c("F1_Score")
  out
}

Train = read.csv("../input/train.csv", stringsAsFactors = T)
Test = read.csv("../input/test.csv", stringsAsFactors = T)

Train = Train %>% select(-Id)

NA_in_Train = colnames(Train)[colSums(is.na(Train)) > 0]
NA_in_Test = colnames(Test)[colSums(is.na(Test)) > 0]

nzv = nearZeroVar(Train, saveMetrics = TRUE)
dropList_nearZeroVar = rownames(nzv[nzv$nzv == "TRUE",])

variables_drop = c(dropList_nearZeroVar,NA_in_Train,NA_in_Test)
variables_drop = unique(variables_drop)

Train = Train %>% select(-one_of(variables_drop))

Test = Test %>% select(-one_of(variables_drop))

Train$Target = as.factor(Train$Target)

Train = Train %>% select(-idhogar,-dependency)
Test = Test %>% select(-idhogar,-dependency)
  
#Grid = expand.grid(nrounds = seq(10, length.out = 10, by = 50),
#                   max_depth = c(2,4,6),
#                   eta = seq(0.1, length.out = 6, by = 0.1),
#                   gamma = seq(0.1, length.out = 6, by = 0.1),
#                   colsample_bytree = .7,
#                   min_child_weight = 1,
#                   subsample = 0.8)

TrainingParameters = trainControl(method = "cv",
                                  number = 10,
                                  p = 0.6,
                                  verboseIter = TRUE,
                                  search = "grid",
                                  summaryFunction = F1_Score_summary,
                                  savePredictions = TRUE)

Modelo = train(Target ~ ., data = Train,
               method = "xgbTree",
               trControl = TrainingParameters,
               tuneLenght = 40,
               na.action = na.omit,
               verboseIter = TRUE,
               metric = "F1_Score",
               maximize = TRUE)

Modelo

Forecast = predict(object = Modelo, newdata = Test)

Submission = cbind(Test$Id, Forecast)

colnames(Submission) = c("Id","Target")

write.csv(Submission, file = "Submission.csv",row.names=FALSE)




























