rm(list=ls())

setwd("D:/!Machine Learning R and Python/Kaggle - Digit Recognizer")

library(caret)
library(telegram)
library(dplyr)

send_telegram_message = function(text){ 
  bot <- TGBot$new(token = "606732762:AAHvtOmBt5AG6Gv3ED2ddxW2RgncI0zdmGY")
  bot$sendMessage(text = text, chat_id = "469455650") }

Train = read.csv("train.csv")

nzv = nearZeroVar(Train, saveMetrics = T)

dropList = rownames(nzv[nzv$nzv == "TRUE",])

Train = Train %>% select(-dropList)

Train$label = as.factor(Train$label)
levels(Train$label)
str(Train$label)

modelLookup("svmRadial")
getModelInfo("svmRadial")$`svmRadial`$grid

Grid = expand.grid(sigma = c(0.01, 0.5), C =  c(1))

Control = trainControl(method = "cv", number = 5, search = "grid",
                       verboseIter = T)

Modelo = train(label ~ ., data = Train, method = "svmRadial",
               trControl = Control, tuneGrid = Grid, verbose = T,
               metric = "Accuracy", maximize = TRUE, na.action = "na.omit")

Modelo

send_telegram_message("Modelo acabado")

Predict_Train = predict(Modelo,Train[,-1])

confusionMatrix(data = Predict_Train,
                reference = Train$label)

Test = read.csv("test.csv")

Submission = read.csv("sample_submission.csv")

Submission[,2] = predict(Modelo,Test)
  
write.csv(Submission, file = "Submission_svm.csv",row.names=FALSE)

send_telegram_message("Script finalizado")
