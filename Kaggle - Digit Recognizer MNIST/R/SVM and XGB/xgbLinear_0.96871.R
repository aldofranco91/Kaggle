rm(list=ls())

library(caret)
library(MLmetrics)
library(telegram)

send_telegram_message = function(text){ 
  bot <- TGBot$new(token = "606732762:AAHvtOmBt5AG6Gv3ED2ddxW2RgncI0zdmGY")
  bot$sendMessage(text = text, chat_id = "469455650") }

Train = read.csv("train.csv")

Train$label = as.factor(Train$label)
levels(Train$label)
str(Train$label)

modelLookup("xgbLinear")
getModelInfo("xgbLinear")$`xgbLinear`$grid

Grid = expand.grid(lambda = c(0,0.1),
                   alpha =  c(0,0.1),
                   nrounds = c(5,20,50),
                   eta = 0.3)

Control = trainControl(method = "cv",
                       number = 2,
                       search = "grid",
                       verboseIter = T)

Modelo = train(label ~ .,
               data = Train,
               method = "xgbLinear",
               trControl = Control,
               tuneGrid = Grid,
               verbose = T,
               metric = "Accuracy",
               maximize = TRUE,
               na.action = "na.omit"
               
)

Modelo

send_telegram_message("Modelo acabado")

Predict_Train = predict(Modelo,Train[,-1])

confusionMatrix(data = Predict_Train,
                reference = Train$label)

Test = read.csv("test.csv")

Submission = read.csv("sample_submission.csv")

Submission[,2] = predict(Modelo,Test)
  
write.csv(Submission, file = "Submission.csv",row.names=FALSE)

send_telegram_message("Script finalizado")
