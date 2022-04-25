rm(list=ls())

suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(telegram))

send_telegram_message = function(text){ 
  bot <- TGBot$new(token = "606732762:AAHvtOmBt5AG6Gv3ED2ddxW2RgncI0zdmGY")
  bot$sendMessage(text = text, chat_id = "469455650") }

Train = read.csv("train.csv", stringsAsFactors = FALSE, header = TRUE)
Test = read.csv("test.csv", stringsAsFactors = FALSE, header = TRUE)

Train = Train %>% select(-PassengerId)

droplist = c("Name","Ticket","Cabin")

Train = Train %>% select(-droplist)
Test = Test %>% select(-droplist)

str(Train)

Train$Survived = as.factor(Train$Survived)
table(Train$Survived)

Train$Pclass = as.factor(Train$Pclass)
table(Train$Pclass)

Train$Sex = as.factor(Train$Sex)

Train$Embarked = as.factor(Train$Embarked)
table(Train$Embarked)
Train$Embarked[Train$Embarked == ""] = "S"
Train$Embarked = droplevels(Train$Embarked)
table(Train$Embarked)

Train = na.omit(Train)

summary(Train) #OK

str(Test)
Test$Embarked = as.factor(Test$Embarked)
Test$Pclass = as.factor(Test$Pclass)

summary(Train)
summary(Test)

Test$Age[is.na(Test$Age)] = mean(Test$Age, na.rm = TRUE)
summary(Test$Age)

Test$Fare[is.na(Test$Fare)] = mean(Test$Fare, na.rm = TRUE)
summary(Test$Fare)

sum(is.na(Train))
sum(is.na(Test))

# Grid = expand.grid(nrounds = seq(from = 5,length.out = 7,by = 5),
#                   max_depth = seq(from = 1,length.out = 7),
#                   eta =  seq(from = 0.1,length.out = 3,to=1),
#                   gamma = seq(from = 0,length.out = 3,by = 1),
#                   colsample_bytree = seq(from = 0.1,length.out = 3,to=1),
#                   min_child_weight = seq(from = 0,length.out = 3,by = 1),
#                   subsample = 0.8)

Control = trainControl(method = "cv",
                       search = "random",
                       verboseIter = T,
                       p = 0.8,
                       savePredictions = T, 
                       number = 10)

Modelo = train(Survived ~ .,
               data = Train,
               method = "treebag",
               trControl = Control,
               verbose = T,
               # tuneGrid = Grid,
               metric = "Accuracy",
               maximize = TRUE)

A = confusionMatrix(data = predict(Modelo, Train[,-1]),
                reference = Train$Survived)

A

A = as.numeric(A$overall[1])

A = round(A,2)

Prediction = predict(Modelo, Test)

Final = data.frame(Test$PassengerId,Prediction)

Sample = read.csv("gender_submission.csv")

colnames(Final) = colnames(Sample)

write.csv(Final, file = paste0("Titanic__",A,"_",Modelo$method,".csv"),row.names=FALSE)

send_telegram_message(paste0("Scritp finalizado ",A))
