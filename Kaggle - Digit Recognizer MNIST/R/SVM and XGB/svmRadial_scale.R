rm(list=ls())

setwd("D:/!Machine Learning R and Python/Kaggle - Digit Recognizer")

library(caret)
library(dplyr)

Train = read.csv("train.csv")
Test = read.csv("test.csv")

nzv = nearZeroVar(Train, saveMetrics = T)

dropList = rownames(nzv[nzv$nzv == "TRUE",])

Train = Train %>% select(-dropList)
Test = Test %>% select(-dropList)

Train$label = as.factor(Train$label)

Control = trainControl(method = "cv",
                       number = 10,
                       search = "grid",
                       verboseIter = T)

Modelo = train(label ~ .,
               data = Train,
               method = "svmRadial",
               trControl = Control,
               tuneLength = 4,
               verbose = T,
               metric = "Accuracy",
               maximize = TRUE,
               preProcess=c("scale"),
               na.action = "na.omit")

plot(Modelo)

Submission = read.csv("sample_submission.csv")

Submission[,2] = predict(Modelo,Test)
  
write.csv(Submission, file = "Submission.csv",row.names=FALSE)

