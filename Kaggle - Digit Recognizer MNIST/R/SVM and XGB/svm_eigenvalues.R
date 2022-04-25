rm(list=ls())

setwd("D:/!Machine Learning R and Python/Kaggle - Digit Recognizer")

library(caret)
library(dplyr)

Train = read.csv("train.csv")
Test = read.csv("test.csv")

Train$label = as.factor(Train$label)

nzv = nearZeroVar(Train, saveMetrics = T)
dropList = rownames(nzv[nzv$nzv == "TRUE",])
Train = Train %>% select(-dropList)
Test = Test %>% select(-dropList)

NewTrain = as.data.frame(matrix(ncol = 25,nrow = 42000))
NewTrain[,25] = Train$label
Train$label = NULL
Train = as.matrix(Train)

for (i in nrow(Train)) {
   Vector = Train[i,1:784]  
   Matrix = matrix(Vector,byrow = T,ncol = 28)
   EigenValues = eigen(Matrix)$values
   NewTrain[i,1:28] = EigenValues
   cat("Iteration:",i,"/",nrow(Train),"\n")
}

modelLookup("svmRadial")
getModelInfo("svmRadial")$`svmRadial`$grid

Grid = expand.grid(sigma = c(0.01, 0.5, 1), C =  c(1))

Control = trainControl(method = "cv", number = 5, search = "grid",
                       verboseIter = T)

Modelo = train(label ~ ., data = Train, method = "svmRadial",
               trControl = Control, tuneGrid = Grid, verbose = T,
               metric = "Accuracy", maximize = TRUE, preProcess=c("pca","scale"),
               na.action = "na.omit")

Modelo

send_telegram_message("Modelo acabado")

Predict_Train = predict(Modelo,Train[,-1])

confusionMatrix(data = Predict_Train,
                reference = Train$label)



Submission = read.csv("sample_submission.csv")

Submission[,2] = predict(Modelo,Test)
  
write.csv(Submission, file = "Submission.csv",row.names=FALSE)

send_telegram_message("Script finalizado")
