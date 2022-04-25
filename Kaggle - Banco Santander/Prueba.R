rm(list=ls())

# setwd("C:/Users/amalonso/Desktop/Banco Santander Kaggle")
setwd("D:/!Machine Learning R and Python/Kaggle - Banco Santander")

suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(MLmetrics))
suppressPackageStartupMessages(library(telegram))
suppressPackageStartupMessages(library(data.table))

send_telegram_message = function(text){ 
  bot <- TGBot$new(token = "606732762:AAHvtOmBt5AG6Gv3ED2ddxW2RgncI0zdmGY")
  bot$sendMessage(text = text, chat_id = "469455650") }

Train = fread(input = "train.csv", header = TRUE)

Test = fread(input = "test.csv", header = TRUE)

print(object.size(Train), units = "MB", standard = "legacy")
print(object.size(Test), units = "MB", standard = "legacy")

Train = Train %>% select(-ID)

nzv = nearZeroVar(Train[,-1], saveMetrics = TRUE)

dropList_nearZeroVar = rownames(nzv[nzv$nzv == "TRUE",])

Train = Train %>% select(-dropList_nearZeroVar)
Test = Test %>% select(-dropList_nearZeroVar)

print(object.size(Train), units = "MB", standard = "legacy")
print(object.size(Test), units = "MB", standard = "legacy")

RMSLE_summary = function(data, lev = NULL, model = NULL) {
  out = MLmetrics::RMSLE(y_true = data[, "obs"], y_pred = data[, "pred"])
  names(out) = c("RMSLE")
  out
}

modelLookup("xgbTree")
getModelInfo("xgbTree")$`xgbTree`$grid

Grid = expand.grid(nrounds = c(1, 10, 50, 100),
                   max_depth = c(5:10),
                   eta = c(0, 0.1, 0.2, 0.3),
                   gamma = c(0, 0.1, 0.2),
                   colsample_bytree = c(0.5,0.7, 0.9),
                   min_child_weight = c(0.5, 1, 1.5),
                   subsample = c(0.5,0.7, 0.9)
)

TrainingParameters = trainControl(method = "cv",
                                  number = 2,
                                  verboseIter = TRUE,
                                  summaryFunction = RMSLE_summary,
                                  search = "grid")

Modelo = train(target ~ ., data = Train, method = "xgbTree",
               trControl= TrainingParameters, tuneGrid = Grid,
               verboseIter = TRUE, metric = "RMSLE",
               maximize = FALSE)

Modelo

send_telegram_message("Modelo acabado")

Predict_Train = exp(predict(Modelo,Train[,-1]))

cat("RMSLE in Train:",RMSLE(y_pred = Predict_Train,
                            y_true = exp(Train$target)))

Submission = data.frame()

for (i in 1:nrow(Test)) {
  Submission[i,1] = Test[i,"ID"]
  Submission[i,2] = exp(predict(Modelo,Test[i,-1]))
  cat("Iteration",i,"\n")
}

Sample = read.csv("sample_submission.csv")

colnames(Submission) = c("ID","target")

Submission$target = exp(Submission$target)

summary(Submission$target)

write.csv(Submission, file = "Submission_.csv",row.names=FALSE)

summary(Train$target)
hist(Train$target, breaks = 100, density = TRUE, probability = TRUE,
     main="Histograma target en Train")

send_telegram_message("Script finalizado")