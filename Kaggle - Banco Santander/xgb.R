rm(list=ls())

# setwd("C:/Users/amalonso/Desktop/Banco Santander Kaggle")
setwd("D:/!Machine Learning R and Python/Kaggle - Banco Santander")

suppressPackageStartupMessages(library(xgboost))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(MLmetrics))
suppressPackageStartupMessages(library(telegram))
suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(data.table))

send_telegram_message = function(text){ 
  bot <- TGBot$new(token = "606732762:AAHvtOmBt5AG6Gv3ED2ddxW2RgncI0zdmGY")
  bot$sendMessage(text = text, chat_id = "469455650") }

Train = fread(input = "train.csv", header = TRUE)

Test = fread(input = "test.csv", header = TRUE)

print(object.size(Train), units = "MB", standard = "legacy")
print(object.size(Test), units = "MB", standard = "legacy")

Train = Train %>% select(-ID)

nzv = nearZeroVar(Train[,-1],
                  saveMetrics = TRUE,
                  allowParallel = TRUE)

dropList_nearZeroVar = rownames(nzv[nzv$nzv == "TRUE",])

Train = Train %>% select(-dropList_nearZeroVar)
Test = Test %>% select(-dropList_nearZeroVar)

print(object.size(Train), units = "MB", standard = "legacy")
print(object.size(Test), units = "MB", standard = "legacy")
###############
library(tidyverse)
library(xgboost)
library(magrittr)
library(caret)

set.seed(0)

#---------------------------
cat("Loading data...\n")

tr <- read_csv("../input/train.csv")
te <- read_csv("../input/test.csv")

target <- log1p(tr$target)
tr$target <- NULL

cols <- colnames(tr)

#---------------------------
cat("Dropping constant & duplicate features...\n")

nzv <- nearZeroVar(tr, names = TRUE, saveMetrics = TRUE)
dup <- cols[duplicated(lapply(tr, c))]
drop <- unique(c(rownames(nzv)[nzv$zeroVar], dup, "ID"))

tr %<>% select(-one_of(drop))
te %<>% select(-one_of(drop))

#---------------------------
cat("Preparing data...\n")

dtest <- xgb.DMatrix(data = data.matrix(te))
tri <- createDataPartition(target, p = 0.9, list = F) %>% c()
dtrain <- xgb.DMatrix(data = data.matrix(tr[tri, ]), label = target[tri])
dval <- xgb.DMatrix(data = data.matrix(tr[-tri, ]), label = target[-tri])

rm(tr, te, target, tri, drop, dup, nzv)
gc()

#---------------------------
cat("Training model...\n")

p <- list(objective = "reg:linear",
          booster = "gbtree",
          eval_metric = "rmse",
          nthread = 4,
          eta = 0.008,
          max_depth = 26,
          min_child_weight = 50,
          gamma = 6.5,
          subsample = 0.9,
          colsample_bytree = 0.3,
          colsample_bylevel = 0.3,
          alpha = 0,
          lambda = 0,
          nrounds = 4500)

m_xgb <- xgb.train(p, dtrain, p$nrounds, list(val = dval), print_every_n = 100, early_stopping_rounds = 500)

xgb.importance(cols, model = m_xgb) %>% 
  xgb.plot.importance(top_n = 30)

#---------------------------
cat("Making submission file...\n")

read_csv("../input/sample_submission.csv") %>%  
  mutate(target = expm1(predict(m_xgb, dtest))) %>%
  write_csv(paste0("base_xgb_", round(m_xgb$best_score, 5), ".csv"))
###############
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
