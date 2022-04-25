library(tidyverse)
library(xgboost)
library(magrittr)
library(caret)

set.seed(42)

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

m <- model.matrix(~.-1, tr) %>% cor(method = "spearman")
cor_var <- findCorrelation(m, cutoff = 0.99, names = TRUE) %>% gsub("`", "", .)
tr %<>% select(-one_of(cor_var))
te %<>% select(-one_of(cor_var))

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
          max_depth = 25,
          min_child_weight = 35,
          gamma = 0.6924046,
          subsample = 0.6701399,
          colsample_bytree = 0.2,
          colsample_bylevel = 0.7,
          alpha = 0,
          lambda = 16.1035,
          nrounds = 5000)

m_xgb <- xgb.train(p, dtrain, p$nrounds, list(val = dval), print_every_n = 100, early_stopping_rounds = 600)

xgb.importance(cols, model = m_xgb) %>% 
  xgb.plot.importance(top_n = 30)

#---------------------------
cat("Making submission file...\n")

read_csv("../input/sample_submission.csv") %>%  
  mutate(target = expm1(predict(m_xgb, dtest))) %>%
  write_csv(paste0("base_xgb_", round(m_xgb$best_score, 5), ".csv"))