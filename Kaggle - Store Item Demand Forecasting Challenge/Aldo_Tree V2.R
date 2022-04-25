rm(list=ls())

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(forecast))
suppressPackageStartupMessages(library(Metrics))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(lubridate))

SMAPE_summary = function(data, lev = NULL, model = NULL) {
  out = Metrics::smape(actual = data[, "obs"],
                       predicted = data[, "pred"])
  names(out) = c("SMAPE")
  out
}

Train = read_csv("train.csv")
Test = read_csv("test.csv")

Train = Train %>%  mutate(year = as.integer(format(date, "%Y")),
                          month = as.integer(format(date, "%m")),
                          weekday = wday(date, week_start = 1),
                          monthday = mday(date),
                          yearday = yday(date))

Test = Test %>%  mutate(year = as.integer(format(date, "%Y")),
                        month = as.integer(format(date, "%m")),
                        weekday = as.integer(factor(weekdays(date, abbreviate = TRUE)) ),
                        
)

Train$month = as.factor(Train$month)
Test$month = as.factor(Test$month)

Train_New = Train %>% 
  filter(month == 1 | month == 2 | month == 3) %>%
  select(sales,month,weekday,item,store,year,quoter)

Test_New = Test %>%
  select(month,weekday,item,store,year,quoter,id)

Grid = expand.grid(nrounds = c(10,50,100,300),
                   max_depth = c(5,15),
                   eta = c(0.3,0.8),
                   gamma = c(0.1,0.8),
                   colsample_bytree = .7,
                   min_child_weight = 1,
                   subsample = 0.8)

TrainingParameters = trainControl(method = "cv",
                                  number = 3,
                                  p = 0.7,
                                  verboseIter = TRUE,
                                  search = "grid",
                                  summaryFunction = SMAPE_summary,
                                  savePredictions = TRUE)

Modelo = train(sales ~ ., data = Train_New,
               method = "xgbTree",
               trControl = TrainingParameters,
               tuneGrid = Grid,
               na.action = na.omit,
               verboseIter = TRUE,
               metric = "SMAPE",
               maximize = FALSE)

Modelo

plot(Modelo)

Forecast = ceiling(predict(object = Modelo, newdata = Test_New))

Submission = cbind(Test_New$id, Forecast)
colnames(Submission) = c("id","sales")

write.csv(Submission, file = "Submission.csv",row.names=FALSE)
