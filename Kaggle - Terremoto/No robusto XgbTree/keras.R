rm(list=ls())

library(keras)
library(dplyr)
library(caret)

data = read.csv(file = "Terremotos.csv", header = FALSE)

x = data[,-ncol(data)] %>% as.matrix()
y = data[,ncol(data)] 

set.seed(1991)
index = createDataPartition(y,p = 0.8,list = F)

x_train = x[index,] %>% as.matrix()
y_train = y[index] 
x_val = x[-index,] %>% as.matrix()
y_val = y[-index]

x_train = x_train %>% scale()

# Use means and standard deviations from training set to normalize test set
col_means_x_train = attr(x_train, "scaled:center") 
col_stddevs_x_train = attr(x_train, "scaled:scale")
x_val = scale(x_val, center = col_means_x_train, scale = col_stddevs_x_train)

model <- keras_model_sequential() %>%
  layer_dense(units = 60, activation = "relu", input_shape = dim(x_train)[2]) %>%
  layer_dropout(0.4) %>% 
  layer_dense(units = 30, activation = "relu") %>%
  layer_dropout(0.3) %>% 
  layer_dense(units = 15, activation = "relu") %>%
  layer_dense(units = 1)

model %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop(),
  metrics = list("mean_absolute_error")
)

history <- model %>% fit(x_train,y_train,
                         epochs = 200,
                         batch_size = 20,
                         validation_data = list(x_val,y_val)
)

results = model %>% evaluate(x_val, y_val)

results$mean_absolute_error


