rm(list=ls())
library(tidyverse)
library(keras)

train = read_csv("train.csv")
test = read_csv("test.csv")

x_train = train %>% select(-label)
x_train = array_reshape(data.matrix(x_train),c(nrow(x_train),28,28,1))/255

y_train = train %>% select(label)
y_train = to_categorical(data.matrix(y_train))

test = array_reshape(data.matrix(test),c(nrow(test),28,28,1))/255

model = keras_model_sequential()
model %>% 
  layer_conv_2d(filters = 32, kernel_size = c(5,5),padding = 'Same', activation = 'relu', input_shape = c(28,28,1)) %>%
  layer_conv_2d(filters = 32, kernel_size = c(5,5),padding = 'Same', activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3),padding = 'Same', activation = 'relu')%>%
  layer_conv_2d(filters = 64, kernel_size = c(3,3),padding = 'Same', activation = 'relu')%>%
  layer_max_pooling_2d(pool_size = c(2,2), strides = c(2,2)) %>% 
  layer_dropout(rate = 0.25) %>%
  layer_flatten() %>% 
  layer_dense(units=256,activation='relu')%>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units=256,activation='relu')%>%
  layer_dense(units=10,activation='softmax')

summary(model)

optimizer = optimizer_rmsprop(lr = 0.001, rho = 0.9, epsilon = 1e-08, decay = 0)

learning_rate_reduction = 

model %>% compile(optimizer= optimizer,
                  loss='categorical_crossentropy',
                  metrics='accuracy')

datagen = image_data_generator(
  featurewise_center = F,
  samplewise_center=F,
  featurewise_std_normalization = F,
  samplewise_std_normalization=F,
  zca_whitening=F,
  horizontal_flip = F,
  vertical_flip = F,
  width_shift_range = 0.15,
  height_shift_range = 0.15,
  zoom_range = 0.15,
  rotation_range = 15,
  shear_range = 0.15)

datagen %>% fit_image_data_generator(x_train)

history = model %>% 
  fit_generator(flow_images_from_data(x_train, y_train, datagen, batch_size = 86),
                steps_per_epoch = nrow(x_train)/86, epochs = 15, verbose = 1)

pred = predict_classes(object = model,
                       x = test,
                       verbose = 1)

cnnsubmission = data.frame(ImageId=1:nrow(test),Label=pred)

write.csv(cnnsubmission, file="cnn.csv", row.names=F)








