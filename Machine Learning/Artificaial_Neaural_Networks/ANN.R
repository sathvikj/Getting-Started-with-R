install.packages("devtools")
library(devtools)
devtools::install_github("rstudio/keras")
install.packages("keras")

install.packages("tensorflow")
library(tensorflow)
install_tensorflow()

library(keras)
install_keras()
library(data.table)

t0 <- Sys.time()
set.seed(12345)
data1 <- fread("OnlineNewsPopularity.csv")
data1 <- data1[,-c(1,2)]
data1$shares <- as.factor(ifelse(data1$shares < median(data1$shares),0,1))

norm <- function(x){
  (x - min(x))/(max(x)-min(x))
}

tui <- function(dataset = ""){
  round(sum(diag(dataset))/sum(dataset)*100,2)
}

data1_norm <- cbind(data.frame(lapply(data1[,-59], norm)),shares = data1$shares)
data1_norm <- as.matrix(data1_norm)
dimnames(data1_norm) <- NULL
sample <- sample.int(n = nrow(data1_norm), size = floor(.75*nrow(data1_norm)), replace = F)
train <- data1_norm[sample, ] 
test  <- data1_norm[-sample,]

train_x <- train[,1:58]
train_y <- as.numeric(train[,59])
test_x <- test[,1:58]
test_y <- as.numeric(test[,59])

layer1 <- seq(5,30,by = 3)
layer2 <- seq(2,18,by = 2)
tests <- data.frame(cbind(layer1,layer2))

#### Tuning for epochs ####

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 30, activation = 'sigmoid', input_shape = c(58)) %>% 
  layer_dense(units = 20, activation = 'sigmoid') %>%
  layer_dense(units = 1, activation = 'sigmoid')
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)
model %>% fit(
  train_x, 
  train_y, 
  epochs = 300, 
  batch_size = 5, 
  validation_split = 0.2
)
score <- model %>% evaluate(train_x, train_y, batch_size = 128)
accuracy <-  score$acc 
accuracy

### Relu Activation ###
Output_relu <- data.frame(accuracy = NULL)
for( i in 1:nrow(tests)){
model <- keras_model_sequential()
model %>% 
  layer_dense(units = tests[i,]$layer1, activation = 'relu', input_shape = c(58)) %>% 
  layer_dense(units = tests[i,]$layer2, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'sigmoid')
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)
model %>% fit(
  train_x, 
  train_y, 
  epochs = 25, 
  batch_size = 5, 
  validation_split = 0.2
)
score <- model %>% evaluate(train_x, train_y, batch_size = 128)
accuracy <-  score$acc
Output_relu <- rbind(Output_relu,accuracy = accuracy)
}
relu <- cbind(tests, accuracy = round(Output_relu*100,2),activation = "relu")
colnames(relu) <- c("first","second","acc","act")
write.csv(relu,file = "relu_sun.csv")
### Tanh Activation ###
Output_tanh <- data.frame(accuracy = NULL)
for( i in 1:nrow(tests)){
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = tests[i,]$layer1, activation = 'tanh', input_shape = c(58)) %>% 
    layer_dense(units = tests[i,]$layer2, activation = 'tanh') %>%
    layer_dense(units = 1, activation = 'sigmoid')
  model %>% compile(
    loss = 'binary_crossentropy',
    optimizer = 'adam',
    metrics = 'accuracy'
  )
  model %>% fit(
    train_x, 
    train_y, 
    epochs = 25, 
    batch_size = 5, 
    validation_split = 0.2
  )
  score <- model %>% evaluate(train_x, train_y, batch_size = 128)
  accuracy <-  score$acc
  Output_tanh <- rbind(Output_tanh,accuracy)
}
tanh <- cbind(tests, accuracy = round(Output_tanh*100,2),activation = "tanh")
colnames(tanh) <- c("first","second","acc","act")
write.csv(tanh,file = "tanh_sun.csv")

### Sigmoid Activation ###
Output_sigmoid <- data.frame(accuracy = NULL)
for( i in 1:nrow(tests)){
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = tests[i,]$layer1, activation = 'sigmoid', input_shape = c(58)) %>% 
    layer_dense(units = tests[i,]$layer2, activation = 'sigmoid') %>%
    layer_dense(units = 1, activation = 'sigmoid')
  model %>% compile(
    loss = 'binary_crossentropy',
    optimizer = 'adam',
    metrics = 'accuracy'
  )
  model %>% fit(
    train_x, 
    train_y, 
    epochs = 25, 
    batch_size = 5, 
    validation_split = 0.2
  )
  score <- model %>% evaluate(train_x, train_y, batch_size = 128)
  accuracy <-  score$acc 
  Output_sigmoid <- rbind(Output_sigmoid,accuracy)
}
sigmoid <- cbind(tests, accuracy = round(Output_sigmoid*100,2),activation = "sigmoid")
colnames(sigmoid) <- c("first","second","acc","act")
write.csv(sigmoid,file = "sigmoid_sun.csv")

final <- rbind(relu,tanh,sigmoid);final
write.csv(final_sun,file = "news.csv")

###### Testing the best model ######
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 23, activation = 'sigmoid', input_shape = c(58)) %>% 
  layer_dense(units = 14, activation = 'sigmoid') %>%
  layer_dense(units = 1, activation = 'sigmoid')
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)
model %>% fit(
  train_x, 
  train_y, 
  epochs = 25, 
  batch_size = 5, 
  validation_split = 0.2
)
score <- model %>% evaluate(test_x, test_y, batch_size = 128)
accuracy <-  score$acc 
accuracy 


 
######################### DATA 2 ############################################
  
  data2 <- fread("credit.csv") 
  data2_norm <- cbind(data.frame(lapply(data2[,-24], norm)),default = data2$default)
  data2_norm <- as.matrix(data2_norm)
  dimnames(data2_norm) <- NULL
  sample <- sample.int(n = nrow(data2_norm), size = floor(.75*nrow(data2_norm)), replace = F)
  train <- data2_norm[sample, ] 
  test  <- data2_norm[-sample,]
  
  train_x <- train[,1:23]
  train_y <- as.numeric(train[,24])
  test_x <- test[,1:23]
  test_y <- as.numeric(test[,24])
  
  layer1 <- seq(5,30,by = 3)
  layer2 <- seq(2,18,by = 2)
  tests <- data.frame(cbind(layer1,layer2))  
  
######### TEST FOR EPOCHS #######################################
  
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 30, activation = 'sigmoid', input_shape = c(23)) %>% 
    layer_dense(units = 20, activation = 'sigmoid') %>%
    layer_dense(units = 1, activation = 'sigmoid')
  model %>% compile(
    loss = 'binary_crossentropy',
    optimizer = 'adam',
    metrics = 'accuracy'
  )
  model %>% fit(
    train_x, 
    train_y, 
    epochs = 300, 
    batch_size = 5, 
    validation_split = 0.2
  )
  score <- model %>% evaluate(train_x, train_y, batch_size = 128)
  accuracy <-  score$acc 
  accuracy    
    
#### Relu Activation #####
  Output_relu <- data.frame(accuracy = NULL)
  for( i in 1:nrow(tests)){
    model <- keras_model_sequential()
    model %>% 
      layer_dense(units = tests[i,]$layer1, activation = 'relu', input_shape = c(23)) %>% 
      layer_dense(units = tests[i,]$layer2, activation = 'relu') %>%
      layer_dense(units = 1, activation = 'sigmoid')
    model %>% compile(
      loss = 'binary_crossentropy',
      optimizer = 'adam',
      metrics = 'accuracy'
    )
    model %>% fit(
      train_x, 
      train_y, 
      epochs = 30, 
      batch_size = 5, 
      validation_split = 0.2
    )
    score <- model %>% evaluate(train_x, train_y, batch_size = 128)
    accuracy <-  score$acc
    Output_relu <- rbind(Output_relu,accuracy = accuracy)
  }
  relu <- cbind(tests, accuracy = round(Output_relu*100,2),activation = "relu")
  colnames(relu) <- c("first","second","acc","act")
  write.csv(relu,file = "reludata2_sun.csv")

### Tanh Activation ###
  Output_tanh <- data.frame(accuracy = NULL)
  for( i in 1:nrow(tests)){
    model <- keras_model_sequential()
    model %>% 
      layer_dense(units = tests[i,]$layer1, activation = 'tanh', input_shape = c(23)) %>% 
      layer_dense(units = tests[i,]$layer2, activation = 'tanh') %>%
      layer_dense(units = 1, activation = 'sigmoid')
    model %>% compile(
      loss = 'binary_crossentropy',
      optimizer = 'adam',
      metrics = 'accuracy'
    )
    model %>% fit(
      train_x, 
      train_y, 
      epochs = 30, 
      batch_size = 5, 
      validation_split = 0.2
    )
    score <- model %>% evaluate(train_x, train_y, batch_size = 128)
    accuracy <-  score$acc
    Output_tanh <- rbind(Output_tanh,accuracy)
  }
  tanh <- cbind(tests, accuracy = round(Output_tanh*100,2),activation = "tanh")
  colnames(tanh) <- c("first","second","acc","act")
  write.csv(tanh,file = "tanhdata2_sun.csv")
  
### Sigmoid Activation ###
  Output_sigmoid <- data.frame(accuracy = NULL)
  for( i in 1:nrow(tests)){
    model <- keras_model_sequential()
    model %>% 
      layer_dense(units = tests[i,]$layer1, activation = 'sigmoid', input_shape = c(23)) %>% 
      layer_dense(units = tests[i,]$layer2, activation = 'sigmoid') %>%
      layer_dense(units = 1, activation = 'sigmoid')
    model %>% compile(
      loss = 'binary_crossentropy',
      optimizer = 'adam',
      metrics = 'accuracy'
    )
    model %>% fit(
      train_x, 
      train_y, 
      epochs = 30, 
      batch_size = 5, 
      validation_split = 0.2
    )
    score <- model %>% evaluate(train_x, train_y, batch_size = 128)
    accuracy <-  score$acc 
    Output_sigmoid <- rbind(Output_sigmoid,accuracy)
  }
  sigmoid <- cbind(tests, accuracy = round(Output_sigmoid*100,2),activation = "sigmoid")
  colnames(sigmoid) <- c("first","second","acc","act")
  write.csv(sigmoid,file = "sigmoidata2_sun.csv")
  
  final <- rbind(relu,tanh,sigmoid);final
  write.csv(final,file = "credit_final_sun.csv")
  time <- Sys.time()-t0;time
   
####### Testing the best model #####
  
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 26, activation = 'relu', input_shape = c(23)) %>% 
    layer_dense(units = 16, activation = 'relu') %>%
    layer_dense(units = 1, activation = 'sigmoid')
  model %>% compile(
    loss = 'binary_crossentropy',
    optimizer = 'adam',
    metrics = 'accuracy'
  )
  model %>% fit(
    train_x, 
    train_y, 
    epochs = 30, 
    batch_size = 5, 
    validation_split = 0.2
  )
  score <- model %>% evaluate(test_x, test_y, batch_size = 128)
  accuracy <-  score$acc 
  accuracy  
  