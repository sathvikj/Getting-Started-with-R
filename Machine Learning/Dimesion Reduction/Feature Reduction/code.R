set.seed(12345)

### Libraries ###
library(data.table)
library(ggplot2)
library(ClusterR)
library(dplyr)
library(ggplot2)
library(expm)
library(fastICA)
library(moments)
library(rsvd)
library(keras)
library(randomForest)
library(ade4)
library(caret)

data1 <- fread("OnlineNewsPopularity.csv")
data1 <- data1[,-c(1,2)]
data2 <- fread("credit.csv")
#data2 <- data2[,-c(24)]
norm <- function(x){
  (x - min(x))/(max(x)-min(x))
}
data1_norm <- data.frame(lapply(data1,norm))
data2_norm <- data.frame(lapply(data2,norm))

##### K-Means ####

wss1	<- rep(1,15)
for (i in 1:15) {
  model <- kmeans(data1,centers=i,nstart=10)
  wss1[i] <- model$tot.withinss
}
plot(1:15,	wss1, type="b", 
     xlab="Number of Clusters data1",
     ylab="Aggregate Within Group SS",
     main = "Aggregate within SS for data1")

modelk1 <- kmeans(data1,centers=6,nstart=10)
modelk1$centers
labelsk1 <- modelk1$cluster


wss2	<- rep(1,15)
for (i in 1:15) {
  model <- kmeans(data2_norm,centers=i,nstart=20) #normalized the data
  wss2[i] <- model$tot.withinss
}
plot(1:15,	wss2, type="b", 
     xlab="Number of Clusters data2",
     ylab="Aggregate Within Group SS",
     main = "Aggregate within SS for data2")

modelk2 <- kmeans(data2_norm,centers=4,nstart=20)
labelsk2 <- modelk2$cluster

########################## EM  ###########
bic1 = NULL
for(i in 4:10){
  model <- Mclust(data1x, G = i,modelNames = mclust.options("emModelNames"))
  bic1 <- rbind(bic1,model$bic)
}
plot(bic1,type = "b", xlab = "Cluster", ylab = "BIC", main = "Plot of BIC vs Cluster for data1")

### optimal model with 5 clusters

model1 <- Mclust(data1x, G = 5, modelNames = mclust.options("emModelNames"))
labelse1 <- model$classification

bic2 = NULL
for(i in 1:10){
  model <- Mclust(data2x, G = i,modelNames = mclust.options("emModelNames"))
  bic2 <- rbind(bic2,model$bic)
}
plot(bic2,type = "b", xlab = "Cluster", ylab = "BIC", main = "Plot of BIC vs Cluster for data2")

### optimal model with 5 clusters

model2 <- Mclust(data2x, G = 9, modelNames = mclust.options("emModelNames"))
labelse2 <- model2$classification 

#### Feature Selection ###
library(data.table)
data1 <- fread("OnlineNewsPopularity.csv")
data1 <- data1[,-c(1,2)]
test <- data1
test$shares <- as.factor(ifelse(test$shares > median(test$shares),"High","Low"))
model <- randomForest(x = test[,1:58],y = test$shares)
importance <- importance(model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>%
  mutate(Rank = dense_rank(desc(Importance)))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip()

rank <- rankImportance[order(rankImportance$Rank),]
num <- which(colnames(data1) %in% rank$Variables[1:15])
data1F <- cbind(subset(data1, select=c(num)),data1$shares)

#2
test <- data2
test$default <- as.factor(test$default)
model <- randomForest(x = test[,1:24],y = test$default)
importance <- importance(model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>%
  mutate(Rank = dense_rank(desc(Importance)))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip()

rank <- rankImportance[order(rankImportance$Rank),]
num <- which(colnames(data2) %in% rank$Variables[1:10])
data2F <- cbind(subset(data2, select=c(num)),data2$default)

### PCA ###
Xdata1 <- data1[,1:58]
pca1 <- prcomp(Xdata1, scale. = T)
plot(pca1$sdev^2, ylab = "Variance",main = "Plot for variance againt PC's' for data1", type = "b")
components1 <- pca1$x[,1:16]
components1 <- components1 %*% solve(sqrtm(crossprod(components1))) * sqrt(nrow(components1))
data1P <- cbind(components1,shares = data1$shares)


Xdata2 <- data2[,1:23]
pca2 <- prcomp(Xdata2, scale. = T)
plot(pca2$sdev^2, ylab = "Variance",main = "Plot for variance againt PC's' for data2", type = "b")
components2 <- pca2$x[,1:12]
components2 <- components2 %*% solve(sqrtm(crossprod(components2))) * sqrt(nrow(components2))
data2P <- cbind(components2,default = data2$default)

### ICA ###

Xdata1 <- data1_norm[,1:58]
ica1 <- fastICA(Xdata1,58)
ica1 <- data.frame(ica1$S)
tui1 <- data.frame(a = names(ica1),k = kurtosis(ica1))

tui1 <- tui1[(tui1$k < 5 & tui1$k > 2),]
num <- which(colnames(ica1) %in% tui1$a)
data1I <- cbind(subset(ica1, select=c(num)),data1$shares)
plot(tui1$k, ylab = "Kurtosis Score", main = "scatter plot of kurtosis values for \n selected Independent Components for data1")

Xdata2 <- data2_norm[,1:23]
ica2 <- fastICA(Xdata2,23)
ica2 <- data.frame(ica2$S)
tui2 <- data.frame(a = names(ica2),k = kurtosis(ica2))
plot(tui2$k, ylab = "Kurtosis Score", main = "scatter plot of kurtosis values for \n selected Independent Components for data2")


tui2 <- tui2[(tui2$k < 10 & tui2$k > 1),]
num <- which(colnames(ica2) %in% tui2$a)
data2I <- cbind(subset(ica2, select=c(num)),data2$default)

#### Random Projections ####

modelrp1 <- rpca(data1[,1:58], k = NULL, center = TRUE, scale = TRUE)
plot(modelrp1$sdev^2, ylab = "Variance", type = "b", main = "plot of Variance vs random Projections for data1")
dataR1 <- modelrp1$x[,1:16]
dataR1 <- cbind(dataR1,shares = data1$shares)

modelrp2 <- rpca(data2[,1:23], k = NULL, center = TRUE, scale = TRUE)
plot(modelrp2$sdev^2, ylab = "Variance", type = "b", main = "plot of Variance vs random Projections for data2")
dataR2 <- modelrp2$x[,1:6]
dataR2 <- cbind(dataR2,default = data2$default)

#########################################################################################
#### Task2 #####
# Featured data Clustering#
#1
wss1	<- rep(1,15)
for (i in 1:15) {
  model <- kmeans(data1F,centers=i,nstart=10)
  wss1[i] <- model$tot.withinss
}
plot(1:15,	wss1, type="b", 
     xlab="Number of Clusters data1",
     ylab="Aggregate Within Group SS",
     main = "Clustering after Feature Selection data1")

modelkF1 <- kmeans(data1,centers=4,nstart=10)
lebelskF1 <- modelk1$cluster
#2
wss2	<- rep(1,15)
for (i in 1:15) {
  model <- kmeans(data2F,centers=i,nstart=20) #normalized the data
  wss2[i] <- model$tot.withinss
}
plot(1:15,	wss2, type="b", 
     xlab="Number of Clusters data2",
     ylab="Aggregate Within Group SS",
     main = "Clustering after Feature Selection data2")

modelkF2 <- kmeans(data2F,centers=4,nstart=20)
labelskF2 <- modelkF2$cluster

#PCA Data Clustering
#1
wss1	<- rep(1,15)
for (i in 1:15) {
  model <- kmeans(data1P,centers=i,nstart=10)
  wss1[i] <- model$tot.withinss
}
plot(1:15,	wss1, type="b", 
     xlab="Number of Clusters data1",
     ylab="Aggregate Within Group SS",
     main = "clustering after PCA -  data1")

modelkP1 <- kmeans(data1P,centers=4,nstart=10)
lebelskP1 <- modelkP1$cluster
#2
wss2	<- rep(1,15)
for (i in 1:15) {
  model <- kmeans(data2P,centers=i,nstart=20) #normalized the data
  wss2[i] <- model$tot.withinss
}
plot(1:15,	wss2, type="b", 
     xlab="Number of Clusters data2",
     ylab="Aggregate Within Group SS",
     main = "clustering after PCA -  data2")

modelkP2 <- kmeans(data2P,centers=4,nstart=20)
labelskP2 <- modelkP2$cluster
# ICA Data Clustering
#1
wss1	<- rep(1,15)
for (i in 1:15) {
  model <- kmeans(data1I,centers=i,nstart=10)
  wss1[i] <- model$tot.withinss
}
plot(1:15,	wss1, type="b", 
     xlab="Number of Clusters data1",
     ylab="Aggregate Within Group SS",
     main = "clustering after ICA -  data1")

modelkI1 <- kmeans(data1I,centers=4,nstart=10)
lebelskI1 <- modelKI1$cluster
#2
wss2	<- rep(1,15)
for (i in 1:15) {
  model <- kmeans(data2I,centers=i,nstart=10)
  wss2[i] <- model$tot.withinss
}
plot(1:15,	wss2, type="b", 
     xlab="Number of Clusters data1",
     ylab="Aggregate Within Group SS",
     main = "clustering after ICA -  data2")

modelkI2 <- kmeans(data2I,centers=4,nstart=10)
lebelskI2 <- modelKI2$cluster

### Randomised Projection Clustering 
#1
wss1	<- rep(1,15)
for (i in 1:15) {
  model <- kmeans(dataR1,centers=i,nstart=10)
  wss1[i] <- model$tot.withinss
}
plot(1:15,	wss1, type="b", 
     xlab="Number of Clusters data1",
     ylab="Aggregate Within Group SS",
     main = "clustering after Random Projection -  data1")

modelkR1 <- kmeans(dataR1,centers=4,nstart=10)
lebelskR1 <- modelKR1$cluster
#2
wss2	<- rep(1,15)
for (i in 1:15) {
  model <- kmeans(norm(dataR2),centers=i,nstart=10)
  wss2[i] <- model$tot.withinss
}
plot(1:15,	wss2, type="b", 
     xlab="Number of Clusters data1",
     ylab="Aggregate Within Group SS",
     main = "clustering after Random Projection -  data2")

modelkR2 <- kmeans(dataR2,centers=4,nstart=10)
lebelskR2 <- modelKR2$cluster


#### EM after Dimention Reduction ####

one <- Mclust(data1F)
plot(one)
two <- Mclust(data2F)
plot(two)
three <- Mclust(data1P)
plot(three)
four <- Mclust(data2P)
plot(four)
five <- Mclust(data1I)
plot(five)
six <- Mclust(data2I)
plot(six)
seven <- Mclust(dataR1)
plot(seven)
eight <- Mclust(dataR2)
plot(eight)

#### Task 4 NN ####
## NN for feature data
#1
data1F$V2 <- as.factor(ifelse(data1F$V2 < median(data1F$V2),0,1))
data1F_norm <- cbind(data.frame(lapply(data1F[,-16], norm)),shares = data1F$V2)
data1F_norm <- as.matrix(data1F_norm)
dimnames(data1F_norm) <- NULL
sample <- sample.int(n = nrow(data1F_norm), size = floor(.75*nrow(data1F_norm)), replace = F)
train <- data1F_norm[sample, ] 
test  <- data1F_norm[-sample,]


train_1F_X <- train[,1:15]
test_1F_X <- test[,1:15]
train_1F_Y <- as.numeric(train[,16])
test_1F_Y <- as.numeric(test[,16])

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 26, activation = 'sigmoid', input_shape = c(15)) %>% 
  layer_dense(units = 16, activation = 'sigmoid') %>%
  layer_dense(units = 1, activation = 'sigmoid')
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)
model %>% fit(
  train_1F_X, 
  train_1F_Y, 
  epochs = 25, 
  batch_size = 5, 
  validation_split = 0.2
)
score <- model %>% evaluate(test_1F_X, test_1F_Y, batch_size = 128)
accuracyF1 <-  score$acc 
accuracyF1
#2
data2F$V2 <- as.factor(data2F$V2)
data2F_norm <- cbind(data.frame(lapply(data2F[,-11], norm)),default = data2F$V2)
data2F_norm <- as.matrix(data2F_norm)
dimnames(data2F_norm) <- NULL
sample <- sample.int(n = nrow(data2F_norm), size = floor(.75*nrow(data2F_norm)), replace = F)
train <- data2F_norm[sample, ] 
test  <- data2F_norm[-sample,]


train_2F_X <- train[,1:10]
test_2F_X <- test[,1:10]
train_2F_Y <- as.numeric(train[,11])
test_2F_Y <- as.numeric(test[,11])

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 26, activation = 'sigmoid', input_shape = c(10)) %>% 
  layer_dense(units = 16, activation = 'sigmoid') %>%
  layer_dense(units = 1, activation = 'sigmoid')
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)
model %>% fit(
  train_2F_X, 
  train_2F_Y, 
  epochs = 25, 
  batch_size = 5, 
  validation_split = 0.2
)
score <- model %>% evaluate(test_2F_X, test_2F_Y, batch_size = 128)
accuracyF2 <-  score$acc 
accuracyF2 

## NN for PCA data
#1
data1P <- data.frame(data1P)
data1P$shares <- as.factor(ifelse(data1P$shares < median(data1P$shares),0,1))
data1P_norm <- cbind(data.frame(lapply(data1P[,-17], norm)),shares = data1P$shares)
data1P_norm <- as.matrix(data1P_norm)
dimnames(data1P_norm) <- NULL
sample <- sample.int(n = nrow(data1P_norm), size = floor(.75*nrow(data1P_norm)), replace = F)
train <- data1P_norm[sample, ] 
test  <- data1P_norm[-sample,]


train_1P_X <- train[,1:16]
test_1P_X <- test[,1:16]
train_1P_Y <- as.numeric(train[,17])
test_1P_Y <- as.numeric(test[,17])

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 26, activation = 'sigmoid', input_shape = c(16)) %>% 
  layer_dense(units = 16, activation = 'sigmoid') %>%
  layer_dense(units = 1, activation = 'sigmoid')
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)
model %>% fit(
  train_1P_X, 
  train_1P_Y, 
  epochs = 25, 
  batch_size = 5, 
  validation_split = 0.2
)
score <- model %>% evaluate(test_1P_X, test_1P_Y, batch_size = 128)
accuracyP1 <-  score$acc 
accuracyP1
#2
data2P <- data.frame(data2P)
data2P$default <- as.factor(data2P$default)
data2P_norm <- cbind(data.frame(lapply(data2P[,-13], norm)),default = data2P$default)
data2P_norm <- as.matrix(data2P_norm)
dimnames(data2P_norm) <- NULL
sample <- sample.int(n = nrow(data2P_norm), size = floor(.75*nrow(data2P_norm)), replace = F)
train <- data2P_norm[sample, ] 
test  <- data2P_norm[-sample,]


train_2P_X <- train[,1:12]
test_2P_X <- test[,1:12]
train_2P_Y <- as.numeric(train[,13])
test_2P_Y <- as.numeric(test[,13])

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 26, activation = 'sigmoid', input_shape = c(12)) %>% 
  layer_dense(units = 16, activation = 'sigmoid') %>%
  layer_dense(units = 1, activation = 'sigmoid')
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)
model %>% fit(
  train_2P_X, 
  train_2P_Y, 
  epochs = 25, 
  batch_size = 5, 
  validation_split = 0.2
)
score <- model %>% evaluate(test_2P_X, test_2P_Y, batch_size = 128)
accuracyP2 <-  score$acc 
accuracyP2 

## NN for ICA data
#1
data1I <- data.frame(data1I)
data1I$data1.shares <- as.factor(ifelse(data1I$data1.shares < median(data1I$data1.shares),0,1))
data1I_norm <- cbind(data.frame(lapply(data1I[,-9], norm)),shares = data1I$data1.shares)
data1I_norm <- as.matrix(data1I_norm)
dimnames(data1I_norm) <- NULL
sample <- sample.int(n = nrow(data1I_norm), size = floor(.75*nrow(data1I_norm)), replace = F)
train <- data1I_norm[sample, ] 
test  <- data1I_norm[-sample,]


train_1I_X <- train[,1:8]
test_1I_X <- test[,1:8]
train_1I_Y <- as.numeric(train[,9])
test_1I_Y <- as.numeric(test[,9])

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 26, activation = 'sigmoid', input_shape = c(8)) %>% 
  layer_dense(units = 16, activation = 'sigmoid') %>%
  layer_dense(units = 1, activation = 'sigmoid')
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)
model %>% fit(
  train_1I_X, 
  train_1I_Y, 
  epochs = 25, 
  batch_size = 5, 
  validation_split = 0.2
)
score <- model %>% evaluate(test_1I_X, test_1I_Y, batch_size = 128)
accuracyI1 <-  score$acc 
accuracyI1
#2
data2I <- data.frame(data2I)
data2I$data2.default <- as.factor(data2I$data2.default)
data2I_norm <- cbind(data.frame(lapply(data2I[,-8], norm)),default = data2I$data2.default)
data2I_norm <- as.matrix(data2I_norm)
dimnames(data2I_norm) <- NULL
sample <- sample.int(n = nrow(data2I_norm), size = floor(.75*nrow(data2I_norm)), replace = F)
train <- data2I_norm[sample, ] 
test  <- data2I_norm[-sample,]


train_2I_X <- train[,1:7]
test_2I_X <- test[,1:7]
train_2I_Y <- as.numeric(train[,8])
test_2I_Y <- as.numeric(test[,8])

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 26, activation = 'sigmoid', input_shape = c(7)) %>% 
  layer_dense(units = 16, activation = 'sigmoid') %>%
  layer_dense(units = 1, activation = 'sigmoid')
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)
model %>% fit(
  train_2I_X, 
  train_2I_Y, 
  epochs = 25, 
  batch_size = 5, 
  validation_split = 0.2
)
score <- model %>% evaluate(test_2I_X, test_2I_Y, batch_size = 128)
accuracyI2 <-  score$acc 
accuracyI2 

## NN for Randomized Projections data
#1
dataR1 <- data.frame(dataR1)
dataR1$shares <- as.factor(ifelse(dataR1$shares < median(dataR1$shares),0,1))
dataR1_norm <- cbind(data.frame(lapply(dataR1[,-17], norm)),shares = dataR1$shares)
dataR1_norm <- as.matrix(dataR1_norm)
dimnames(dataR1_norm) <- NULL
sample <- sample.int(n = nrow(dataR1_norm), size = floor(.75*nrow(dataR1_norm)), replace = F)
train <- dataR1_norm[sample, ] 
test  <- dataR1_norm[-sample,]


train_1R_X <- train[,1:16]
test_1R_X <- test[,1:16]
train_1R_Y <- as.numeric(train[,17])
test_1R_Y <- as.numeric(test[,17])

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 26, activation = 'sigmoid', input_shape = c(16)) %>% 
  layer_dense(units = 16, activation = 'sigmoid') %>%
  layer_dense(units = 1, activation = 'sigmoid')
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)
model %>% fit(
  train_1R_X, 
  train_1R_Y, 
  epochs = 25, 
  batch_size = 5, 
  validation_split = 0.2
)
score <- model %>% evaluate(test_1R_X, test_1R_Y, batch_size = 128)
accuracyR1 <-  score$acc 
accuracyR1
#2
dataR2 <- data.frame(dataR2)
dataR2$default <- as.factor(dataR2$default)
dataR2_norm <- cbind(data.frame(lapply(dataR2[,-7], norm)),default = dataR2$default)
dataR2_norm <- as.matrix(dataR2_norm)
dimnames(dataR2_norm) <- NULL
sample <- sample.int(n = nrow(dataR2_norm), size = floor(.75*nrow(dataR2_norm)), replace = F)
train <- dataR2_norm[sample, ] 
test  <- dataR2_norm[-sample,]


train_2R_X <- train[,1:6]
test_2R_X <- test[,1:6]
train_2R_Y <- as.numeric(train[,7])
test_2R_Y <- as.numeric(test[,7])

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 26, activation = 'sigmoid', input_shape = c(6)) %>% 
  layer_dense(units = 16, activation = 'sigmoid') %>%
  layer_dense(units = 1, activation = 'sigmoid')
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)
model %>% fit(
  train_2R_X, 
  train_2R_Y, 
  epochs = 25, 
  batch_size = 5, 
  validation_split = 0.2
)
score <- model %>% evaluate(test_2R_X, test_2R_Y, batch_size = 128)
accuracyR2 <-  score$acc 
accuracyR2
nn <- data.frame(accuracyF1,accuracyF2,accuracyI1,accuracyI2,accuracyP1,accuracyP2,accuracyR1,accuracyR2)
nn
write.csv(nn,nn.csv)
 

## Neural Net on Clustering Results

library(caret)

#data1
NN1<- data.frame(C = as.factor(labelsk1), EM = as.factor(labelse1), shares = as.factor(ifelse(data1$shares < median(data1$shares),0,1)))

#data2
NN2 <- data.frame(C = as.factor(labelsk2), EM = as.factor(labelse2), default = as.factor(data2$default))

#Dummy Data
NNK1 <- as.matrix(cbind(data.frame(predict(dummyVars(~ C, data = NN1), newdata = NN1)),NN1$shares))
NNE1 <- as.matrix(cbind(data.frame(predict(dummyVars(~ EM, data = NN1), newdata = NN1)),NN1$shares))
NNK2 <- as.matrix(cbind(data.frame(predict(dummyVars(~ C, data = NN2), newdata = NN2)),NN2$default))
NNE2 <- as.matrix(cbind(data.frame(predict(dummyVars(~ EM, data = NN2), newdata = NN2)),NN2$default))


## NNK1
dimnames(NNK1) <- NULL
sample <- sample.int(n = nrow(NNK1), size = floor(.75*nrow(NNK1)), replace = F)
train <- NNK1[sample, ] 
test  <- NNK1[-sample,]


train_NNK1_X <- train[,1:6]
test_NNK1_X <- test[,1:6]
train_NNK1_Y <- as.numeric(train[,7])
test_NNK1_Y <- as.numeric(test[,7])

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 26, activation = 'sigmoid', input_shape = c(6)) %>% 
  layer_dense(units = 16, activation = 'sigmoid') %>%
  layer_dense(units = 1, activation = 'sigmoid')
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)
model %>% fit(
  train_NNK1_X, 
  train_NNK1_Y, 
  epochs = 25, 
  batch_size = 5, 
  validation_split = 0.2
)
score <- model %>% evaluate(test_NNK1_X, test_NNK1_Y, batch_size = 128)
accuracyNNK1 <-  score$acc 
accuracyNNK1

print("done1")
## NNE1

dimnames(NNE1) <- NULL
sample <- sample.int(n = nrow(NNE1), size = floor(.75*nrow(NNE1)), replace = F)
train <- NNE1[sample, ] 
test  <- NNE1[-sample,]


train_NNE1_X <- train[,1:9]
test_NNE1_X <- test[,1:9]
train_NNE1_Y <- as.numeric(train[,10])
test_NNE1_Y <- as.numeric(test[,10])

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 26, activation = 'sigmoid', input_shape = c(9)) %>% 
  layer_dense(units = 16, activation = 'sigmoid') %>%
  layer_dense(units = 1, activation = 'sigmoid')
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)
model %>% fit(
  train_NNE1_X, 
  train_NNE1_Y, 
  epochs = 25, 
  batch_size = 5, 
  validation_split = 0.2
)
score <- model %>% evaluate(test_NNE1_X, test_NNE1_Y, batch_size = 128)
accuracyNNE1 <-  score$acc 
accuracyNNE1
print("done2")
## NNK2

dimnames(NNK2) <- NULL
sample <- sample.int(n = nrow(NNK2), size = floor(.75*nrow(NNK2)), replace = F)
train <- NNK2[sample, ] 
test  <- NNK2[-sample,]


train_NNK2_X <- train[,1:4]
test_NNK2_X <- test[,1:4]
train_NNK2_Y <- as.numeric(train[,5])
test_NNK2_Y <- as.numeric(test[,5])

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 26, activation = 'sigmoid', input_shape = c(4)) %>% 
  layer_dense(units = 16, activation = 'sigmoid') %>%
  layer_dense(units = 1, activation = 'sigmoid')
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)
model %>% fit(
  train_NNK2_X, 
  train_NNK2_Y, 
  epochs = 25, 
  batch_size = 5, 
  validation_split = 0.2
)
score <- model %>% evaluate(test_NNK2_X, test_NNK2_Y, batch_size = 128)
accuracyNNK2 <-  score$acc 
accuracyNNK2
print("done3")
## NNE2

dimnames(NNE2) <- NULL
sample <- sample.int(n = nrow(NNE2), size = floor(.75*nrow(NNE2)), replace = F)
train <- NNE2[sample, ] 
test  <- NNE2[-sample,]


train_NNE2_X <- train[,1:7]
test_NNE2_X <- test[,1:7]
train_NNE2_Y <- as.numeric(train[,8])
test_NNE2_Y <- as.numeric(test[,8])

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 26, activation = 'sigmoid', input_shape = c(7)) %>% 
  layer_dense(units = 16, activation = 'sigmoid') %>%
  layer_dense(units = 1, activation = 'sigmoid')
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)
model %>% fit(
  train_NNE2_X, 
  train_NNE2_Y, 
  epochs = 25, 
  batch_size = 5, 
  validation_split = 0.2
)
score <- model %>% evaluate(test_NNE2_X, test_NNE2_Y, batch_size = 128)
accuracyNNE2 <-  score$acc 
accuracyNNE2
print("done4")

nn_five <- data.frame(accuracyNNK1,accuracyNNE1,accuracyNNK2,accuracyNNE2)
nn_five
write.csv(nn_five,"nn_five.csv")
