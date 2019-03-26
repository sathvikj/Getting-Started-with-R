#install.packages("data.table")
#install.packages("class")
library(data.table)
library(class)

t1 <- Sys.time()

data1 <- fread("OnlineNewsPopularity.csv")
data1 <- data1[,-c(1,2)]
data1$shares <- as.factor(ifelse(data1$shares < median(data1$shares),"LOW","HIGH"))

norm <- function(x){
  (x - min(x))/(max(x)-min(x))
}
tui <- function(dataset = ""){
  round(sum(diag(dataset))/sum(dataset)*100,2)
}

data1_norm <- cbind(data.frame(lapply(data1[,-59], norm)),shares = data1$shares)
sample <- sample.int(n = nrow(data1_norm), size = floor(.75*nrow(data1_norm)), replace = F)
train <- data1_norm[sample, ] 
test  <- data1_norm[-sample,]


K <- seq(5,200,by = 10)

Output <- data.frame(K = NULL,accuracy = NULL)
for (i in K){
  print(paste("we are at the iteration where k =",i))
  predict <- data.frame(pred = knn(train[,-59],train[,-59],train[,59],i))
  conf <- as.matrix(table(predict$pred,train[,59]))
  accuracy <- data.frame(K = i,accuracy = tui(conf))
  Output <- rbind(Output,accuracy)
}
write.csv(Output, file = "KNNdata1_train.csv")
plot(Output,type = 'line', col = 'blue',xlab = "Number of Nearest Neighbors" , ylab = "Accuracy", main = "Accuracy v/s K for OnlineNewsPopularity Data")
t1 <- Sys.time()-t1
print(paste("time took to complete data1 is",t1))

predict_test <- knn(train[,-59],test[,-59],train[,59],150)
conf <- as.matrix(table(predict_test,test[,59]))
test_accuracy <- tui(conf)
test_accuracy

##### data2 ####
t2 <- Sys.time()
data2 <- fread("credit.csv")
data2$default <- as.factor(data2$default)
data2_norm <- cbind(data.frame(lapply(data2[,-24], norm)),default = data2$default)
sample <- sample.int(n = nrow(data2_norm), size = floor(.75*nrow(data2_norm)), replace = F)
train <- data2_norm[sample, ] 
test  <- data2_norm[-sample,]

K <- seq(5,170,by = 10)

Output <- data.frame(K = NULL,accuracy = NULL)
for (i in K){
  print(paste("we are at the iteration where k =",i))
  predict <- data.frame(pred = knn(train[,-24],train[,-24],train[,24],i))
  conf <- as.matrix(table(predict$pred,train[,24]))
  accuracy <- data.frame(K = i,accuracy = tui(conf))
  Output <- rbind(Output,accuracy)
}
write.csv(Output, file = "KNNdata2_train.csv")
plot(Output,type = 'line', col = 'blue',xlab = "Number of Nearest Neighbors" , ylab = "Accuracy", main = "Accuracy v/s K for Credit Data")
t2 <- Sys.time()-t2
print(paste("time took to complete data2 is",t2))

predict_test <- knn(train[,-24],test[,-24],train[,24],125)
conf <- as.matrix(table(predict_test,test[,24]))
test_accuracy <- tui(conf)
test_accuracy


















