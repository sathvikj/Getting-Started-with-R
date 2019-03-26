install.packages("rpart")
rm(list = ls())
set.seed(12345)
library(rpart)
library(e1071)
library(data.table)
tui <- function(dataset = ""){
  round(sum(diag(dataset))/sum(dataset)*100,2)
}
data2 <- fread("credit.csv")
#data2 <- data2[,-c(1,2)]
data2$default <- as.factor(ifelse(data2$default == 0,"NO","YES"))
#data2$default <- as.factor(data2$default)
sample <- sample.int(n = nrow(data2), size = floor(.75*nrow(data2)), replace = F)
train <- data2[sample, ]
test  <- data2[-sample,]

Output_size<- data.frame(depth = NULL,accuracy = NULL)
size = c(2,3,4,5,6,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30)
for (s in size){
  tree <- rpart(default~., data = train,method = "class", control=rpart.control(maxdepth = s,xval = 0,cp = 0))
  predict <- predict(tree,test,type = "class")
  conf <- as.matrix(table(test$default,predict))
  accuracy <- data.frame(s,accuracy = tui(conf))
  Output_size <- rbind(Output_size,accuracy)
}
plot(Output_size,type = 'line',col = 'blue', xlab = "max depth parameter" , ylab = "accuracy", main = "Plot of Accuracy wrt depth for data2")
Output_size
#Tryin the partition without any cross validation
Output_CP <- data.frame(CP = NULL, accuracy = NULL)
cp = c(0,.0001,.001,.01,.1)
for (c in cp){
  tree <- rpart(default~., data = train,method = "class", control=rpart.control(xval = 0,cp = c))
  val <- prune(tree, cp = c)
  print(c)
  predict <- predict(val,test,type = "class")
  conf <- as.matrix(table(test$default,predict))
  accuracy <- data.frame(CP = c,accuracy = tui(conf))
  Output_CP <- rbind(Output_CP,accuracy)
}
plot(Output_CP,type = 'line',col = 'blue',xlab = "Complexity Paramater" , ylab = "accuracy %", main = "Plot of Accuracy wrt Complexity Parameter(NO CV)")

Output_CP

Output_Val <- data.frame(CP = NULL, accuracy = NULL)
cp = c(0,.0001,.001,.01,.1)
for (c in cp){
  tree <- rpart(default~., data = train,method = "class", control=rpart.control(xval = 10,cp = c))
  val <- prune(tree, cp = c)
  predict <- predict(val,test,type = "class")
  conf <- as.matrix(table(test$default,predict))
  accuracy <- data.frame(CP = c,accuracy = tui(conf))
  Output_Val <- rbind(Output_Val,accuracy)
}
plot(Output_Val,type = 'line',col = 'blue',xlab = "Complexity Paramater" , ylab = "accuracy % ", main = "Plot of Accuracy wrt Complexity Parameter(CV")
Output_Val
cbind(Output_CP,Output_Val)






