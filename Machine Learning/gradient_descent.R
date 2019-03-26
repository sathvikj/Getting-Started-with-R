rm(list=ls(all=T))

library(data.table)

#Functions to normalize the data 

mean_normalize <- function(x) {
  return ((x - mean(x)) / sqrt(var(x)))
}

#Function to calculate RMSE

RMSE = function(x,y){
  sqrt(mean((x - y)^2))
}

# Gradient Descent Alogorithm for Linear Regression
algo_linear <- function(y, X, c.tres ,alpha,iters){
  c.tres = .0001
  X = as.matrix(data.frame(rep(1,length(y)),X))  #matrix of independent variables
  N= dim(X)[1] # number of independent variables
  #beta.i = as.matrix(rnorm(n=dim(X)[2], mean=0,sd = 1))
  beta.i = as.matrix(rep(0,ncol(X)))# Initial theta values(Coefficients)
  beta.i = t(beta.i)   #transpose to get initial vector of coefficients
  e = t(y) - beta.i%*%t(X) #this is y minus y hat or error
  grad.i = -(2/N)%*%(e)%*%X #calculating the initial gradient
  beta = beta.i - alpha*(1/N)*grad.i #calculating the 
  desc = c()
  for(i in 1:iters){ #Loop to generate the best coefficients until the convergence treshold is reached
    desc = c(desc,sqrt(sum((t(y) -  beta%*%t(X))^2)))
    e = t(y) - beta%*%t(X)
    grad = -(2/N)%*%e%*%X
    beta = beta - alpha*(2/N)*grad
    if(sqrt(sum(grad^2)) <= c.tres){
      break  # to beark the code when the convergence treshold is reached
    }
  }
  print(sqrt(sum(grad^2)))
  values<-list("coefficients" = t(beta), "descent" = desc)
  return(values)
}

# Gradient Descent Algorithm for Logistic Regression

algo_logistic <- function(y, X,alpha,iters){
  X = as.matrix(data.frame(rep(1,length(y)),X))  #matrix of independent variables
  N = dim(X)[1] # number of independent variables
  #beta.i = as.matrix(rnorm(n=dim(X)[2], mean=0,sd = 1))
  beta.i = as.matrix(rep(0,ncol(X)))# Initial theta values(Coefficients)
  beta.i = t(beta.i)   #transpose to get initial vector of coefficients
  e = t(y) - 1/(1+exp(-(beta.i%*%t(X)))) #this is y minus y hat or error
  grad.i = -(2/N)%*%(e)%*%X
  beta = beta.i - alpha*(1/N)*grad.i 
  desc = c()
  for(i in 1:1000){
    desc = c(desc,sqrt(sum((t(y) - (1/N)*sum((-y*log(1/(1+exp(-(beta%*%t(X)))))) - ((1-y)*log(1-1/(1+exp(-(beta%*%t(X))))))) )^2)))
    e = t(y) - 1/(1+exp(-(beta%*%t(X))))
    grad = -(2/N)%*%e%*%X
    beta = beta - alpha*(2/N)*grad
  }
  print(sqrt(sum(grad^2)))
  values<-list("coefficients" = t(beta), "descent" = desc)
  return(values)
}

par(mfrow = c(2,2))


# Reading the data and assigning them for train and test sets both for linear and logistic regressions
data <- fread("OnlineNewsPopularity.csv")
data <- data[,-c(1,2)]

normlin <- as.data.frame(lapply(data, mean_normalize))
normlog <- cbind(as.data.frame(lapply(data[,1:58], mean_normalize)),logistic = ifelse(data$shares < median(data$shares),0,1))

set.seed(12345)
sample <- sample.int(n = nrow(data), size = floor(.75*nrow(data)), replace = F)
trainlin <- normlin[sample, ]
testlin  <- normlin[-sample,]
trainlog <- normlog[sample, ]
testlog  <- normlog[-sample,]

########################################################################

#As per the question picking 10 random variables to run linear and logistic regresisons


#Fixed Iterations
gdec1 <- algo_linear(y = trainlin$shares,X = trainlin[,c(1,2,10,11,12,37,43,49,55,56)],c.tres = .001,alpha = .1,iters = 10000)
pred <- as.data.frame((gdec1$coefficients[1,]+as.matrix(testlin[,c(1,2,10,11,12,37,43,49,55,56)])%*%gdec1$coefficients[2:11,]))
#RMSE(testlin$shares,pred)
plot(1:length(gdec1$descent),gdec1$descent,xlab = "iterations", ylab = "loss")
title(main = paste("Trila 1 RMSE: ",round(RMSE(testlin$shares,pred),2)))
lines(1:length(gdec1$descent),gdec1$descent)

gdec2 <- algo_linear(y = trainlin$shares,X = trainlin[,c(1,2,10,11,12,37,43,49,55,56)],c.tres = .001,alpha = 1,iters = 10000)
pred <- as.data.frame((gdec2$coefficients[1,]+as.matrix(testlin[,c(1,2,10,11,12,37,43,49,55,56)])%*%gdec2$coefficients[2:11,]))
#RMSE(testlin$shares,pred)
plot(1:length(gdec2$descent),gdec2$descent,xlab = "iterations", ylab = "loss")
title(main = paste("Trial 2 RMSE: ",round(RMSE(testlin$shares,pred),2)))
lines(1:length(gdec2$descent),gdec2$descent)

gdec3 <- algo_linear(y = trainlin$shares,X = trainlin[,c(1,2,10,11,12,37,43,49,55,56)],c.tres = .001,alpha = 10,iters = 10000)
pred <- as.data.frame((gdec3$coefficients[1,]+as.matrix(testlin[,c(1,2,10,11,12,37,43,49,55,56)])%*%gdec3$coefficients[2:11,]))
#RMSE(testlin$shares,pred)
plot(1:length(gdec3$descent),gdec3$descent,xlab = "iterations", ylab = "loss")
title(main = paste("Trial 3 RMSE: ",round(RMSE(testlin$shares,pred),2)))
lines(1:length(gdec3$descent),gdec3$descent)

gdec4 <- algo_linear(y = trainlin$shares,X = trainlin[,c(1,2,10,11,12,37,43,49,55,56)],c.tres = .001,alpha = 100,iters = 10000)
pred <- as.data.frame((gdec4$coefficients[1,]+as.matrix(testlin[,c(1,2,10,11,12,37,43,49,55,56)])%*%gdec4$coefficients[2:11,]))
#RMSE(testlin$shares,pred)
plot(1:length(gdec4$descent),gdec4$descent,xlab = "iterations", ylab = "loss")
title(main = paste("Trial 4 RMSE: ",round(RMSE(testlin$shares,pred),2)))
lines(1:length(gdec4$descent),gdec4$descent)


#Fixed Alpha

gdec5 <- algo_linear(y = trainlin$shares,X = trainlin[,c(1,2,10,11,12,37,43,49,55,56)],c.tres = .001,alpha = .1,iters = 100)
pred <- as.data.frame((gdec5$coefficients[1,]+as.matrix(testlin[,c(1,2,10,11,12,37,43,49,55,56)])%*%gdec5$coefficients[2:11,]))
#RMSE(testlin$shares,pred)
plot(1:length(gdec5$descent),gdec5$descent,xlab = "iterations", ylab = "loss")
title(main = paste("Trila 1 RMSE: ",round(RMSE(testlin$shares,pred),2)))
lines(1:length(gdec5$descent),gdec5$descent)

gdec6 <- algo_linear(y = trainlin$shares,X = trainlin[,c(1,2,10,11,12,37,43,49,55,56)],c.tres = .001,alpha = .1,iters = 1000)
pred <- as.data.frame((gdec6$coefficients[1,]+as.matrix(testlin[,c(1,2,10,11,12,37,43,49,55,56)])%*%gdec6$coefficients[2:11,]))
#RMSE(testlin$shares,pred)
plot(1:length(gdec6$descent),gdec6$descent,xlab = "iterations", ylab = "loss")
title(main = paste("Trial 2 RMSE: ",round(RMSE(testlin$shares,pred),2)))
lines(1:length(gdec6$descent),gdec6$descent)

gdec7 <- algo_linear(y = trainlin$shares,X = trainlin[,c(1,2,10,11,12,37,43,49,55,56)],c.tres = .001,alpha = .1,iters = 10000)
pred <- as.data.frame((gdec7$coefficients[1,]+as.matrix(testlin[,c(1,2,10,11,12,37,43,49,55,56)])%*%gdec7$coefficients[2:11,]))
#RMSE(testlin$shares,pred)
plot(1:length(gdec7$descent),gdec7$descent,xlab = "iterations", ylab = "loss")
title(main = paste("Trial 3 RMSE: ",round(RMSE(testlin$shares,pred),2)))
lines(1:length(gdec7$descent),gdec7$descent)

gdec8 <- algo_linear(y = trainlin$shares,X = trainlin[,c(1,2,10,11,12,37,43,49,55,56)],c.tres = .001,alpha = .1,iters = 100000)
pred <- as.data.frame((gdec8$coefficients[1,]+as.matrix(testlin[,c(1,2,10,11,12,37,43,49,55,56)])%*%gdec8$coefficients[2:11,]))
#RMSE(testlin$shares,pred)
plot(1:length(gdec8$descent),gdec8$descent,xlab = "iterations", ylab = "loss")
title(main = paste("Trial 4 RMSE: ",round(RMSE(testlin$shares,pred),2)))
lines(1:length(gdec8$descent),gdec8$descent)


#Running the code for best 10 model's:

#Fixed Iterations
gdec9 <- algo_linear(y = trainlin$shares,X = trainlin[,c(1,2,6,7,10,12,13,27,28,29,43)],c.tres = .001,alpha = .1,iters = 10000)
pred <- as.data.frame((gdec9$coefficients[1,]+as.matrix(testlin[,c(1,2,6,7,10,12,13,27,28,29,43)])%*%gdec9$coefficients[2:11,]))
#RMSE(testlin$shares,pred)
plot(1:length(gdec9$descent),gdec9$descent,xlab = "iterations", ylab = "loss")
title(main = paste("Trila 9 RMSE: ",round(RMSE(testlin$shares,pred),2)))
lines(1:length(gdec9$descent),gdec9$descent)

gdec10 <- algo_linear(y = trainlin$shares,X = trainlin[,c(1,2,6,7,10,12,13,27,28,29,43)],c.tres = .001,alpha = 1,iters = 10000)
pred <- as.data.frame((gdec10$coefficients[1,]+as.matrix(testlin[,c(1,2,6,7,10,12,13,27,28,29,43)])%*%gdec10$coefficients[2:11,]))
#RMSE(testlin$shares,pred)
plot(1:length(gdec10$descent),gdec10$descent,xlab = "iterations", ylab = "loss")
title(main = paste("Trial 10 RMSE: ",round(RMSE(testlin$shares,pred),2)))
lines(1:length(gdec10$descent),gdec10$descent)

gdec11 <- algo_linear(y = trainlin$shares,X = trainlin[,c(1,2,6,7,10,12,13,27,28,29,43)],c.tres = .001,alpha = 10,iters = 10000)
pred <- as.data.frame((gdec11$coefficients[1,]+as.matrix(testlin[,c(1,2,6,7,10,12,13,27,28,29,43)])%*%gdec11$coefficients[2:11,]))
#RMSE(testlin$shares,pred)
plot(1:length(gdec11$descent),gdec11$descent,xlab = "iterations", ylab = "loss")
title(main = paste("Trial 11 RMSE: ",round(RMSE(testlin$shares,pred),2)))
lines(1:length(gdec11$descent),gdec11$descent)

gdec12 <- algo_linear(y = trainlin$shares,X = trainlin[,c(1,2,6,7,10,12,13,27,28,29,43)],c.tres = .001,alpha = 100,iters = 10000)
pred <- as.data.frame((gdec12$coefficients[1,]+as.matrix(testlin[,c(1,2,6,7,10,12,13,27,28,29,43)])%*%gdec12$coefficients[2:11,]))
#RMSE(testlin$shares,pred)
plot(1:length(gdec12$descent),gdec12$descent,xlab = "iterations", ylab = "loss")
title(main = paste("Trial 12 RMSE: ",round(RMSE(testlin$shares,pred),2)))
lines(1:length(gdec12$descent),gdec12$descent)


#Fixed Alpha

gdec13 <- algo_linear(y = trainlin$shares,X = trainlin[,c(1,2,6,7,10,12,13,27,28,29,43)],c.tres = .001,alpha = .1,iters = 100)
pred <- as.data.frame((gdec13$coefficients[1,]+as.matrix(testlin[,c(1,2,6,7,10,12,13,27,28,29,43)])%*%gdec13$coefficients[2:11,]))
#RMSE(testlin$shares,pred)
plot(1:length(gdec13$descent),gdec13$descent,xlab = "iterations", ylab = "loss")
title(main = paste("Trila 13 RMSE: ",round(RMSE(testlin$shares,pred),2)))
lines(1:length(gdec13$descent),gdec13$descent)

gdec14 <- algo_linear(y = trainlin$shares,X = trainlin[,c(1,2,6,7,10,12,13,27,28,29,43)],c.tres = .001,alpha = .1,iters = 1000)
pred <- as.data.frame((gdec14$coefficients[1,]+as.matrix(testlin[,c(1,2,6,7,10,12,13,27,28,29,43)])%*%gdec14$coefficients[2:11,]))
#RMSE(testlin$shares,pred)
plot(1:length(gdec14$descent),gdec14$descent,xlab = "iterations", ylab = "loss")
title(main = paste("Trial 14 RMSE: ",round(RMSE(testlin$shares,pred),2)))
lines(1:length(gdec14$descent),gdec14$descent)

gdec15 <- algo_linear(y = trainlin$shares,X = trainlin[,c(1,2,6,7,10,12,13,27,28,29,43)],c.tres = .001,alpha = .1,iters = 10000)
pred <- as.data.frame((gdec15$coefficients[1,]+as.matrix(testlin[,c(1,2,6,7,10,12,13,27,28,29,43)])%*%gdec15$coefficients[2:11,]))
#RMSE(testlin$shares,pred)
plot(1:length(gdec15$descent),gdec15$descent,xlab = "iterations", ylab = "loss")
title(main = paste("Trial 15 RMSE: ",round(RMSE(testlin$shares,pred),2)))
lines(1:length(gdec15$descent),gdec15$descent)

gdec16 <- algo_linear(y = trainlin$shares,X = trainlin[,c(1,2,6,7,10,12,13,27,28,29,43)],c.tres = .001,alpha = .1,iters = 100000)
pred <- as.data.frame((gdec16$coefficients[1,]+as.matrix(testlin[,c(1,2,6,7,10,12,13,27,28,29,43)])%*%gdec16$coefficients[2:11,]))
#RMSE(testlin$shares,pred)
plot(1:length(gdec16$descent),gdec16$descent,xlab = "iterations", ylab = "loss")
title(main = paste("Trial 16 RMSE: ",round(RMSE(testlin$shares,pred),2)))
lines(1:length(gdec16$descent),gdec16$descent)

