library("e1071")
library(ggvis)
library(class)
library(gmodels)
library(neuralnet)
library(nnet)



setwd("C:/Keshav/Engineering/ML/Session")
############################################################################################
##                         IRIS DataSet
############################################################################################
iris<-read.csv("IRIS.csv",header = TRUE)
layer_points(ggvis(iris, x = ~PetalLengthCm, y = ~PetalWidthCm, fill = ~Species ))##
head(iris)
irisdf<-cbind(iris[,2:5],class.ind(iris$Species))
head(irisdf)

set.seed(2)
proportion <- 0.70 # Set to Split
index <- sample(1:nrow(irisdf), round(proportion*nrow(irisdf)))
train_iris <- irisdf[index, ]
test_iris <- irisdf[-index, ]
head(train_iris)
head(test_iris)

NROW(train_iris)
NROW(test_iris)

iris_n<-neuralnet(Setosa + Versicolor + Virginica ~ SepalLengthCm
             +SepalWidthCm+PetalLengthCm+PetalWidthCm
             , train_iris ,hidden = c(3))
plot(iris_n)
head(test_iris)
pred_test<-compute(iris_n,test_iris[,1:4])

predtestResult <- pred_test$net.result
predtestResult<-as.data.frame(predtestResult)

colnames(predtestResult)<-c("Setosa","Versicolor","Virginica")
head(predtestResult)
result_IRIS <- colnames(predtestResult)[apply(predtestResult,1,which.max)]
result_IRIS<- as.data.frame(result_IRIS)
IRIS_testdataset<-colnames(test_iris[,5:7])[apply(test_iris[,5:7],1,which.max)]
IRIS_testdataset<-as.data.frame(IRIS_testdataset)
table(result_IRIS$result_IRIS,IRIS_testdataset$IRIS_testdataset)

