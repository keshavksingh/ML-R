library("e1071")
library(ggvis)
library(class)   # required for knn
library(gmodels)
setwd("C:/Keshav/Engineering/ML/Session")
iris_full <- read.csv("IRIS.csv")


layer_points(ggvis(iris_full, x = ~SepalLengthCm, y = ~SepalWidthCm, fill = ~Species ))
layer_points(ggvis(iris_full, x = ~PetalLengthCm, y = ~PetalWidthCm, fill = ~Species ))

iris_sentosa <- iris_full[which(iris_full$Species == "Setosa"),,]##All records ##All Columns
nrow(iris_sentosa)
iris_virginica <- iris_full[which(iris_full$Species == "Virginica"),,]##All records ##All Columns
nrow(iris_virginica)
iris_versicolor <- iris_full[which(iris_full$Species == "Versicolor"),,]##All records ##All Columns
nrow(iris_versicolor)
## generate a random sample of 70 % records from each bundle i.e. 35 records 
## each from sentosa, virginica and versicolor
## 70 % forms the traning dataset and 30 % test
set.seed(2)##generate random numbers starting from 2
proportion <- 0.70 # Set to Split
trdata <- sample(1:nrow(iris_sentosa),round(proportion*nrow(iris_sentosa)))
tstdata <- -trdata
train_sentosa <- iris_sentosa[trdata,]
test_sentosa <- iris_sentosa[tstdata,]

train_virginica <- iris_virginica[trdata,]
test_virginica <- iris_virginica[tstdata,]

train_versicolor <- iris_versicolor[trdata,]
test_versicolor <- iris_versicolor[tstdata,]

##### Sampling comlete 
##### Create the final train and test datasets
train <- rbind(train_sentosa,train_versicolor,train_virginica) ## use rbind to combine training datasets
test <- rbind(test_sentosa,test_versicolor,test_virginica) ## use rbind to combine training datasets


trainFinal <- train[,c("PetalLengthCm","PetalWidthCm","Species")]
#train_lablel<-train[,c("Species")]

testFinal <- test[,c("PetalLengthCm","PetalWidthCm")]
test_lablel<-test[,c("Species")]



svm_model <- svm(Species ~ ., data=trainFinal)

#######Test Accuracy

pred <- predict(svm_model,testFinal)
table(pred,test_lablel)

###################################################################
##install.packages("AzureML")
##install.packages("devtools")
##.rs.restartR()

PredictSpecies <-function(testFinal)
{
  
  predict(svm_model,testFinal,type="response")
}

print(PredictSpecies(testFinal))

version

library("AzureML")
library("devtools")


ws <- workspace(
  id = "eae3b20510ce461cabff35baac401b81",
  auth = "3ODmMrOULK6XsYPL39YOHcpGwL961frFbnVNWj0FSuCe7q8RBfP14gpE069NUJUV2YLNGADEY/g/P6/loyegRA==",
  api_endpoint = "https://studioapi.azureml.net"
)

PredictIRISWebService <-publishWebService(ws,
                                          fun = PredictIRISWebService,
                                          name = PredictIRISWebService,
                                          inputSchema = testFinal)

head(PredictIRISWebService)

