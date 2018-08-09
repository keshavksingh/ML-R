#install.packages("ggvis")
#install.packages("class")
#install.packages("gmodels")
library(ggvis)
library(class)   # required for knn
library(gmodels)
setwd("D:/Training/TraningMaterial")
iris_full <- read.csv("IRIS.csv")
head (gc)

layer_points(ggvis(iris_full, x = ~SepalLengthCm, y = ~SepalWidthCm, fill = ~Species ))
layer_points(ggvis(iris_full, x = ~PetalLengthCm, y = ~PetalWidthCm, fill = ~Species ))

iris_sentosa <- iris_full[which(iris_full$Species == "Iris-setosa"),,]##All records ##All Columns
nrow(iris_sentosa)
iris_virginica <- iris_full[which(iris_full$Species == "Iris-virginica"),,]##All records ##All Columns
nrow(iris_virginica)
iris_versicolor <- iris_full[which(iris_full$Species == "Iris-versicolor"),,]##All records ##All Columns
nrow(iris_versicolor)
## generate a random sample of 70 % records from each bundle i.e. 35 records 
## each from sentosa, virginica and versicolor
## 70 % forms the traning dataset and 30 % test
set.seed(2)##generate random numbers starting from 2
trdata <- sample(1:nrow(iris_sentosa),35)
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


trainFinal <- train[,c("PetalLengthCm","PetalWidthCm")]
train_lablel<-train[,c("Species")]

testFinal <- test[,c("PetalLengthCm","PetalWidthCm")]
test_lablel<-test[,c("Species")]

test_prediction <- knn(train = trainFinal, test = testFinal,cl = train_lablel, k=12) ##SQRT of 105

#######Test Accuracy
table (test_lablel,test_prediction)
CrossTable(x=test_lablel,y=test_prediction,prop.chisq = FALSE)

#########################################
### Test Sepal Accuracy 
#########################################
Sepaltrain <- train[,c("SepalLengthCm","SepalWidthCm")]
Sepaltrain_lablel<-train[,c("Species")]

Sepaltest <- test[,c("SepalLengthCm","SepalWidthCm")]
Sepaltest_lablel<-test[,c("Species")]

sepaltest_prediction <- knn(train = Sepaltrain, test = Sepaltest,cl = Sepaltrain_lablel, k=10) ##SQRT of 105
#######Test Accuracy
table (Sepaltest_lablel,sepaltest_prediction)
CrossTable(x=Sepaltest_lablel,y=sepaltest_prediction,prop.chisq = FALSE)


