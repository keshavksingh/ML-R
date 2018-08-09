library("e1071")
library(ggvis)
library(class)
library(gmodels)
library(neuralnet)
library(nnet)



setwd("D:/Training/TraningMaterial")
wine_full <- read.csv("WineDataWithResults.csv")
head (wine_full)
plot(wine_full)

winefulldf <- cbind(wine_full[,2:14],class.ind(wine_full$WineVal))
head(winefulldf)

set.seed(2)
proportion <- 0.75 # Set to Split
index <- sample(1:nrow(winefulldf), round(proportion*nrow(winefulldf)))
train_cv <- winefulldf[index, ]
test_cv <- winefulldf[-index, ]
head(train_cv)
head(test_cv)
n<-neuralnet(EveningLand + PeterMicheal + QuilcedaCreek ~ Alcohol
          +MalicAcid+Ash+AlcalinityOfAsh+Magnesium+TotalPhenols+Flavanoids+
            NonflavanoidPhenols+Proanthocyanins+ColorIntensity+Hue+OD280.OD315.OfDilutedWines
          +Proline
          , train_cv ,hidden = c(8))

plot(n)
head(test_cv)
pr.nnn<-compute(n,test_cv[,1:13])
pr.nnn_ <- pr.nnn$net.result
pr.nnn_<-as.data.frame(pr.nnn_)
colnames(pr.nnn_)<-c("EveningLand","PeterMicheal","QuilcedaCreek")
head(pr.nnn_)
result <- colnames(pr.nnn_)[apply(pr.nnn_,1,which.max)]
result<- as.data.frame(result)
testdataset<-colnames(test_cv[,14:16])[apply(test_cv[,14:16],1,which.max)]
testdataset<-as.data.frame(testdataset)
table(result$result,testdataset$testdataset)
############################################################################################
##                         IRIS DataSet
############################################################################################
iris<-read.csv("D:/Training/TraningMaterial/IRIS.csv",header = TRUE)
layer_points(ggvis(iris, x = ~PetalLengthCm, y = ~PetalWidthCm, fill = ~Species ))##
head(iris)
irisdf<-cbind(iris[,2:5],class.ind(iris$Species))
head(irisdf)

set.seed(2)
proportion <- 0.80 # Set to Split
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

######################################################################################
####                     TITANIC Dataset
#######################################################################################
titanic<-read.csv("D:/Training/TraningMaterial/Titanic.csv",header = TRUE)
head(titanic)
titanicdf<-cbind(titanic[,3:5],class.ind(titanic$Class),class.ind(titanic$Gender),class.ind(titanic$EmbarkedFrom),class.ind(titanic$Survival))
titanicdf<-as.data.frame(titanicdf)
head(titanicdf)

set.seed(2)
proportion <- 0.75 # Set to Split
index <- sample(1:nrow(titanicdf), round(proportion*nrow(titanicdf)))
train_titanic <- titanicdf[index, ]
test_titanic <- titanicdf[-index, ]
head(train_titanic)
head(test_titanic)

NROW(train_titanic)
NROW(test_titanic)

titanic_n<-neuralnet(NotSurvived + Survived ~ Age+NoOfSiblings+NoOfParents+Class1+Class2+Class3+female+male+
                       EmbarkedFromC+EmbarkedFromQ+EmbarkedFromS
                  , train_titanic ,hidden = c(9))
plot(titanic_n)
head(test_titanic)
pred_test<-compute(titanic_n,test_titanic[,1:11])

predtestResult <- pred_test$net.result
predtestResult<-as.data.frame(predtestResult)

colnames(predtestResult)<-c("NotSurvived","Survived")
head(predtestResult)
result_titanic <- colnames(predtestResult)[apply(predtestResult,1,which.max)]
result_titanic<- as.data.frame(result_titanic)
titanic_testdataset<-colnames(test_titanic[,12:13])[apply(test_titanic[,12:13],1,which.max)]
titanic_testdataset<-as.data.frame(titanic_testdataset)
table(result_titanic$result_titanic,titanic_testdataset$titanic_testdataset)

##(55+33)/106 83%
##(54+88)/176 80%

#############################################################################################