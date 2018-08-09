#install.packages("tree")
library(tree)
iris_full<-read.csv("C:/Keshav/Engineering/ML/Session/IRIS.csv")
head(iris_full)
nrow(iris_full)

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
proportion <- 0.50 # Set to Split
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


treemodel<-tree(Species~.,trainFinal)
plot(treemodel)
text(treemodel,pretty = 0)

pre<-predict(treemodel,testFinal)

colnames(pre)<-c("Setosa","Versicolor","Virginica")
head(pre)
result_IRIS <- colnames(pre)[apply(pre,1,which.max)]
result_IRIS<- as.data.frame(result_IRIS)

table(result_IRIS$result_IRIS,test_lablel)



### Accuracy

misClasificError <- mean(result_IRIS$result_IRIS != test_lablel)
print(paste('Accuracy',1-misClasificError))


