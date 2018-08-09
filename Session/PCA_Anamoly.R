#install.packages("ggvis")
#install.packages("class")
#install.packages("gmodels")
library(ggvis)
library(class)   # required for knn
library(gmodels)
library("e1071")
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
proportion <- 0.80 # Set to Split
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
##View(train)
##head(train)
###############################
pcatrain<-subset(train,select = -c(Id,Species))
pcatest<-subset(test,select = -c(Id,Species))

prin_comp<-prcomp(pcatrain,scale. = T)
names(prin_comp)
biplot(prin_comp,scale = 0)

std_dev <- prin_comp$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)

plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")


##################### Predictive Modelling with PCA Picking 11 Principal Components
#### SVM 
train.data <- data.frame(Result = train$Species, prin_comp$x)
train.data <- train.data[,1:4]

test.data <- predict(prin_comp, newdata = pcatest)
test.data <- as.data.frame(test.data)
test.data <- test.data[,1:3]

svm_model <- svm(Result~., data=train.data)


pred <- predict(svm_model,test.data)
table(pred,test$Species)