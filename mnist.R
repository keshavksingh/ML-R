library("e1071")
library(ggvis)
library(class)
library(gmodels)
library(neuralnet)
library(nnet)
#install.packages("randomForest")
#install.packages("MASS")
library(MASS)
library(randomForest)
library(tree)



mnist<-read.csv('D:/Training/TraningMaterial/MNIST/mnist_train_step1.csv',header = TRUE)
head(mnist)
##mnist_train<-subset(mnist, select = -c(Id))
##head(mnist_train)
model<-randomForest(Class ~ . , data = mnist)
summary(model)
mnistTest<-read.csv('D:/Training/TraningMaterial/MNIST/mnist_test_step1.csv',header = TRUE)
pre<-predict(model,mnistTest)
table(pre,mnistTest$Class)

misClasificError <- mean(pre != mnistTest$Class)
print(paste('Accuracy',1-misClasificError))
##97%
###############################################################################################
modelTree<-tree(Class ~ . , data = mnist)
summary(modelTree)

mnistTreeTest<-read.csv('D:/Training/TraningMaterial/MNIST/mnist_test_step1.csv',header = TRUE)
head(mnistTreeTest)
testdf<-subset(mnistTreeTest[,2:785])
head(testdf)

preTree<-predict(modelTree,testdf)
#### The tree predicts the probabilty for each class
#### we need to pick the class with highest probability as the predicted value. 
preTree <-as.data.frame(preTree)
preTree$MaxVal <- colnames(preTree)[apply(preTree,1,which.max)]
head(preTree)

table(preTree$MaxVal,mnistTreeTest$Class)

misClasificError <- mean(preTree$MaxVal != mnistTest$Class)
print(paste('Accuracy',1-misClasificError))
##63.48%

##############################################################################################

svmModel<-svm(Class~.,data = mnist)
summary(svmModel)

preSVM<-predict(svmModel,testdf)




dataset<-df[,2:10]
head(dataset)
nrow(dataset)

mo<-lm(data = dataset, SalesUnit~.)
summary(mo)



set.seed(2)
proportion <- 0.75 # Set to Split
index <- sample(1:nrow(dataset), round(proportion*nrow(dataset)))
train_tree <- dataset[index, ]
test_tree <- dataset[-index, ]
head(train_tree)
head(test_tree)

nrow(train_tree)
nrow(test_tree)

treemodel<-tree(SalesUnit~.,train_tree)
plot(treemodel)
text(treemodel,pretty = 0)

pre<-predict(treemodel,test_tree)
pre <- ifelse(pre > 0.5,1,0)
table(pre,dtest$Survived)


### Accuracy =(21+14)/50 = 0.7 or 70%

misClasificError <- mean(pre != dtest$Survived)
print(paste('Accuracy',1-misClasificError))







dd<-cbind(df[,],df$SalesUnitThisWeek,df$ReturnsUnitNextWeek)
head(dd)