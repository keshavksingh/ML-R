#install.packages("e1071")
#install.packages("ggvis")
#install.packages("class")
#install.packages("gmodels")
#install.packages("neuralnet")
#install.packages("nnet")

library("e1071")
library(ggvis)
library(class)
library(gmodels)
library(neuralnet)
library(nnet)



setwd("C:/Keshav/Engineering/ML")
mnist_train <- read.csv("mnist_train.csv")
head (mnist_train)
pcatrain<-subset(mnist_train,select = -c(Class))

pcatrainT<-pcatrain[,apply(pcatrain, 2, var, na.rm=TRUE) != 0]
head(pcatrainT)

prin_comp<-prcomp(pcatrainT,scale. = T)
names(prin_comp)
biplot(prin_comp,scale = 0)

std_dev <- prin_comp$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)

plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
##################################
train.data <- data.frame(Result = mnist_train$Class, prin_comp$x)
train.data <- train.data[,1:301]
head(train.data)

mnist_test<-read.csv("mnist_test.csv")
pcatest<-subset(mnist_test,select = -c(Class))
test.data <- predict(prin_comp, newdata = pcatest)
test.data <- as.data.frame(test.data)
test.data <- test.data[,1:300]
head(test.data)


################### The SVM Model gave up hence taking this to AML and Solving it via MultiClass NN ####
svm_model <- svm(Result~., data=train.data)
summary(svm_model)

pred <- predict(svm_model,test.data)
table(pred,test$Species)

###################
head(train.data)
head(test.data)
trainNew <- data.frame(Result = mnist_test$Class, test.data)
trainNew<-as.data.frame(trainNew)
head(trainNew)

write.csv(train.data,"AML_MNIST_TRAIN.csv")
write.csv(trainNew,"AML_MNIST_TEST.csv")





