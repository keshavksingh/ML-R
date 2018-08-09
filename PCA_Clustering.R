#install.packages("ggvis")
#install.packages("class")
#install.packages("gmodels")
library(ggvis)
library(class)   # required for knn
library(gmodels)
library("e1071")
setwd("D:/Training/TraningMaterial")
wine_full <- read.csv("WineData.csv")
head (wine_full)
plot(wine_full)

###############################
prin_comp<-prcomp(wine_full,scale. = T)
names(prin_comp)
biplot(prin_comp,scale = 0)

std_dev <- prin_comp$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)

plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

## With 7 to 8 PC will be able to 90% +  of variance
##################### Predictive Modelling with PCA Picking 8 Principal Components
wine_WithResuts<-read.csv("WineDataWithResults.csv")
head(wine_WithResuts)
nrow(wine_WithResuts)##178
train.data<-data.frame(Result=wine_WithResuts$WineVal,prin_comp$x)
head(train.data)
train.data <- train.data[,1:9]
head(train.data)

proportion <- 0.50 
index <- sample(1:nrow(train.data), round(proportion*nrow(train.data)))

train_wine <- train.data[index, ]
test_wine <- train.data[-index, ]
nrow(train_wine)
head(train_wine)
nrow(test_wine)
head(test_wine)

svm_model <- svm(Result~., data=train_wine)
summary(svm_model)

pred <- predict(svm_model,test_wine[,2:9])
table(pred,test_wine[,1])
#######
clusters <- hclust(dist(prin_comp$x))
plot(clusters)


clusterCut <- cutree(clusters, 3)
table(clusterCut, wine_WithResuts$WineVal)

##### H Cluster average method
clustersAvg <- hclust(dist(prin_comp$x), method = 'average')##f "average", "centroid" or "median"
plot(clustersAvg)

clusterCutAvg <- cutree(clusters, 3)
predicted<-data.frame(clusterCutAvg)
table(clusterCutAvg, wine_WithResuts$WineVal)