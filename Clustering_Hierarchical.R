#install.packages("ggvis")
#install.packages("class")
#install.packages("gmodels")
library(ggvis)
library(class)   # required for knn
library(gmodels)
library("e1071")
library("ggplot2")
setwd("D:/Training/TraningMaterial")
wine_full <- read.csv("WineDataWithResults.csv")
head (wine_full)
plot(wine_full)
clusters <- hclust(dist(wine_full))
plot(clusters)

clusterCut <- cutree(clusters, 3)

table(clusterCut, wine_full$WineVal)


clusters <- hclust(dist(wine_full[,2:14]), method = 'average')
plot(clusters)

clusterCut <- cutree(clusters, 3)
predicted<-data.frame(clusterCut)
table(clusterCut, wine_full$WineVal)
#############################################

set.seed(20)
kmeans_clust <- kmeans(wine_full[,2:14], 3, nstart = 20)
table(kmeans_clust$cluster, wine_full$WineVal)

kmeans_clust$cluster <- as.factor(kmeans_clust$cluster)
ggplot(wine_full, aes(TotalPhenols, Flavanoids, color = kmeans_clust$cluster)) + geom_point()

################ Using a relevant dataset IRIS ###############

IRIS_FULL <-read.csv("IRIS.csv")
head(IRIS_FULL)
layer_points(ggvis(IRIS_FULL, x = ~PetalLengthCm, y = ~PetalWidthCm, fill = ~Species ))


####### Trying Hierarchical Clustering
clusters <- hclust(dist(IRIS_FULL[,4:5]), method = 'average')
plot(clusters)

clusterCut <- cutree(clusters, 3)
table(clusterCut, IRIS_FULL$Species)

###### Trying kMeans Clustering
set.seed(20)
kmeans_clust <- kmeans(IRIS_FULL[,4:5], 3, nstart = 20)
table(kmeans_clust$cluster, IRIS_FULL$Species)







