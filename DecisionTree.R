#install.packages("tree")
library(tree)
dtree<-read.csv("D:/Training/TraningMaterial/DecisionTreeTitanic.csv")
head(dtree)


nrow(dtree)

#### Smaple Data Set

dtr<-sample(1:nrow(dtree),655)
dts<--dtr
dtrain<-dtree[dtr,]
dtest<-dtree[dts,]

nrow(dtrain)
nrow(dtest)

treemodel<-tree(Survived~.,dtrain)
plot(treemodel)
text(treemodel,pretty = 0)

pre<-predict(treemodel,dtest)
pre <- ifelse(pre > 0.5,1,0)
table(pre,dtest$Survived)


### Accuracy =(21+14)/50 = 0.7 or 70%

misClasificError <- mean(pre != dtest$Survived)
print(paste('Accuracy',1-misClasificError))


