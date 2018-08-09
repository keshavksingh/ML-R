library(ROCR)
##install.packages("ROCR")
cbil<-read.csv("D:/Training/TraningMaterial/CBIL.csv",header = TRUE)
cbil
with(cbil,plot(CBIL,Approval))
#### Random Sample the test and train Datasets
set.seed(2)
tr <- sample(1:nrow(cbil),35)
ts<--tr
train <- cbil[tr,]
test <- cbil[ts,]
##############
model <- glm(Approval ~CBIL,family=binomial(link='logit'),data=train)
summary(model)

p <- predict(model, newdata=test, type="response")
pr <- prediction(p, test$Approval)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc