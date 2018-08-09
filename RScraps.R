


reg <- lm(Sales~Advertisement,data=sales) 
summary(reg)

with(sales,plot(Sales, Advertisement))



sales<-read.csv("D:/Training/TraningMaterial/Sales.csv",header = TRUE)
sales
plot(sales,main="Scatterplot Example",xlab="Sales in Million", ylab="Advertisement In thousand", pch=19)
plot(Sales~Advertisement,data=sales)
abline(lm(Sales~Advertisement,data=sales))


salesP<-read.csv("D:/Training/TraningMaterial/SalesPredict.csv",header = TRUE)
salesP
predict(reg,salesP)

155
185
210
165
215
230
cbil<-read.csv("D:/Training/TraningMaterial/CBIL.csv",header = TRUE)
cbil
with(cbil,plot(CBIL,Approval))
train <- cbil[1:45,]
test <- cbil[46:50,]
model <- glm(Approval ~CBIL,family=binomial(link='logit'),data=train)
summary(model)

p <- predict(model, newdata=test, type="response")
pr <- prediction(p, test$Approval)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc



################## Predict
library(ROCR)
install.packages("ROCR")
cbilTest<-read.csv("D:/Training/TraningMaterial/CBILTest.csv",header = TRUE)
cbilTest
p <- predict(model, newdata=cbilTest, type="response")
p
pr <- prediction(p, cbilTest$IsApproved)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc