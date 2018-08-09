p<-read.csv("D:/Training/TraningMaterial/poisson_sim.csv")
######## Sample data into Test and Train
set.seed(2)
tr<-sample(1:nrow(p),180)
ts<--tr

train<-p[tr,]
test<-p[ts,]

nrow(train)
nrow(test)

#########################################
poisson_model <- glm(num_awards~math+factor(prog),data = train, family = "poisson")
summary(poisson_model)

head(test)
test_predict <-subset(test, select = -c(id,num_awards))
result <-predict(poisson_model,test_predict)
result_t<-exp(result)
test$num_awards


##################### Eliminating 0s
q<-p[which(p$num_awards !=0),,]
nrow(q)

set.seed(2)
tr<-sample(1:nrow(q),60)
ts<--tr

train<-q[tr,]
test<-q[ts,]

nrow(train)
nrow(test)

zp_model<-glm(num_awards~math+factor(prog),data = train, family = "poisson")
summary(zp_model)

test_predict <-subset(test, select = -c(id,num_awards))
result <-predict(zp_model,test_predict)
result_t<-exp(result)
