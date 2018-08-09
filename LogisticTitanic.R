setwd("D:/Personal/Titanic")
df<-read.csv("train.csv",header = TRUE)
head(df)
dftr<-subset(df,select = -c(Ticket,Cabin,PassengerId,Name))
head(dftr)
plot(dftr)

modellog<-glm(formula = Survived~factor(Pclass)+factor(Sex)+Age+factor(SibSp)
              +factor(Parch)+Fare+factor(Embarked),family = binomial(link = "logit") ,data = dftr)

summary(modellog)

anova(modellog, test="Chisq")