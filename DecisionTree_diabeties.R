#-------------------diabetes data-------using decision tree-----------------------------
Diabetes<-read.csv(file.choose())
head(Diabetes)
View(Diabetes)
nrow(Diabetes)

#sampling and dividing the data into traindata and testdata
set.seed(123)
Diabetes$ind<-sample(2,nrow(Diabetes),replace=TRUE,prob=c(0.7,0.3))
traindata<-Diabetes[(Diabetes$ind==1),]
View(traindata)
testdata<-Diabetes[Diabetes$ind==2,]
View(testdata)
nrow(traindata)
nrow(testdata)
head(traindata)

library(rpart)
head(traindata)
dt<-rpart(Class.variable~Number.of.times.pregnant+Plasma.glucose.concentration+Diastolic.blood.pressure+Triceps.skin.fold.thickness+Body.mass.index,data=traindata,control=rpart.control(minsplit = 2))
attach(traindata)
Class.variable
plot(dt)
text(dt)
plot(as.party(dt))
dt

str(traindata)

#predit
pred<-predict(dt,testdata,type = c("class"))
pred
cbind(as.character(testdata$Class.variable,as.character(pred)))
t=table(testdata$Class.variable,pred)
sum(diag(t))/sum(t)

#predict prob
pred1<-predict(dt,testdata,type=c("prob"))
t1=table(as.character(testdata$Class.variable),as.character(pred1))
attributes(dt)
dt$variable.importance

#new prediction..
new<-read.csv(file.choose())
View(new)
predict(dt,new,type=c("class"))
predict(dt,new,type=c("prob"))