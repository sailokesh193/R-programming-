#Titanic 
getwd()
data<-read.csv(file.choose())
head(data)
str(data)
summary(data)
data[!complete.cases(data),]
nrow(data[!complete.cases(data),])
View(data[!complete.cases(data),])
lol<-median(data$Age,na.rm = T)
data[is.na(data$Age)&data$Sex=="male","Age"]<-28
lol1<-mean(data$Age,na.rm = T) 
data[is.na(data$Age)&data$Sex=="female","Age"]<-lol1
summary(data)
View(data)

#identifying outliers
quantile(data$Age,0.995)
data$Age<-ifelse(data$Age>70.275,70.275,data$Age)
summary(data)
data$Age<-ifelse(data$Age<10,28,data$Age)
library(ggplot2)
ggplot()+geom_histogram(data=data,aes(x=Survived,colour=Sex,fill=Age))

#dummy variables for p class
lol<-table(data$Pclass,data$Survived)
myplot<-function(n)
{
  data<-lol[1:n,1,drop=F]
  data1<-lol[1:n,2,drop=F]
  sum=(data+data1)
  per=(data1/sum)*100
  per
}
myplot(3)
#dummy variables
data$dummy_Pclass_1<-ifelse(data$Pclass=="1",1,0)
data$dummy_Pclass_2<-ifelse(data$Pclass=="2",1,0)

#dummy variables for gender
lol1<-table(data$Sex,data$Survived)
myplot1<-function(n)
{
  data<-lol[1:n,1,drop=F]
  data1<-lol[1:n,2,drop=F]
  sum=(data+data1)
  per=(data1/sum)*100
  per
}
myplot1(2)
data$dummy_male<-ifelse(data$Sex=="male",1,0)

lol2<-table(data$SibSp,data$Survived)
#dummy variables for Sibsp
myplot2<-function(n)
{
  data<-lol2[1:n,1,drop=F]
  data1<-lol2[1:n,2,drop=F]
  sum=data+data1
  per=(data1/sum)*100
  per
}
myplot2(7)
data$dummy_sibsp_high<-ifelse(data$SibSp=="0"|data$SibSp=="1"|data$SibSp=="2",1,0)
data$dummy_sibsp_low<-ifelse(data$SibSp=="5"|data$SibSp=="8",0,1)

#dummy variables for Age
table(data$Age,data$Survived)
data$dummy_Age_high<-ifelse(data$Age>29,1,0)

#dummy variables for Parch
lol3<-table(data$Parch,data$Survived)
myplot3<-function(n)
{
  data<-lol3[1:n,1,drop=F]
  data1<-lol3[1:n,2,drop=F]
  sum=data+data1
  per=(data1/sum)*100
  per
}
myplot3(6)
data$dummy_parch_high<-ifelse(data$Parch=="3"|data$Parch=="1"|data$Parch=="2",1,0)
data$dummy_parch_low<-ifelse(data$Parch=="0"|data$Parch=="5",0,1)
data$dummy_parch_zero<-ifelse(data$Parch=="4",1,0)


#dummy variables for fare
lol6<-table(data$Fare,data$Survived)
nrow(lol6<-table(data$Fare,data$Survived))
myplot6<-function(n)
{
  data<-lol6[1:n,1,drop=F]
  data1<-lol6[1:n,2,drop=F]
  sum=data+data1
  per=(data1/sum)*100
  per
}
myplot6(248)
data$dummy_fare_high<-ifelse(data$Fare> 26.2833,1,0)

#dummy variables for Embarked
lol7<-table(data$Embarked,data$Survived)
myplot7<-function(n)
{
  data<-lol7[1:n,1,drop=F]
  data1<-lol7[1:n,2,drop=F]
  sum=data+data1
  per=(data1/sum)*100
  per
}
myplot7(4)
data$dummy_Embarked_low<-ifelse(data$Embarked=="Q"|data$Embarked=="S",0,1)

#multicolinearity check
attach(data)
vif_output<-lm(Survived~dummy_Pclass_1+dummy_Pclass_2+dummy_male+dummy_Age_high+dummy_sibsp_low+dummy_sibsp_high+dummy_fare_high+dummy_Embarked_low,data=data)
library(car)
vif(vif_output)
multicol1<-lm(Survived~dummy_Pclass_1,data=data)
multicol2<-lm(Survived~dummy_Pclass_2,data=data)
summary(multicol1)
summary(multicol2)
vif_output<-lm(Survived~dummy_Pclass_2+dummy_male+dummy_Age_high+dummy_sibsp_low+dummy_sibsp_high+dummy_fare_high+dummy_Embarked_low,data=data)
vif(vif_output)

#logistic regression model building
logreg_ot<-glm(Survived~dummy_Pclass_2+dummy_male+dummy_Age_high+dummy_sibsp_low+dummy_sibsp_high+dummy_fare_high+dummy_Embarked_low,family = binomial(logit),data=data)
summary(logreg_ot,direction="forward")
logreg_ot<-glm(Survived~dummy_Pclass_2+dummy_male+dummy_sibsp_high+dummy_fare_high+dummy_Embarked_low)

#estimating the probability of each record
data$predicted_probability<-predict(logreg_ot,data,type="response")
View(data)
data$final_prediction<-ifelse(data$predicted_probability>0.3,1,0)


#---------------------------test data------------------------------------
testdata<-read.csv(file.choose())
head(testdata)
summary(testdata)
quantile(testdata$Age,0.995)
nrow(testdata[!complete.cases(testdata),])

#data imputaion
med<-median(testdata$Age,na.rm = T)
mea<-mean(testdata$Age,na.rm=T)
testdata[is.na(testdata$Age)&testdata$Sex=="male","Age"]<-med
testdata[is.na(testdata$Age)&testdata$Sex=="female","Age"]<-mea
summary(testdata)
View(testdata)

#capping outliers
quantile(testdata$Age,0.995)
quantile(testdata$Age,0.998)
testdata$age<-ifelse(testdata$Age>68.494,68.494,testdata$Age)
summary(testdata)


#dummy variables for p class
library(car)
lil<-table(testdata$Pclass,testdata$Survived)
myplot<-function(n)
{
  data<-lol[1:n,1,drop=F]
  data1<-lol[1:n,2,drop=F]
  sum=(data+data1)
  per=(data1/sum)*100
  per
}
myplot(3)
#dummy variables
testdata$dummy_Pclass_1<-ifelse(testdata$Pclass=="1",1,0)
testdata$dummy_Pclass_2<-ifelse(testdata$Pclass=="2",1,0)

#dummy variables for gender
lol1<-table(testdata$Sex,testdata$Survived)
myplot1<-function(n)
{
  data<-lol[1:n,1,drop=F]
  data1<-lol[1:n,2,drop=F]
  sum=(data+data1)
  per=(data1/sum)*100
  per
}
myplot1(2)
testdata$dummy_male<-ifelse(testdata$Sex=="male",1,0)

lol2<-table(testdata$SibSp,testdata$Survived)
#dummy variables for Sibsp
myplot2<-function(n)
{
  data<-lol2[1:n,1,drop=F]
  data1<-lol2[1:n,2,drop=F]
  sum=data+data1
  per=(data1/sum)*100
  per
}
myplot2(7)
testdata$dummy_sibsp_high<-ifelse(testdata$SibSp=="0"|testdata$SibSp=="1"|testdata$SibSp=="2",1,0)
testdata$dummy_sibsp_low<-ifelse(testdata$SibSp=="5"|testdata$SibSp=="8",0,1)

#dummy variables for Age
table(testdata$Age,testdata$Survived)
testdata$dummy_Age_high<-ifelse(testdata$Age>29,1,0)

#dummy variables for Parch
lol3<-table(testdata$Parch,testdata$Survived)
myplot3<-function(n)
{
  data<-lol3[1:n,1,drop=F]
  data1<-lol3[1:n,2,drop=F]
  sum=data+data1
  per=(data1/sum)*100
  per
}
myplot3(6)
testdata$dummy_parch_high<-ifelse(testdata$Parch=="3"|testdata$Parch=="1"|testdata$Parch=="2",1,0)
testdata$dummy_parch_low<-ifelse(data$Parch=="0"|testdata$Parch=="5",0,1)
testdata$dummy_parch_zero<-ifelse(testdata$Parch=="4",1,0)


#dummy variables for fare
lol6<-table(testdata$Fare,testdata$Survived)
nrow(lol6<-table(testdata$Fare,testdata$Survived))
myplot6<-function(n)
{
  data<-lol6[1:n,1,drop=F]
  data1<-lol6[1:n,2,drop=F]
  sum=data+data1
  per=(data1/sum)*100
  per
}
myplot6(248)
testdata$dummy_fare_high<-ifelse(testdata$Fare> 26.2833,1,0)

#dummy variables for Embarked
lol7<-table(testdata$Embarked,testdata$Survived)
myplot7<-function(n)
{
  data<-lol7[1:n,1,drop=F]
  data1<-lol7[1:n,2,drop=F]
  sum=data+data1
  per=(data1/sum)*100
  per
}
myplot7(4)
testdata$dummy_Embarked_low<-ifelse(testdata$Embarked=="Q"|testdata$Embarked=="S",0,1)

View(testdata)

#Predicting
testdata$prediction_probability1<-predict(logreg_ot,testdata,type='response')
View(testdata)
testdata$final_prediction<-ifelse(testdata$prediction_probability1>0.3,1,0)
View(testdata[!complete.cases(testdata),])
testdata[is.na(testdata$final_prediction)&testdata$PassengerId==1044,"final_prediction"]<-1
a<-testdata$final_prediction
View(a)
View(a[!complete.cases(a)])
mat<-as.matrix(a)
len<-length(mat)
#no of passengers survived
myplot12<-function()
{
  b<-mat[1:len!=0]
  sum(b)
}
passenger_surv<-myplot12()

#no of passengers not survived
myplot13<-function()
{
  b<-mat[(1:len)!=1]
  sum(b)
}
passenger_not_surv<-myplot13()

