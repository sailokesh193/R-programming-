#logistic..
setwd("F:/R")
data<-read.csv(file.choose())
nrow(data)
head(data,10)
nrow(data[!complete.cases(data),])
dim(data)
ggplot()+geom_point(data=data,aes(x=satisfaction_level,y=left,colour=salary))
install.packages(ggplot2)
library(ggplot2)
ggplot()+geom_point(data=data,aes(x=promotion_last_5years,y=satisfaction_level,colour=left))
ggplot()+geom_point(data=data,aes(x=promotion_last_5years,y=salary,colour=left))


#-------------------------------------------------using logistic regression-------------------------------------------------
str(data)
summary(data)
data$promotion<-factor(data$promotion_last_5years)
data[!complete.cases(data)]

attach(data)
table(left,satisfaction_level)
data$dummy_satisfaction<-ifelse(satisfaction_level>0.4,1,0)

data$dummy_last<-ifelse(last_evaluation>0.45,1,0)

lol<-table(number_project,left)
myplot<-function(n)
{
  data<-lol[1:n,1,drop=F]
  data1<-lol[1:n,2,drop=F]
  sum=data+data1
  per=(data1/sum)*100
  per
}
myplot(6)

data$dummy_project_l<-ifelse(data$number_project=="3"|data$number_project=="4"|data$number_project=="5",1,0)

lol1<-table(time_spend_company,left)
myplot1<-function(n)
{
  data<-lol1[1:n,1,drop=F]
  data1<-lol1[1:n,2,drop=F]
  sum=data+data1
  per=(data1/sum)*100
  per
}
myplot1(8)

data$dummy_time_l<-ifelse(data$time_spend_company=="2"|data$time_spend_company=="7"|data$time_spend_company=="8"|data$time_spend_company=="10",1,0)

table(Work_accident,left)
data$dummy_work_accident<-ifelse(data$Work_accident=="0",1,0)

table(promotion_last_5years,left)
data$dummy_promotion_l<-ifelse(data$promotion_last_5years=="0",1,0)

lol2<-table(salary,left)
myplot2<-function(n)
{
  data<-lol2[1:n,1,drop=F]
  data1<-lol2[1:n,2,drop=F]
  sum=data+data1
  per=(data1/sum)*100
  per
}
myplot2(3)
data$dummy_salary_h<-ifelse(data$salary=="high",1,0)

#--------------------multicollinearity check-----------------------------
library(car)
vif_output<-lm(left~dummy_satisfaction+dummy_last+dummy_project_l+dummy_time_l+dummy_work_accident+dummy_promotion_l+dummy_salary_h,data=data)
vif(vif_output)

#-----------------------------logistic regression model building-------------------------------------------
logreg<-glm(left~dummy_satisfaction+dummy_last+dummy_project_l+dummy_time_l+dummy_work_accident+dummy_promotion_l+dummy_salary_h,family=binomial(logit),data=data)
summary(logreg,direction="forward")

pre<-predict(logreg,data)
predi<-ifelse(pre>0.3,1,0)
t=table(left,predi)
sum(diag(t))/sum(t)

#https://www.kaggle.com/haijie/decision-tree/data

#------------------------------------------------random forest-------------------------------------------------------
data<-read.csv(file.choose())
head(data)
library(randomForest)
rf<-randomForest(left~satisfaction_level+last_evaluation+number_project+time_spend_company+Work_accident+promotion_last_5years+salary,data=data,ntree=600,mtry=4)
pre<-predict(rf,data,type=c("class"))
pred<-ifelse(pre>0.3,1,0)
t1<-table(left,pred)
sum(diag(t1))/sum(t1)

#----------------------------------------------decisions treee-------------------------------------------------------
