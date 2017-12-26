#------------------------------------------------------SVM ON LOAN DATA------------------------------------------------------------
#Our Aim is to predict who are willing to not make the payment and ready to default
#columns in the data and description
#1.int.rate=intrest rate
#2.installment=what is the installment they paid
#3.dti=debt to income ratio
#4.fico=it is a type of credit score
#5.days.with.cr.line=usually a bank, and a customer that establishes a maximum loan balance that the lender permits the borrower to access
#6.revol.bal=revolving balance
#7.inq.last.6mths=inquiry in last 6 months
#8.deliq.2yrs=Delinquent in 2 yrs(nothing but accounts who have not been paid past due the date in 2 years)

#------------------------------------------------------Model building-------------------------------------------------------------
getwd()
setwd('F:/Data science/R-DS')
loans<-read.csv("Loan_data.csv")
str(loans)
summary(loans)

#converting numeric to factors
loans$credit.policy<-factor(loans$credit.policy)

loans$inq.last.6mths<-factor(loans$inq.last.6mths)

loans$delinq.2yrs<-factor(loans$delinq.2yrs)

loans$pub.rec<-factor(loans$pub.rec)

loans$not.fully.paid<-factor(loans$not.fully.paid)

#here we can see that above are converted into factors
summary(loans)
head(loans)

#basic visualization
library(ggplot2)
pl<-ggplot(loans,aes(x=fico))
pl<-pl+geom_histogram(aes(fill=not.fully.paid),color='black',bins=40,alpha=0.5)

#this plot shows who are not fully paid (defaulted and not defaulted)
pl+scale_fill_manual(values =c('green','red'))+theme_bw()

pl<-ggplot(loans,aes(x=factor(purpose)))
pl<-pl+geom_bar(aes(fill=not.fully.paid),position="dodge")
pl+theme_bw()+theme(axis.text.x=element_text(angle=90,hjust=1))

#here all the dots are black
ggplot(loans,aes(int.rate,fico))+geom_point()+theme_bw()

#now we can here blue dots represents the people who left and red dots represent who havent left
#one of the reason maybe due to high intrest rate
ggplot(loans,aes(int.rate,fico))+geom_point(aes(color=not.fully.paid),alpha=0.3,size=2.5)+theme_bw()

library(caTools)

#spliting the dataset into train and test data
set.seed(101)
spl=sample.split(loans$not.fully.paid,0.7)
train=subset(loans,spl=TRUE)
test=subset(loans,spl=TRUE)

library(e1071)
model<-svm(not.fully.paid~.,data=train)
predict.values<-predict(model,test[1:13])
t=table(predict.values,test$not.fully.paid)
t
#accuracy
sum(diag(t))/sum(t)
#anyhow in the above we got an accuracy of 84% but our model is not doing good job
#if u observe on confusion matrix then we get to know that our model is not predicting who are ready to default
#This type of problem is called Accuracy paradox

#to sort out this problem we are using tune
#it will take some time maybe 7 mins
#after that we will get best cost and gamma values
#instead of sigmoid kernel we can also use radial kernel
tune.results<-tune(svm,train.x=not.fully.paid~.,data=train,kernel='sigmoid',ranges = list(cost=c(1,10),gamma=c(0.1,1)))
summary(tune.results)

model<-svm(not.fully.paid~.,data=train,cost=1,gamma=0.1)
predict.values1<-predict(model,test[1:13])
t1=table(predict.values1,test$not.fully.paid)
t1
sum(diag(t1))/sum(t1)

#radial kernel
#it will aware it will take 20 mins
tune.results1<-tune(svm,train.x=not.fully.paid~.,data=train,kernel='radial',ranges = list(cost=c(1,10),gamma=c(0.1,1)))
summary(tune.results1)
model1<-svm(not.fully.paid~.,data=train,cost=1,gamma=0.1)
predict.values2<-predict(model1,test[1:13])
t2=table(predict.values2,test$not.fully.paid)
t2
sum(diag(t2))/sum(t2)
#here we got a accuracy of 84.09.it is pretty good