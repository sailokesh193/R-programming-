getwd()
setwd("F:/Data science/R-DS")
getwd()
data<-read.csv("50-Startups.csv")
head(data)
head(data$R.D.Spend)
attach(data)
R.D.Spend
hist(R.D.Spend)
cor(Profit,R.D.Spend)
cor(R.D.Spend,Profit)
#here we can see that the data is linearly dependent hence we can apply linear regression
plot(R.D.Spend,Profit)
#here we can see that data is not linearly dependent hence we cannot include this
plot(R.D.Spend,Marketing.Spend)
plot(R.D.Spend,State)
plot(R.D.Spend,Administration)
#checking R-squared values
reg=lm(Profit~.,data)
summary(reg)
#here we can see that R.d.spend and administraton is related so we can include them
plot(R.D.Spend,Profit)
reg2=lm(Profit~R.D.Spend+Administration,data)
abline(reg2)

test<-read.csv("Test_startups.csv")
test
fit<-lm(Profit~R.D.Spend)
wfit<-lm(Profit~R.D.Spend,weights='4')
pred<-predict(reg2,test,interval='prediction')
