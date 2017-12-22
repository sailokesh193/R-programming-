AirPassengers
View(AirPassengers)
class(AirPassengers)
start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers)
#The cycle of ts is 12 months in a year
summary(AirPassengers)
plot(AirPassengers)
#here drawing a best fit line
abline(reg=lm(AirPassengers~time(AirPassengers)))

#here we can see that it prints cycles across the years
cycle(AirPassengers)

#this will display the trend
plot(aggregate(AirPassengers,FUN=mean))
plot(aggregate(AirPassengers,FUN=median))

?aggregate

#chick data
data<-ChickWeight
head(data)
dim(data)
summary(data)
str(data)

#how many chickens
unique(data$Chick)

#how many diets
unique(data$Diet)

#how many diets
unique(data$Diet)

#how many time points
unique(data$Time)

library(ggplot2)
ggplot(data=data,aes(x=Time,y=weight,group=Chick,colour=Diet))+geom_line()+geom_point()
ggplot(data=data,aes(x=Time,y=weight,group=Chick,colour=Chick))+geom_line()+geom_point()
View(data)

aggregate(data$weight,list(diet=data$Diet),FUN=mean)

#AGGREGATE ON TIME
aggregate(data$weight,list(time=data$Time),mean)

aggregate(data$weight,list(time=data$Time),sd)
(aggregate(data$weight,list(time=data$Time,diet=data$Diet),mean))
tail(aggregate(data$weight,list(time=data$Time,diet=data$Diet),mean))

#boxplot
boxplot(AirPassengers~cycle(AirPassengers))
plot(AirPassengers)

#here we can observe that variance is decreased to some extent
plot(log(AirPassengers))

#First of all to apply time series model our data should be in stationary so we are giving diff(differentiating)
plot(diff(log(AirPassengers)))
install.packages('tseries')
library(tseries)

adf.test(diff(log(AirPassengers)),alternative = c("stationary","explosive"),k=0)

#here we can see that data is not stationary
acf(AirPassengers)

#time series modelling
#it is also not stationary though but variance somewhat decreased
#here the data is not stationary
#the lines are above the blue line
#our objective is to keep the lines within the blue lines
acf(log(AirPassengers))
plot(diff(log(AirPassengers)))

#here we can see that it is stationary
a<-diff(log(AirPassengers))
#here we can see that the axis got shifted for 2 nd line so q=1 
acf(a)
plot(a)

#here we can see that line got shifted after first line so p=0
pacf(diff(log(AirPassengers)))

#in the above we did diff only once so d=1
plot(diff(log(AirPassengers)))
acf(diff(log(AirPassengers)))

# we found that (0,1,1) as (p,d,q) comes out to be the combination
# with least AIC and BIC.
fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
# ntc v r not specifying diff abv
# as v r manually specifying the val of d as 1
pred <- predict(fit, n.ahead = 10*12)
# predict for next 10 yrs
# since v hv used log our vals r in logarithmic form
# v need to convert em to decimal form
# v need to use e value which is 2.718
pred1 <- round(2.718^pred$pred,0)
pred1
ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))
# ntc the dotted lines r predicted lines
# so this is how our future looks like
# Test the model
datawide <- ts(AirPassengers,frequency = 12, start=c(1949,1),end=c(1959,12))
fit <- arima(log(datawide),seasonal=list(order=c(0,1,1),period=12))
pred <- predict(fit,n.ahead=10*12)
pred1 <- 2.718^pred$pred
data1<-pred1
predicted_1960 <- round(data1,digits=0)
lol<-head(predicted_1960,12)
original_1960 <- tail(AirPassengers,12)
lol1<-original_1960
t=table(predicted_1960,original_1960)
sum(diag(t))/sum(t)

#prediction based on error
ds<-lol1-lol
ds2<-as.matrix(ds)
#to get 1 and 0 according to the data
myplot<-function(n)
{
  
  ds1<-ifelse(ds2[1:n]<=10,1,0)
  ds1
  
}
myplot(12)
ab<-myplot(12)
ac<-as.matrix(ab)
len<-length(ac)

#number of 1's and their sum
myplot2<-function(n)
{
  a<-ac[1:n!=0]
  sum(a)
}
at<-myplot2(12)

#accuracy
myplot3<-function()
{
  predict<-(at/len)*100
  predict
}
myplot3()
