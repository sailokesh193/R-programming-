install.packages("ggplot2")
library(ggplot2)
getwd()
setwd("F:/R")
install.packages("tree")
install.packages("party")
install.packages("partykit")
install.packages("rpart")
library(tree)
library(party)
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
library("partykit")

#creating a tree using tree package
dataups<-read.csv(file.choose())
View(dataups)

treeups<-tree(Profit~.,data=dataups)
plot(treeups)
text(treeups)

cart_model<-rpart(Profit~.,data=dataups)
plot(as.party(cart_model))
print(cart_model)
summary(cart_model)
rsq.rpart(cart_model)

#here we can see that on 2nd split error incresed
cart_model<-rpart(Profit~Administration,data=dataups)
plot(as.party(cart_model))
rsq.rpart(cart_model)

cart_model<-rpart(Profit~Administration+State,data=dataups)
rsq.rpart(cart_model)

cart_model<-rpart(Profit~R.D.Spend+State,data=dataups)
plot(as.party(cart_model))
rsq.rpart(cart_model)

rpart.plot(cart_model)
rpart.plot(cart_model,extra=1)

fit_1<-rpart(Profit~State,method="class",data=dataups)
plot(fit_1)
text(fit_1)
plot(as.party(fit_1))
rsq.rpart(fit_1)

fit_1<-rpart(Profit~State+R.D.Spend,method="class",data=dataups)
plot(as.party(fit_1))
rsq.rpart(fit_1)
rpart.plot(fit_1)
rpart.plot(fit_1,extra=1)
rpart.plot(fit_1,extra=2)
rpart.plot(fit_1,extra=3)
?rpart.plot

treeups<-tree(Profit~Administration,data=dataups)
plot(treeups)
text(treeups)

treeups<-tree(Profit~Administration+Marketing.Spend,data=dataups)
plot(treeups)
text(treeups)

treeups<-tree(Profit~Administration+Marketing.Spend+R.D.Spend,data=dataups)
plot(treeups)
text(treeups)


summary(treeups)
print(treeups$cptable)

