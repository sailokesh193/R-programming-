#Data cleaning part i have done in R
setwd("G:/downloads")
data <- read.csv("intern-training-samples.csv", sep = ",", fileEncoding="utf-16")
set.seed(123)
train=sample(1:nrow(data),nrow(data)*.7)
sample(train)
test=-train
modeldata=data[train,]
testdata=data[test,]
modeldata$title<-NULL
modeldata$description<-NULL


View(modeldata[!complete.cases(modeldata),])
summary(modeldata)
levels(modeldata$category)
View(modeldata[!complete.cases(modeldata$category)=='accessories',])
nrow(modeldata[!complete.cases(modeldata),])

#data cleaning
View(modeldata[!complete.cases(modeldata),])
View(modeldata[complete.cases(modeldata),])
levels(modeldata$category)
View(modeldata[modeldata$category=='health & beauty'&modeldata$sub_category=='beauty',])
modeldata[is.na(modeldata$gender)& modeldata$category=="health & beauty"&modeldata$sub_category=='beauty',"gender"]<-'womens'
summary(modeldata)

View(modeldata[modeldata$category=='home & pet'&modeldata$sub_category=='home appliances',])
modeldata[is.na(modeldata$gender)& modeldata$category=="home & pet"&modeldata$sub_category=='home appliances',"gender"]<-'womens'
modeldata[is.na(modeldata$gender)& modeldata$category=="home & pet"&modeldata$sub_category=='kitchen & dining',"gender"]<-'womens'
modeldata[is.na(modeldata$gender)& modeldata$category=="home & pet","gender"]<-'womens'
summary(modeldata)

View(modeldata[modeldata$category=='sports & outdoors'&modeldata$sub_category=='team shop',])
modeldata[is.na(modeldata$gender)& modeldata$category=="sports & outdoors","gender"]<-'unisex'
summary(modeldata)

View(modeldata[modeldata$category=='toys & games',])
modeldata[is.na(modeldata$gender)& modeldata$category=="toys & games"&modeldata$sub_category=='games & puzzles',"gender"]<-'boys'
modeldata[is.na(modeldata$gender)& modeldata$category=="toys & games"&modeldata$sub_category=='dolls & stuffed animals',"gender"]<-'girls'
modeldata[is.na(modeldata$gender)& modeldata$category=="toys & games"&modeldata$sub_category=='building blocks & sets',"gender"]<-'boys'
modeldata[is.na(modeldata$gender)& modeldata$category=="toys & games"&modeldata$sub_category=='pretend play & dress up',"gender"]<-'girls'
modeldata[is.na(modeldata$gender)&modeldata$category=='toys & games','gender']<-'baby & toddler'
summary(modeldata)

View(modeldata[modeldata$category=='electronics',])
modeldata[is.na(modeldata$gender)&modeldata$category=='electronics','gender']<-'unisex'
summary(modeldata)

View(modeldata[modeldata$category=='automotive',])
modeldata[is.na(modeldata$gender)&modeldata$category=='automotive','gender']<-'unisex'
summary(modeldata)

View(modeldata[modeldata$category=='books, movies & music',])
modeldata[is.na(modeldata$gender)&modeldata$category=='books, movies & music','gender']<-'unisex'
summary(modeldata)

View(modeldata[modeldata$category=='kids & baby',])
modeldata[is.na(modeldata$gender)& modeldata$category=="kids & baby"&modeldata$sub_category=='baby',"gender"]<-'baby & toddler'
summary(modeldata)

modeldata[is.na(modeldata$gender)&modeldata$category=='flowers & food gifting','gender']<-'unisex'
summary(modeldata)

modeldata[is.na(modeldata$gender)&modeldata$category=='health & beauty','gender']<-'unisex'

interndata1<-write.csv(modeldata,"intern1.csv")

