#LOGISTIC REGRESSION

getwd()
fulldata<-read.csv(file.choose())
head(fulldata)
set.seed(123)
train=sample(1:nrow(fulldata),nrow(fulldata)*.7)
sample(train)
test=-train
modeldata=fulldata[train,]
testdata=fulldata[test,]
#------------------------------------------------------------------------------------------------
#->univariate analysis
summary(modeldata)

#outler identification
quantile(modeldata$Age,0.995)
quantile(modeldata$Age,0.999)

#outlier capping
modeldata$Age<-ifelse(modeldata$Age>75,75,modeldata$Age)
summary(modeldata)

#missing value imputation
summary(modeldata$Housing)
modeldata$Housing[modeldata$Housing==""]<-"A152"
head(modeldata)
#------------------------------------------------------------------------------------------------
#variable reduction
table(modeldata$Num_Dependents,modeldata$Default_On_Payment)
aggregate(modeldata$Num_Dependents,by=list(modeldata$Num_Dependents,modeldata$Default_On_Payment),length)
lol<-table(modeldata$Purpose_Credit_Taken,modeldata$Default_On_Payment)
colnames(lol)<-c("ND","D")

table(modeldata$Job_Status,modeldata$Default_On_Payment)
aggregate(modeldata$Job_Status,by=list(job_status=modeldata$Job_Status,d_p=modeldata$Default_On_Payment),length)

#missing value imputaion
modeldata$Job_Status[modeldata$Job_Status==""]<-"A174"

#converting categorial value to dummyvariables
#dummy variables for job status
modeldata$dummy_j_s_A171<-ifelse(modeldata$Job_Status=="A171",1,0)
modeldata$dummy_j_s_A172<-ifelse(modeldata$Job_Status=="A172",1,0)
modeldata$dummy_j_s_A173<-ifelse(modeldata$Job_Status=="A173",1,0)

#dummy variables for purpose credit taken
lol<-table(modeldata$Purpose_Credit_Taken,modeldata$Default_On_Payment)
lil=(length(lol)/2)
aggregate(modeldata$Purpose_Credit_Taken,by=list(modeldata$Purpose_Credit_Taken,modeldata$Default_On_Payment),length)
#This function is used to calculate the percentage who has defaulted or not defaulted.based on that we can divide into high,low
myplot<-function()
{
  data<-lol[1:lil,1,drop=F]
  data1<-lol[1:lil,2,drop=F]
  sum1=data+data1
  per=(data1/sum1)*100
  per
}
myplot()
modeldata$dummy_p_c_t_l<-ifelse(modeldata$Purpose_Credit_Taken=="P41"|modeldata$Purpose_Credit_Taken=="P43"|modeldata$Purpose_Credit_Taken=="P48",1,0)
modeldata$dummy_p_c_t_h<-ifelse(modeldata$Purpose_Credit_Taken=="P49"|modeldata$Purpose_Credit_Taken=="P40"|modeldata$Purpose_Credit_Taken=="P45"|modeldata$Purpose_Credit_Taken=="P50"|modeldata$Purpose_Credit_Taken=="P46",1,0)

#dummy variables for status checking account
lol2<-table(modeldata$Status_Checking_Accnt,modeldata$Default_On_Payment)
lil2=(length(lol2)/2)
myplot2<-function()
{
  data<-lol2[1:lil2,1,drop=F]
  data1<-lol2[1:lil2,2,drop=F]
  sum1=data+data1
  per=(data1/sum1)*100
  per
}
myplot2()
modeldata$dummy_s_c_a_H<-ifelse(modeldata$Status_Checking_Accnt=="S11",1,0)
modeldata$dummy_s_c_a_M<-ifelse(modeldata$Status_Checking_Accnt=="S12",1,0)

#dummy variables for credit history
lol3<-table(modeldata$Credit_History,modeldata$Default_On_Payment)
lil3=(length(lol3)/2)
myplot3<-function()
{
  data<-lol3[1:lil3,1,drop=F]
  data1<-lol3[1:lil3,2,drop=F]
  sum1=data+data1
  per=(data1/sum1)*100
  per
}
myplot3()
modeldata$dummy_Credit_History_high<-ifelse(modeldata$Credit_History=="A30"|modeldata$Credit_History=="A31",1,0)
modeldata$dummy_Credit_History_low<-ifelse(modeldata$Credit_History=="A34",1,0)

#dummy variables for years at present employement
lol4<-table(modeldata$Years_At_Present_Employment,modeldata$Default_On_Payment)
lil4=(length(lol4)/2)
myplot4<-function()
{
  data<-lol4[1:lil4,1,drop=F]
  data1<-lol4[1:lil4,2,drop=F]
  sum1=data+data1
  per=(data1/sum1)*100
  per
}
myplot4()
modeldata$dummy_Years_At_Present_Employment_high<-ifelse(modeldata$Years_At_Present_Employment=="E71"|modeldata$Years_At_Present_Employment=="E72",1,0)
modeldata$Yearsdummy_At_Present_Employment_medium<-ifelse(modeldata$Years_At_Present_Employment=="E73",1,0)

#dummy variables for maritial status
table(modeldata$Marital_Status_Gender,modeldata$Default_On_Payment)
modeldata$dummy_maritial_s_g<-ifelse(modeldata$Marital_Status_Gender=="A91"|modeldata$Marital_Status_Gender=="A92",1,0)
head(modeldata$dummy_maritial_s_g)

#dummy variables for other debtors
table(modeldata$Other_Debtors_Guarantors,modeldata$Default_On_Payment)
modeldata$dummy_other_d_g<-ifelse(modeldata$Other_Debtors_Guarantors=="A103",0,1)

#dummy variable for housing
table(modeldata$Housing,modeldata$Default_On_Payment)
modeldata$dummy_h<-ifelse(modeldata$Housing=="A152",0,1)

#dummy variable for foreign worker
table(modeldata$Foreign_Worker,modeldata$Default_On_Payment)
modeldata$dummy_f_w<-ifelse(modeldata$Foreign_Worker=="A201",1,0)
View(modeldata)

#set2-convert numeric variables to dummy variables

#dummy variable for age
table(modeldata$Age,modeldata$Default_On_Payment)
modeldata$dummy_Age_group<-ifelse(modeldata$Age<30,1,0)

#dummy variable for credit amount
modeldata$dummy_credit_amount<-ifelse(modeldata$Credit_Amount<5000,0,1)

#dummy variable for current year address
table(modeldata$Current_Address_Yrs,modeldata$Default_On_Payment)
modeldata$dummy_Current_Address_yrs<-ifelse(modeldata$Current_Address_Yrs==1,0,1)

#---------------------------------------------------------------------------------------
#Multicollinearity check
library(car)
#lm stands for linear collinearity
vif_output<-lm(Default_On_Payment~dummy_j_s_A171+dummy_j_s_A172+dummy_j_s_A173+dummy_p_c_t_h+dummy_p_c_t_l+dummy_s_c_a_H+dummy_s_c_a_M+dummy_Credit_History_high+dummy_Credit_History_low+dummy_Years_At_Present_Employment_high+dummy_Credit_History_low+dummy_maritial_s_g+dummy_other_d_g+dummy_f_w+dummy_Age_group+dummy_credit_amount+dummy_Current_Address_yrs+Duration_in_Months,data=modeldata)
#view vif output
vif(vif_output)
#here we can see that for a172 and a173 the vif is >2.so 1 variable to be removed
#in the next step what iam doing is regressing both variables seperately and retaining one variable with highest r square
multicoll_out1<-lm(Default_On_Payment~dummy_j_s_A172,data=modeldata)
multicoll_out2<-lm(Default_On_Payment~dummy_j_s_A173,data=modeldata)
summary(multicoll_out1)
summary(multicoll_out2)

#remove dummy job status A173 and repeat vif
vif_output<-lm(Default_On_Payment~dummy_j_s_A171+dummy_j_s_A172+dummy_p_c_t_h+dummy_p_c_t_l+dummy_s_c_a_H+dummy_s_c_a_M+dummy_Credit_History_high+dummy_Credit_History_low+dummy_Years_At_Present_Employment_high+dummy_Credit_History_low+dummy_maritial_s_g+dummy_other_d_g+dummy_f_w+dummy_Age_group+dummy_credit_amount+dummy_Current_Address_yrs+Duration_in_Months,data=modeldata)
vif(vif_output)
#here we can see that for dummy purpose high and low vif>1.5
multicoll_out3<-lm(Default_On_Payment~dummy_p_c_t_h,data=modeldata)
multicoll_out4<-lm(Default_On_Payment~dummy_p_c_t_l,data=modeldata)
summary(multicoll_out3)
summary(multicoll_out4)
vif_output<-lm(Default_On_Payment~dummy_j_s_A171+dummy_j_s_A172+dummy_p_c_t_l+dummy_s_c_a_H+dummy_s_c_a_M+dummy_Credit_History_high+dummy_Credit_History_low+dummy_Years_At_Present_Employment_high+dummy_Credit_History_low+dummy_maritial_s_g+dummy_other_d_g+dummy_f_w+dummy_Age_group+dummy_credit_amount+dummy_Current_Address_yrs+Duration_in_Months,data=modeldata)
vif(vif_output)

#here we can see that all vif values are less than 1.5 so we can peoceed further

#-------------------------------------------------------------------------------------------------------------------
#Logistic regression model building
#glm-generalized inear model
#family=binomial specifies it is binomial logistic regression model
logreg_output<-glm(Default_On_Payment~dummy_j_s_A171+dummy_j_s_A172+dummy_p_c_t_l+dummy_s_c_a_H+dummy_s_c_a_M+dummy_Credit_History_high+dummy_Credit_History_low+dummy_Years_At_Present_Employment_high+dummy_Credit_History_low+dummy_maritial_s_g+dummy_other_d_g+dummy_f_w+dummy_Age_group+dummy_credit_amount+dummy_Current_Address_yrs+Duration_in_Months,family=binomial(logit),data=modeldata)
summary(logreg_output,direction="forward")

#removing insignificant variables
logreg_output<-glm(Default_On_Payment~dummy_j_s_A171+dummy_s_c_a_H+dummy_s_c_a_M+dummy_Credit_History_high+dummy_Credit_History_low+dummy_Years_At_Present_Employment_high+dummy_Credit_History_low+dummy_maritial_s_g+dummy_other_d_g+dummy_f_w+dummy_Age_group+Duration_in_Months,family=binomial(logit),data=modeldata)
summary(logreg_output,direction="forward")

#estimate the probability of each record
modeldata$predicted_probabilty<-predict(logreg_output,modeldata,type="response")
View(modeldata)
write.csv(modeldata,"model_output.csv")
getwd()
#--------------------------------------------------------------------------------------------------------------
#Logistic Regression-Model Validation
#outlier capping
#outlier capping  testdata
testdata$Age<-ifelse(testdata$Age>75,75,testdata$Age)
summary(testdata)

#missing value imputation
summary(testdata$Housing)
testdata$Housing[testdata$Housing==""]<-"A152"
head(testdata)
#------------------------------------------------------------------------------------------------
#variable reduction
table(testdata$Num_Dependents,testdata$Default_On_Payment)
aggregate(testdata$Num_Dependents,by=list(testdata$Num_Dependents,testdata$Default_On_Payment),length)
lol<-table(testdata$Purpose_Credit_Taken,testdata$Default_On_Payment)
colnames(lol)<-c("ND","D")

table(testdata$Job_Status,testdata$Default_On_Payment)
aggregate(testdata$Job_Status,by=list(testdata$Job_Status,testdata$Default_On_Payment),length)

#missing value imputaion
testdata$Job_Status[testdata$Job_Status==""]<-"A174"

#converting categorial value to dummyvariables
#dummy variables for job status
testdata$dummy_j_s_A171<-ifelse(testdata$Job_Status=="A171",1,0)
testdata$dummy_j_s_A172<-ifelse(testdata$Job_Status=="A172",1,0)
testdata$dummy_j_s_A173<-ifelse(testdata$Job_Status=="A173",1,0)

#dummy variables for purpose credit taken
lol5<-table(testdata$Purpose_Credit_Taken,testdata$Default_On_Payment)
lil5=(length(lol)/2)
aggregate(testdata$Purpose_Credit_Taken,by=list(testdata$Purpose_Credit_Taken,testdata$Default_On_Payment),length)
#if we want all rows 
myplot5<-function()
{
  data<-lol5[1:lil5,1,drop=F]
  data1<-lol5[1:lil5,2,drop=F]
  sum1=data+data1
  per=(data1/sum1)*100
  per
}
myplot5()
testdata$dummy_p_c_t_l<-ifelse(testdata$Purpose_Credit_Taken=="P41"|testdata$Purpose_Credit_Taken=="P43"|testdata$Purpose_Credit_Taken=="P48",1,0)
testdata$dummy_p_c_t_h<-ifelse(testdata$Purpose_Credit_Taken=="P49"|testdata$Purpose_Credit_Taken=="P40"|testdata$Purpose_Credit_Taken=="P45"|testdata$Purpose_Credit_Taken=="P50"|testdata$Purpose_Credit_Taken=="P46",1,0)

#dummy variables for status checking account
lol6<-table(testdata$Status_Checking_Accnt,testdata$Default_On_Payment)
lil6=(length(lol6)/2)
myplot6<-function()
{
  data<-lol2[1:lil6,1,drop=F]
  data1<-lol2[1:lil6,2,drop=F]
  sum1=data+data1
  per=(data1/sum1)*100
  per
}
myplot6()
testdata$dummy_s_c_a_H<-ifelse(testdata$Status_Checking_Accnt=="S11",1,0)
testdata$dummy_s_c_a_M<-ifelse(testdata$Status_Checking_Accnt=="S12",1,0)

#dummy variables for credit history
lol7<-table(testdata$Credit_History,testdata$Default_On_Payment)
lil7=(length(lol7)/2)
myplot7<-function()
{
  data<-lol3[1:lil7,1,drop=F]
  data1<-lol3[1:lil7,2,drop=F]
  sum1=data+data1
  per=(data1/sum1)*100
  per
}
myplot7()
testdata$dummy_Credit_History_high<-ifelse(testdata$Credit_History=="A30"|testdata$Credit_History=="A31",1,0)
testdata$dummy_Credit_History_low<-ifelse(testdata$Credit_History=="A34",1,0)

#dummy variables for years at present employement
lol8<-table(testdata$Years_At_Present_Employment,testdata$Default_On_Payment)
lil8=(length(lol8)/2)
myplot8<-function()
{
  data<-lol8[1:lil8,1,drop=F]
  data1<-lol8[1:lil8,2,drop=F]
  sum1=data+data1
  per=(data1/sum1)*100
  per
}
myplot8()
testdata$dummy_Years_At_Present_Employment_high<-ifelse(testdata$Years_At_Present_Employment=="E71"|testdata$Years_At_Present_Employment=="E72",1,0)
testdata$Yearsdummy_At_Present_Employment_medium<-ifelse(testdata$Years_At_Present_Employment=="E73",1,0)

#dummy variables for maritial status
table(testdata$Marital_Status_Gender,testdata$Default_On_Payment)
testdata$dummy_maritial_s_g<-ifelse(testdata$Marital_Status_Gender=="A91"|testdata$Marital_Status_Gender=="A92",1,0)
head(testdata$dummy_maritial_s_g)

#dummy variables for other debtors
table(testdata$Other_Debtors_Guarantors,testdata$Default_On_Payment)
testdata$dummy_other_d_g<-ifelse(testdata$Other_Debtors_Guarantors=="A103",0,1)

#dummy variable for housing
table(testdata$Housing,testdata$Default_On_Payment)
testdata$dummy_h<-ifelse(testdata$Housing=="A152",0,1)

#dummy variable for foreign worker
table(testdata$Foreign_Worker,testdata$Default_On_Payment)
testdata$dummy_f_w<-ifelse(testdata$Foreign_Worker=="A201",1,0)
View(modeldata)

#set2-convert numeric variables to dummy variables

#dummy variable for age
table(testdata$Age,testdata$Default_On_Payment)
testdata$dummy_Age_group<-ifelse(testdata$Age<30,1,0)

#dummy variable for credit amount
testdata$dummy_credit_amount<-ifelse(testdata$Credit_Amount<5000,0,1)

#dummy variable for current year address
table(testdata$Current_Address_Yrs,testdata$Default_On_Payment)
testdata$dummy_Current_Address_yrs<-ifelse(testdata$Current_Address_Yrs==1,0,1)

#Mullticolinearity check
vif_output<-lm(Default_On_Payment~dummy_j_s_A171+dummy_j_s_A172+dummy_p_c_t_l+dummy_s_c_a_H+dummy_s_c_a_M+dummy_Credit_History_high+dummy_Credit_History_low+dummy_Years_At_Present_Employment_high+dummy_Credit_History_low+dummy_maritial_s_g+dummy_other_d_g+dummy_f_w+dummy_Age_group+dummy_credit_amount+dummy_Current_Address_yrs+Duration_in_Months,data=modeldata)vif(vif_output)
summary(logreg_output,direction="forward")

#estimate the probability of each record
testdata$predicted_probabilty<-predict(logreg_output,testdata,type="response")
testdata$final_prediction<-ifelse(testdata$predicted_probabilty>0.3,1,0)

#To test Accuracy
t1=table(testdata$Default_On_Payment,testdata$final_prediction)
sum(diag(t))/sum(t)
View(testdata)
write.csv(testdata,"model_output.csv")
z<-testdata$final_prediction
y<-as.matrix(z)
length(z)

#to calculate no of 1's that is who will default
myplot9<-function()
{
  data<-y[1:length(z)!=0]
  sum(data)
}
x<-myplot9()
#percentage for people who will default what we predicted is
myplot10<-function()
 {
  per=(x/length(z))*100
  per
}
people_default<-myplot10()
#percentage of people who will not default what we predicted is
people_n_d<-100-people_default
people_n_d
#.........................function................................................


myplot<-function()
{
  data<-lol[1,1]
  data1<-lol[1,2]
  sum1=data+data1
  per=(data1/sum1)*100
  per
}
myplot()
lol[1:1,1,drop=F]

#if we want all rows 
myplot<-function(n)
{
  data<-lol[1:n,1,drop=F]
  data1<-lol[1:n,2,drop=F]
  sum1=data+data1
  per=(data1/sum1)*100
  per
}
myplot(10)
#if we want specific row
myplot1<-function(m,n)
{
  data<-lol[m:n,1,drop=F]
  data1<-lol[m:n,2,drop=F]
  sum1=data+data1
  per=(data1/sum1)*100
  per
}
myplot1(2,2)
install.packages("car")
#---------------------------------Roc(receiving opearting characteristic curve)--------------------------------------------------------------
intall.packages("ROCR")
library(ROCR)
pred<-predict(logreg_output,testdata,type="response")
head(pred)
hist(pred)
head(cbind(testdata$Default_On_Payment,pred))

#prediction function is used to transform the input data into standardized form
pred<-prediction(pred,testdata$Default_On_Payment)

#using performnace All kinds of predictor evaluations are performed
#here acc is nothing but accuracy
eval<-performance(pred,"acc")
plot(eval)
abline(h=0.78)
plot(eval)
abline(h=0.76)
plot(eval)

#here we manually harcoding it
abline(h=0.76,v=0.51)

max<-which.max(slot(eval,"y.values")[[1]])
max
acc<-slot(eval,"y.values")[[1]][max]
cutoff<-slot(eval,"x.values")[[1]][max]
print(c(Accuracy=acc,Cutoff=cutoff))

#sometimes we are not intrested in overall accuracy and we will be more concerned on predicting people who will default
pred<-predict(logreg_output,testdata,type="response")
head(pred)
hist(pred)
head(cbind(testdata$Default_On_Payment,pred))
pred<-prediction(pred,testdata$Default_On_Payment)
performance(pred,"tpr")
t=table(testdata$Default_On_Payment,testdata$final_prediction)

#here we are just calculating percentage of people who will default and we also predicted them that they will default
myplot11<-function()
{
  data<-t[1,2]
  data1<-t[2,2]
  per=data1/(data+data1)
  per
}
myplot11()
# or we can maually do this 343/(289+343)*100

roc<-performance(pred,"tpr","fpr")#here tpr=true positive rate and fpr=false positive rate
141/(141+727)
plot(roc)
abline(a=0,b=1)#intercept is 0 and slope is 1
#here  we can see that the curve is above the line so it is better
#if it is below the line then it is worst

plot(roc,colorize=T)
#color is  based on sensitivity
plot(roc,colorize=T,main="ROC Curve",ylab="Sensitivity",xlab="1-Specificity")
abline(a=0,b=1)
