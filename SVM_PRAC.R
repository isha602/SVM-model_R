
library("e1071")
library(ISLR)

set.seed(80)
data("Khan")
data=Khan
View(Khan)
head(Khan)

#working on training data
d=data.frame(x=Khan$xtrain,y=as.factor(Khan$ytrain))

fit=svm(y~., data=d,kernel="linear",cost=10)
summary(fit)

table(fit$fitted,d$y)

#Here we see that there are actually no training errors. 
#This is not surprising due to the large number of predictors and few observations. 

#working on testing data 

t=data.frame(x=Khan$xtest,y=as.factor(Khan$ytest))
test_pred=predict(fit,newdata=t)
summary(test_pred)
table(test_pred,t$y)

#Here we only make 2 errors on the test data. 
#We might as well continue and tune the parameters to see if we can make any improvements

tn=train(y~.,data=d,method="svmLinear")
################################
data("Auto")
View(Auto)
#Create a binary variable that takes on a 1 for cars with gas mileage above the median, and a 0 for cars with gas mileage below the median.

library(ISLR)
med=median(Auto$mpg)
var=ifelse(Auto$mpg>med,1,0)
var
Auto$mpglevel=as.factor(var)

#Fit a support vector classifier to the data with various values of cost, in order to predict whether a car gets high or low gas mileage.
#Report the cross-validation errors associated with different values of this parameter. Comment on your results.
library(e1071)
set.seed(3255)

svm_fit=tune(svm,mpglevel~.,data=Auto,kernel="linear",ranges = list(cost = c(0.01,0.1, 1, 5, 10, 100)))
summary(svm_fit)

svm_fit
set.seed(30)
svm_fit1=tune(svm,mpglevel~.,data=Auto,kernel="polynomial",ranges = list(cost = c(0.01,0.1, 1, 5, 10, 100)),degree = c(2, 3, 4))

summary(svm_fit1)
set.seed(463)
tune.out = tune(svm, mpglevel ~ ., data = Auto, kernel = "radial", ranges = list(cost = c(0.1,1, 5, 10), gamma = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
###################################
library("ISLR")
data("OJ")
set.seed(9000)
train = sample(nrow(OJ), 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]
View(OJ)
#Fit a support vector classifier to the training data using cost=0.01, 
#with Purchase as the response and the other variables as predictors. 
#Use the summary() function to produce summary statistics, and describe the results obtained.

fit=svm(Purchase~.,data=OJ.train,kernel="linear",cost=0.01)
summary(fit)

#What are the training and test error rates?
pred=predict(fit,OJ.train)
tt=table(pred,OJ.train$Purchase)
acc=diag(tt)/sum(tt)
acc

pred_test=predict(fit,OJ.test)
tt1=table(OJ.test$Purchase,pred_test)
tt1
acc1=diag(tt1)/sum(tt1)
acc1

#Use the tune() function to select an optimal cost. Consider values in the range 0.01 to 10.
tun=tune(svm,Purchase~.,data=OJ.train,kernel="linear",ranges=list(cost=c(0.01,1,5,10)))
summary(tun)
#Compute the training and test error rates using this new value for cost
tun1=svm(Purchase~.,data=OJ.train,kernel="linear",cost=tun$best.parameters$cost)
train_pred=predict(tun1,OJ.train)
r=table(train_pred,OJ.train$Purchase)
r
(73+55)/(436+236+55+73)

test_pred=predict(tun1, OJ.test)
rt=table(test_pred,OJ.test$Purchase)
rt
(29+16)/(29+16+146+79)


#Repeat parts (b) through (e) using a support vector machine with a radial kernel. Use the default value for gamma.
svm_rad=svm(Purchase~.,data=OJ.train,kernel="radial",gamma=1)
training_pred=predict(svm_rad,OJ.train)
table(training_pred,OJ.train$Purchase)
(75+38)/(75+38+453+234)

testing_pred=predict(svm_rad,OJ.test)
table(testing_pred,OJ.test$Purchase)
(31+17)/(31+17+145+77)

tun2=tune(svm,Purchase~.,data=OJ.train,kernel="radial",range=list(cost=c(0.01,1,5,10)))

summary(tun2)
svm2=svm(Purchase~.,data=OJ.train,kernel="radial",cost=tun2$best.parameters$cost)
train01=predict(svm2,OJ.train)
table(train01,OJ.train$Purchase)
(75+38)/(453+234+75+38)
test01=predict(svm2,OJ.test)
table(test01,OJ.test$Purchase)
(17+31)/(145+17+77+31)

#Repeat parts (b) through (e) using a support vector machine with a polynomial kernel. Set degree=2.
svm3=svm(Purchase~.,data=OJ.train,kernel="poly",degree=2)
train3=predict(svm3, OJ.train)
table(train3,OJ.train$Purchase)
(106+34)/(106+34+457+203)
test3=predict(svm3, OJ.test)
table(test3,OJ.test$Purchase)
(12+41)/(150+67+12+41)
summary(svm3)
tun3=tune(svm,Purchase~.,data=OJ.train,kernel="poly",degree=2,range=list(cost=c(0.01,1,5,10)))
svm4=svm(Purchase~.,data=OJ.train,kernel="poly",degree=2,cost=tun3$best.parameters$cost)
train4=predict(svm4,OJ.train)
table(train4,OJ.train$Purchase)             
(79+34)/(79+34+457+230)

test4=predict(svm4,OJ.test)
table(test4,OJ.test$Purchase)             
(19+30)/(19+30+143+78)


############
install.packages("mlbench")
library(mlbench)
set.seed(142)
data("Glass")
#View(Glass)
str(Glass)
class(Glass$Type)


#splitting into training and testing dataset 
index=sample(1:nrow(Glass),round(0.7*nrow(Glass)),replace=F)
traingl=Glass[index,]
testgl=Glass[-index,]

#svm model
glass_fit=svm(Type~.,data=traingl,kernel="linear",cost=0.1)
glass_pred=predict(glass_fit,testgl)
glass_pred
table(glass_pred,testgl$Type)

library(caret)
library(dplyr)
######################
set.seed(1234)
brdata=read.csv("C:/Users/Isha/Downloads/Breast Cancer Wisconsin.csv")
#View(brdata)
brdata=brdata[-1]   
#brdata[which(brdata$diagnosis == "M"),]$diagnosis <- 1
#brdata[which(brdata$diagnosis == "B"),]$diagnosis <- 0
#brdata  = apply(brdata,1,as.numeric)


#splitting the dataset into train ans test 
set.seed(99)
index=sample(1:nrow(brdata),round(0.8*nrow(brdata)),replace=F)
trainbr=brdata[index,]
levels(testbr)
testbr=brdata[-index,]

#svm model 
br_fit=tune(svm,as.factor(diagnosis)~.,data=trainbr,kernel="linear",range=list(cost=c(0.001, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 5)))
br_svm=svm(as.factor(diagnosis)~.,data=trainbr,kernel="linear",cost=br_fit$best.parameters$cost)
br_pred=predict(br_svm,testbr)
br_pred
table(br_pred,testbr$diagnosis)
(72+39)/(72+39+3)
##OR
#br_pred_test=predict(br_fit$best.model,newdata=testbr)
#confusionMatrix(br_pred_test,testbr$diagnosis)

#svm model polynomial 
br_fit1=tune(svm,as.factor(diagnosis)~.,data=trainbr,kernel="polynomial",range=list(cost=c(0.001, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 5)))
br_svm1=svm(as.factor(diagnosis)~.,data=trainbr,kernel="polynomial",cost=br_fit1$best.parameters$cost)
br_pred1=predict(br_svm1,testbr)
table(br_pred1,testbr$diagnosis)
(73+26)/(73+26+15)
##OR
#br_pred1_test=predict(br_fit1$best.model,newdata=testbr)
confusionMatrix(br_pred1_test,testbr$diagnosis)

summary(br_fit1$best.model)
class(br_pred1)
class(testbr)
levels(br_pred1)
levels(testbr$diagnosis)

#svm model radial
set.seed(123)
br_fit2=tune(svm,as.factor(diagnosis)~.,data=trainbr,kernel="radial",range=list(cost=c(0.001, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 5)))
br_svm2=svm(as.factor(diagnosis)~.,data=trainbr,kernel="radial",cost=br_fit2$best.parameters$cost)
br_pred2=predict(br_svm2,testbr)
table(br_pred2,testbr$diagnosis)
