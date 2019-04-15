rm(list=ls())
library(e1071)


temp<-read.csv("E:\\Books\\ADVANCED BOOKS\\data mining and predictive analysis r codes\\Data sets\\BREAST_CANCER.txt",
               header=TRUE,
               sep="")
str(temp)
colnames(temp)<-c("x.1","x.2","x.3","x.4","x.5","x.6","x.7","x.8","x.9","y")

temp$x.6<-as.integer(temp$x.6)
temp$y<-as.factor(temp$y)

dat<-as.data.frame(scale(temp[,-10]))
dat$y<-temp$y
temp<-dat
#divide the set into train test split

set.seed(40)

train=sample(699,500)

train_data<-temp[train,]
library(dplyr)


test_data<-setdiff(temp,train_data)


#applying svc on this data ie support vector classifier

svc.model<-svm(y~.,data = train_data,kernel="radial",cost=1,gamma=1)

plot(svc.model,train_data)
#using tune fucntion to find optimal value of cost parameter

tune.model<-tune(svm,y~.,data=train_data,kernel="linear",
                 ranges =list(cost=c(.001,.01,.1,1,5,10,100)))
                 


table(true=test_data$y,pred=predict(tune.model$best.model,newdata = test_data[,-10]))
#correct pred =116
#incorrect pred=8
print((116/124)*100)#accuracy


                 
                 
                 
                 
                 
                 
                 
                 
                 
                 


