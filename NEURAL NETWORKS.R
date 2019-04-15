rm(list=ls())
library(e1071)


temp<-read.csv("E:\\Books\\ADVANCED BOOKS\\data mining and predictive analysis r codes\\Data sets\\BREAST_CANCER.txt",
               header=TRUE,
               sep="")
str(temp)
colnames(temp)<-c("x.1","x.2","x.3","x.4","x.5","x.6","x.7","x.8","x.9","y")

temp$x.6<-as.integer(temp$x.6)
temp$y<-as.factor(temp$y) 

temp$x.1<-(temp$x.1-min(temp$x.1))/max(temp$x.1)-min(temp$x.1)

temp$x.2<-(temp$x.2-min(temp$x.2))/max(temp$x.2)-min(temp$x.2)

temp$x.3<-(temp$x.3-min(temp$x.3))/max(temp$x.3)-min(temp$x.3)

temp$x.4<-(temp$x.4-min(temp$x.4))/max(temp$x.4)-min(temp$x.4)

temp$x.5<-(temp$x.5-min(temp$x.5))/max(temp$x.5)-min(temp$x.5)

temp$x.6<-(temp$x.6-min(temp$x.6))/max(temp$x.6)-min(temp$x.6)

temp$x.7<-(temp$x.7-min(temp$x.7))/max(temp$x.7)-min(temp$x.7)

temp$x.8<-(temp$x.8-min(temp$x.8))/max(temp$x.8)-min(temp$x.8)

temp$x.9<-(temp$x.9-min(temp$x.9))/max(temp$x.9)-min(temp$x.9)

#divide the set into train test split

set.seed(40)

train=sample(699,500)

train_data<-temp[train,]
library(dplyr)


test_data<-setdiff(temp,train_data)



#applying nnet package

library(nnet)

net.dat<-nnet(y~.,data = train_data,size=4)

pred.net.model<-predict(net.dat,newdata = test_data[,-10])

pred.net.model<-ifelse(pred.net.model>.55,1,0)

tabel.mod<-table(true=test_data$y,pred=pred.net.model)

correct_pred<-tabel.mod[1]
correct_pred<-correct_pred+tabel.mod[4]

incorrect_pred<-tabel.mod[3]

incorrect_pred<-incorrect_pred+tabel.mod[2]

print((correct_pred/(correct_pred+incorrect_pred))*100)

#accuracy is 91.129% least from all the classification models
