rm(list=ls())

temp<-read.csv("E:\\Books\\ADVANCED BOOKS\\data mining and predictive analysis r codes\\Data sets\\BREAST_CANCER.txt",
               header = TRUE,
               sep="")
str(temp)
temp$class<-as.factor(temp$class)
temp$bare_nuclei<-as.integer(temp$bare_nuclei)

temp[,-10]<-data.frame(scale(temp[,-10]))
#train and test split

set.seed(40)

train<-sample(699,500)
train_data<-temp[train,]

library(dplyr)

test_data<-setdiff(temp,train_data)

##applying knn
library(class)
for (i in 2:10) {
  

knn.model<-knn(train_data[,-10],test_data[,-10],train_data$class,k=i)

correct_pred<-table(true=test_data$class,pred=knn.model)[1]
correct_pred<-correct_pred+table(true=test_data$class,pred=knn.model)[4]

incorrect_pred<-table(true=test_data$class,pred=knn.model)[3]
incorrect_pred<-incorrect_pred+table(true=test_data$class,pred=knn.model)[2]
print(paste("accuracy for i = ",i))
print((correct_pred/(correct_pred+incorrect_pred))*100)
}

#without scaling we find highest accuracy of 95.161 for k=4,5
#with scaling we find highest accuracy at k=5,6,7,10  ie 94.354% less then without scaled data