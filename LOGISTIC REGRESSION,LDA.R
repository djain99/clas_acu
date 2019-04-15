rm(list=ls())

temp<-read.csv("E:\\Books\\ADVANCED BOOKS\\data mining and predictive analysis r codes\\Data sets\\BREAST_CANCER.txt",
               header = TRUE,
               sep="")
str(temp)
temp$class<-as.factor(temp$class)
temp$bare_nuclei<-as.integer(temp$bare_nuclei)




#train and test split

set.seed(40)

train<-sample(699,500)
train_data<-temp[train,]

library(dplyr)

test_data<-setdiff(temp,train_data)


#######applying logistic regression

log.model<-glm(class~.,data = train_data,family=binomial)

contrasts(temp$class)
#from this contrast function we know that these predict probabilities are of 
#probability of instance lying in class 1
predicted_class<-ifelse(predict(log.model,newdata = test_data[,-10],type="response")>.5,1,0)

table(true=test_data$class,pred=predicted_class)
#correct prediction=115
#incorrect prediction=9

print((115/124)*100)#accuracy

############################################################
############################################################
#LDA-LINEAR DISCRIMINANT ANALYSIS

#performing lda

library(MASS)

lda.model<-lda(class~.,data = train_data)

contrasts(temp$class)
#from this contrast function we know that these predict probabilities are of 
#probability of instance lying in class 1
predicted_class<-predict(lda.model,newdata = test_data[,-10])

table(true=test_data$class,pred=predicted_class$class)
#correct prediction=116
#incorrect prediction=8

print((116/124)*100)#accuracy
#we cannot apply the principle of precision and recall here
#as these are not true and false type categorization