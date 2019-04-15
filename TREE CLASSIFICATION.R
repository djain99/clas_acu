rm(list=ls())
set.seed(40)
temp<-read.csv("E:\\Books\\ADVANCED BOOKS\\data mining and predictive analysis r codes\\Data sets\\BREAST_CANCER.txt",
               header=TRUE,
               sep="")



#test train split
train=sample(699,500)

train_data<-temp[train,]
library(dplyr)


test_data<-setdiff(temp,train_data)


train_data$class<-as.factor(train_data$class)
train_data$bare_nuclei<-as.integer(train_data$bare_nuclei)

test_data$class<-as.factor(test_data$class)
test_data$bare_nuclei<-as.integer(test_data$bare_nuclei)


str(temp)


#basic classification tree

library(tree)

tree.model<-tree(class~.,data = train_data)

#cross validation
cvm_tree.model<-cv.tree(tree.model,FUN = prune.misclass)


cvm_tree.model
#pruning to the required tree

prune.tree<-prune.misclass(tree.model,best = 8)

table(true=test_data$class,pred=predict(prune.tree,newdata =test_data[,-10],type="class" ))
#correct pred=103
#incorrect pred=9

print((114/124)*100)#accuracy


#####bagging tree

library(randomForest)

bagging.model<-randomForest(class~.,data = train_data,mtry=9,importance=TRUE)

#cross validation
#it  uses bootstrap so we dont need cross validation here
table(true=test_data$class,pred=predict(bagging.model,newdata =test_data[,-10],type="class" ))
#correct pred=108
#incorrect pred=4

print((116/124)*100)#accuracy

##random forest

random.model<-randomForest(class~.,data = train_data,mtry=3,importance=TRUE)

table(true=test_data$class,pred=predict(random.model,newdata =test_data[,-10],type="class" ))
#correct pred=109
#incorrect pred=3

print((118/124)*100)#accuracy


#using boosting method

#library(gbm)

#boosting.model<-gbm(class~.,data = train_data,distribution = "bernoulli",n.trees=5000,interaction.depth = 4 ,cv.folds =3)
#summary(boosting.model)

#table(true=test_data$class,pred=predict(boosting.model,newdata =test_data[,-10],type="response",n.trees=5000))
#correct pred=109
#incorrect pred=3

#print((109/112)*100)#accuracy
