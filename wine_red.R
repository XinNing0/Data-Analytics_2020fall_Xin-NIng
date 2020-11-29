#wine_red
#loaddataset
dataset<-read.csv(file.choose(),header = T)
summary(dataset)
str(dataset)
dataset$quality<-as.factor(dataset$quality)

#PCA to reduce variables
dataforpca<-dataset[,-12]
dataforpca<-as.matrix(dataforpca)
pr<-prcomp(dataforpca,scale. = T)
biplot(pr,scale = 0)
names(pr)
pr.var<-pr$sdev^2
pve<-pr.var/sum(pr.var)
plot(pve,xlab ="Principle Component",ylab = "Proportion of variance explained",ylim = c(0,1),type = "b")

#6 components are enough
#free sulfur dioxide(total),residual sugar(volatile),density(ph),fixed&citric acid(ph) should be removed.
newdata<-dataset[,-c(1,3,4,6,8)]

#EDA(distribution) of variables
library("ggplot2")
ggplot(data = newdata)+geom_boxplot(mapping = aes(x=reorder(quality,alcohol,FUN=median),y=alcohol))+coord_flip()
ggplot(data = newdata)+geom_boxplot(mapping = aes(x=reorder(quality,volatile.acidity,FUN=median),y=volatile.acidity))+coord_flip()
ggplot(data = newdata)+geom_boxplot(mapping = aes(x=reorder(quality,chlorides,FUN=median),y=chlorides))+coord_flip()
ggplot(data = newdata)+geom_boxplot(mapping = aes(x=reorder(quality,total.sulfur.dioxide,FUN=median),y=total.sulfur.dioxide))+coord_flip()
ggplot(data = newdata)+geom_boxplot(mapping = aes(x=reorder(quality,pH,FUN=median),y=pH))+coord_flip()
ggplot(data = newdata)+geom_boxplot(mapping = aes(x=reorder(quality,sulphates,FUN=median),y=sulphates))+coord_flip()
newdata$alcohol

#Ctrees for classification
library("C50")
set.seed(10)
N<-sample(nrow(newdata),nrow(newdata)*0.8)
train<-newdata[N,]
test<-newdata[-N,]
class<-C5.0(train[,1:6],train[,7])
summary(class)
plot(class)
pre<-predict(class,test[,1:6])
table(pre,test$quality)

install.packages("lattice")
installed.packages("ggplot2")

library("caret")
set.seed(10)
trcontrol<-trainControl(method = "cv", number = 10)
model<-train(quality~.,data = train,method="ctree",trControl = trcontrol)
print(model)

set.seed(10)
rf<-train(quality~.,data = train,method="rf",trControl = trcontrol)
print(rf)

library("randomForest")
rfNews()
randomf<-randomForest(quality~.,data = train,ntree=500,mtry=2,importance=T)
prf<-predict(randomf,test1.1,type = "class")
table(prf,test$quality)

set.seed(10)
rp<-train(quality~.,data = train,method="rpart",trControl = trcontrol)
print(rp)

library("rpart")
library("rpart.plot")
rpa<-rpart(quality~.,data = train1.1,cp=0.014,method = "class")
rpart.plot(rpa)

rpre<-predict(rpa,test,type="class")
table(rpre,test$quality)

set.seed(10)
rp<-train(quality~.,data = train,method="rpart",trControl = trcontrol)
print(rp)
