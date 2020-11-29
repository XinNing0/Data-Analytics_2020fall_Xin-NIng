#wine_white
#loaddataset
dataw<-read.csv(file.choose(),header = T)
summary(dataw)
str(dataw)
dataw$quality<-as.factor(dataw$quality)

#PCA
dataforpca<-as.matrix(dataw[,-12])
prw<-prcomp(dataforpca,scale. = T)
biplot(prw,scale = 1)
#related: residual~density~alcohol, citric~fixed~ph,free~total
pr.var<-prw$sdev^2
pve<-pr.var/sum(pr.var)
plot(pve,ylim = c(0,1),type = "b")
#remove residual,density,citric,fixed,and free 
newdata<-dataw[,-c(1,3,4,6,8)]

#EDA
newdata$sulphates
library("ggplot2")
ggplot(data = newdata)+geom_boxplot(mapping = aes(x=reorder(quality,alcohol,FUN=median),y=alcohol))+coord_flip()
ggplot(data = newdata)+geom_boxplot(mapping = aes(x=reorder(quality,volatile.acidity,FUN=median),y=volatile.acidity))+coord_flip()
ggplot(data = newdata)+geom_boxplot(mapping = aes(x=reorder(quality,chlorides,FUN=median),y=chlorides))+coord_flip()
ggplot(data = newdata)+geom_boxplot(mapping = aes(x=reorder(quality,total.sulfur.dioxide,FUN=median),y=total.sulfur.dioxide))+coord_flip()
ggplot(data = newdata)+geom_boxplot(mapping = aes(x=reorder(quality,pH,FUN=median),y=pH))+coord_flip()
ggplot(data = newdata)+geom_boxplot(mapping = aes(x=reorder(quality,sulphates,FUN=median),y=sulphates))+coord_flip()

#classification
library("caret")
library("rpart")
library("rpart.plot")
partition<-createDataPartition(newdata$quality,p=0.8,list = F)
test<-newdata[-partition,]
train<-newdata[partition,]
control<-trainControl(method = "cv", number = 10)
#rpart
set.seed(10)
fit.rpart<-train(quality~.,data=train,method="rpart",trControl = control)
print(fit.rpart)

rp<-rpart(quality~.,data = train,control =rpart.control(cp=0.0189727) ,method = "class")
pre.rpart<-predict(rp,test,type = "class")
table(pre.rpart,test$quality)

#random forest
set.seed(10)
fit.rf<-train(quality~.,data=train,method="rf",trControl = control)
print(fit.rf)

library("randomForest")
raf<-randomForest(quality~.,train,ntree=500,mtry=2,importance=T)
pre.rf<-predict(raf,test,type = "class")
table(pre.rf,test$quality)

#knn
set.seed(10)
install.packages("knn")
fit.knn<-train(quality~.,data=train,method="knn",trControl = control)
print(fit.knn)
install.packages("knn")
pre.knn<-knn(train[,-7],test[,-7],train$quality,k=5)
table(pre.knn,test$quality)

