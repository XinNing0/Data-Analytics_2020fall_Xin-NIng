#bank-marketing
#loaddataset
databank<-read.csv(file.choose(),header = T)
test<-read.csv(file.choose(),header = T)
head(databank)

#clean and get a new dataset
newdatabank<-databank[,-c(8,9,10,11,12,13,14,15)]
Data<-which(newdatabank$marital=="unknown"|newdatabank$housing=="unknown"|
           newdatabank$loan=="unknown"|newdatabank$job=="unknown"|newdatabank$education=="unknown")
new<-newdatabank[-Data,-5]

#EDA
summary(new)
hist(new[which(new$marital=="single"),1],seq(0,100,3))
abline(v = median(new[which(new$marital=="single"),1]))
hist(new[which(new$marital=="married"|new$marital=="divorced"),1],seq(0,100,3))
abline(v=median(new[which(new$marital=="married"|new$marital=="divorced"),1]))
library("ggplot2")
ggplot(data = new)+geom_bar(mapping = aes(x=job,fill=education))
ggplot(data = new)+geom_bar(mapping = aes(x=housing,fill=loan))

#age,job and loan can be removed from the past data analysis
dataforpca<-newdatabank[,7:11]
dataforpca <- dataforpca[complete.cases(dataforpca),]
dataforpca<-as.matrix(dataforpca[,-1])
rownames(dataforpca) <- dataforpca[ ,1]
pca<-prcomp(dataforpca,scale. = T)
biplot(pca,scale = 0)
pr.var<-pca$sdev^2
pve<-pr.var/sum(pr.var)
plot(pve,ylim=c(0,1),type="b")

#use 2 continuous variables- cons.conf.idx & nr.employed)
final<-newdatabank[,-c(1,2,6,7,8,10)]
newdatabank$emp.var.rate

#do same data munging to test dataset
test<-read.csv(file='/users/xin/Desktop/bank-additional/bank-additional.csv')
M<-which(test$marital=="unknown"|test$education=="unknown"|test$housing=="unknown")
finaltest<-test[-M,c(3,4,6,18,20,21)]
head(final)

#construct models
library("caret")
control<-trainControl(method = "cv",number = 10)
metric<-"Accuracy"
set.seed(10)
model<-train(y~.,data = final, method="ctree", metric=metric, trControl=control)
summary(model)
print(model)

install.packages("partykit")

library("party")
ctr<-ctree(y~., data = final, subset=NULL, weights=NULL, controls = ctree_control(mincriterion = 0.5))
help("ctree")
plot(ctr)
pre<-predict(ctr,finaltest)
table(pre,finaltest$y)

