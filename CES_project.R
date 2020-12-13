#load the data
CES<-read.csv(file.choose(),header = T)
#data cleaning
Dataset<-na.omit(CES)
#basic information of data
head(Dataset,30)
summary(Dataset)
#regression analysis to reduce some unrelated factors and get the most influencial factors
lmfilter<-lm(CES ~.,Dataset)
summary(lmfilter)

#EDA
#histogram
library(ggplot2)
hist(Dataset$CES)
CESDatav<-cut(Dataset$CES,br=c(1,20,40,60,80),labels=c('Great','Good','Moderate','Harmful'))
CESData<-na.omit(CESDatav)
summary(CESData)

#piechart
pie.number<-c(2850,2935,1958,270)
pie.labels<-c('Gerat','Good','Moderate','Harmful')
pie.pct<- round(pie.number/sum(pie.number)*100)
pie.labels<-paste(pie.labels,"", pie.pct, "%",sep="")
pie(pie.number,pie.labels,main="CES pie chart")

#scatterplot matrix
install.packages("psych")
library(psych)
pairs.panels(Dataset[c("Longitude", "Latitude", "PM2.5","Pesticides","Solid.Waste","Pollution.Burden","Asthma","Low.Birth.Weight","Education","Unemployment","Housing.Burden" )])

#PCA
#row names
names(Dataset)
#mean and variance
apply(Dataset,2,mean)
apply(Dataset,2,var)
#centers the variables to have mean zero
pr.out=prcomp(Dataset, scale=TRUE)
names(pr.out)
#correspond to the means and standard deviations of the variables that were used for scaling prior to implementing PCA
pr.out$center
pr.out$scale
#principal component loadings
pr.out$rotation
dim(pr.out$x)
#biplots with different interpretations
biplot(pr.out,scale=0)
pr.out$rotation=-pr.out$rotation 
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)
#standard deviation of each prin- cipal component
pr.out$sdev
pr.var=pr.out$sdev ^2
pr.var
pve=pr.var/sum(pr.var)
pve
#PVE explained by each component, as well as the cumulative PVE
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained ", ylim=c(0,1), type="b")
plot(cumsum(pve), xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1), type="b")
a=c(1,2,8,-3) 
cumsum (a)


#model1
#Multivariable linear regression and prediction
#This website is really helpful:https://guangchuangyu.github.io/statistics_notes/section-9.html
lm.fit<-lm(CES ~ PM2.5 + Pesticides + Pollution.Burden + Asthma + Low.Birth.Weight + Education + Unemployment + Housing.Burden, data=Dataset)
summary(lm.fit)
ls()
library(car)
vif(lm.fit)
#prediction
#The parameter prediction itself has errors. 
#Even if we know the real parameters, it is impossible to predict the data perfectly, 
#Because the model contains random errors. When predicting, it is best to use the confidence interval, 
#which means that the prediction include the uncertainty information Inside.
library(data.table)
test1<-Dataset[sample(row.names(Dataset)),]
pre<- predict(lm.fit, data.frame(PM2.5=test1$PM2.5,  Pesticides=test1$Pesticides, Pollution.Burden= test1$Pollution.Burden, Asthma=test1$Asthma,  Low.Birth.Weight=test1$Low.Birth.Weight,  Education=test1$Education,  Unemployment=test1$Unemployment,  Housing.Burden=test1$Housing.Burden), interval = 'prediction',level=0.95)
pre<-as.data.frame(pre, fit)
pre$y<-Dataset$CES
head(pre)
mean(with(pre, y> lwr & y < upr))
#the model R^2=0.791
#we may make some change
yy <- predict(lm.fit, se.fit=TRUE, interval="prediction", level=0.95)$fit
colnames(yy) <- c("fitpred", "lwrpred", "uprpred")
xx <- cbind(pre, yy)
head(xx)
mean(with(xx, y> lwrpred & y < uprpred))
remove.packages("ggplot2") # Unisntall ggplot
install.packages("ggplot2") # Install it again
library(ggplot2) # Load the librarie (you have to do this one on each new session)
ggplot(xx, aes(fit, y))+geom_point() + geom_line(aes(y=fit)) + geom_line(aes(y=lwr), color="red") +  geom_line(aes(y=upr), color="red") +  geom_line(aes(y=lwrpred), color="green") +  geom_line(aes(y=uprpred), color="blue")

#Model2 Knn
#L learned from this website and notebooks from class https://zhuanlan.zhihu.com/p/24533189
install.packages("kknn")
install.packages("class")
library(kknn)
library(class)
#Normalize the data
Data_scale <- scale(Dataset[2:16])
Data_scale <- as.data.frame(cbind(Dataset$CES,Data_scale))
names(Data_scale)[1] <- 'CES'
#standardization of data to 0-1
normalize <- function(x){
     return((x-min(x))/(max(x)-min(x)))
}
Data_normalize <- as.data.frame(lapply(Dataset[2:16],normalize))
summary(Data_normalize$Housing.Burden)
#after all the data change to 0-1,
Data_normalize <- as.data.frame(cbind(Dataset$CES,Data_normalize))
names(Data_normalize)[1] <- 'CES' 
Dataset <- as.data.frame(cbind(Dataset$CES,Dataset))
#Select training samples (70%) and test samples (30%) in the data set
index <- sample(2,nrow(Data_scale),replace = TRUE,prob=c(0.5,0.5))
traindata <- Dataset[index==1,]
testdata <- Dataset[index==2,]
#find the best k
for (i in 1:round(sqrt(dim(traindata)[1]))){
  model <- knn(train = traindata[,-1], test = testdata[,-1], cl = traindata$CES, k = i)
  Freq <- table(testdata[,1], model)
  print(1-sum(diag(Freq))/sum(Freq))
}
#We find that when K=1, the corresponding classification error rate is the lowest, so k=1
# Establish knn
model <- knn(train = traindata[,-1], test = testdata[,-1], cl = traindata$CES, k=1)
# use knn model  to predict
Freq <- table(testdata[,1], model)
Freq
sum(diag(Freq))/sum(Freq)

#kmeans
install.packages("factoextra")
data<-scale(Dataset)
head(data)
library(factoextra)
#determin the numbrer of clusters
fviz_nbclust(data, kmeans, method = "wss") + geom_vline(xintercept = 3, linetype = 2)
# another method for the number of clusters
wss <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:15) 
  wss[i] <- sum(kmeans(data,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
#Use k-mean to perform clustering
km <- kmeans(data, 17, nstart = 2)
# a glance at the result
print(km)
#The purpose of clustering is to have small distances within groups and large distances between groups. 
#between_SS / total_SS is the proportion of the total distance between groups, the closer to 1 the better
#To be honest the result (between_SS / total_SS = 59.4%) is not so good
#cluster visualization
fviz_cluster(km, data = data,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid",
             star.plot = TRUE, 
             repel = TRUE,
             ggtheme = theme_minimal()
)





