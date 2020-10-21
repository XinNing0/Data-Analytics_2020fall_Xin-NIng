Pbronx<-read.csv("/Users/Eileen/Desktop/DA/Data/rollingsales_bronx.csv",header=T)
names(Pbronx)<-as.matrix(Pbronx[4,])
#Assign the dataframe name with the 4th row characters.
colnames(Pbronx)[c(11,15,16,20)]<-c("ZIP.CODE","LAND.SQUARE.FEET","GROSS.SQUARE.FEET","SALE.PRICE")
#For the 4 variables to be explored, replace the column names as the previous names contains blank.
Pbronx<-Pbronx[which(Pbronx$GROSS.SQUARE.FEET!="0" & bronx1$LAND.SQUARE.FEET!="0" & bronx1$SALE.PRICE!="$0"),]
#Filter out useless data in the above 3 variables to make the analysis effective.
Pbronx<-Pbronx[-c(1,2,3,4),]
#Delete the first 3 rows that contain introduction information but not useful data.
attach(Pbronx)
Pbronx$SALE.PRICE<-sub("\\$","",SALE.PRICE) 
temp<-gsub(",","",Pbronx$SALE.PRICE)
temp
Pbronx$SALE.PRICE<-as.numeric(temp) 
#Change the type of SALE.PRICE, GROSS.SQUARE.FEET and LAND.SQUARE.FEET from character to be numeric.
Pbronx$GROSS.SQUARE.FEET<-as.numeric(gsub(",","",Pbronx$GROSS.SQUARE.FEET)) 
Pbronx$LAND.SQUARE.FEET<-as.numeric(gsub(",","",Pbronx$LAND.SQUARE.FEET)) 
plot(Pbronx$GROSS.SQUARE.FEET,Pbronx$SALE.PRICE)
#Finally, we obtain the dataset



#====#
mul<-lm(log(SALE.PRICE)~log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET),data = Pbronx)
summary(mul)
plot(mul)
set.seed(250)
x<-sample(1:2431,10,replace=F)
train1<-Pbronx[-x,]
test1<-Pbronx[x,]
mul<-lm(log(SALE.PRICE)~log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET),data = train1)
summary(mul)
pm1<-predict(mul,data.frame(GROSS.SQUARE.FEET=test1$GROSS.SQUARE.FEET,LAND.SQUARE.FEET=test1$LAND.SQUARE.FEET),interval = "prediction")
pm1<-as.data.frame(pm1)
ac1<-log(test1$SALE.PRICE)
ac1<-as.data.frame(ac1)
pm1$real<-ac1
#====#
Pbronx<-bronx1
str(bronx1$ZIP.CODE)
Pbronx$ZIP.CODE<-droplevels(ZIP.CODE)
summary(train2$ZIP.CODE)
indicator<-sample(2,nrow(Pbronx),replace=T,prob = c(0.9,0.1))
train2<-Pbronx[indicator==1,]
test2<-Pbronx[indicator==2,]
sqrt(2421)
pk1<-knn(train=train2[,c(15,16,20)],test = test2[,c(15,16,20)],cl=train2$ZIP.CODE,k=49)
str(test2$SALE.PRICE)
dim(train2$SALE.PRICE)
dim(test2$SALE.PRICE)
library("e1071")
classifier<-naiveBayes(train2$SALE.PRICE,train2$ZIP.CODE)
pk2<-predict(classifier,test2$SALE.PRICE)
table(pk1,test2$ZIP.CODE)
summary(pk1)
test2$ZIP.CODE

