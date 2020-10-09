nyt2<-read.csv("/Users/Eileen/Desktop/DA/a3/dds_ch2_nyt/nyt2.csv",header=T)
nyt3<-read.csv("/Users/Eileen/Desktop/DA/a3/dds_ch2_nyt/nyt3.csv",header=T)
nyt4<-read.csv("/Users/Eileen/Desktop/DA/a3/dds_ch2_nyt/nyt4.csv",header=T)
nyt5<-read.csv("/Users/Eileen/Desktop/DA/a3/dds_ch2_nyt/nyt5.csv",header=T)
nyt6<-read.csv("/Users/Eileen/Desktop/DA/a3/dds_ch2_nyt/nyt6.csv",header=T)

#a#
boxplot(nyt2$Age,nyt3$Age,nyt4$Age,nyt5$Age,nyt6$Age,names = c("nyt2","nyt3","nyt4","nyt5","nyt6"),main="Age distribution")
boxplot(nyt2$Impressions,nyt3$Impressions,nyt4$Impressions,nyt5$Impressions,nyt6$Impressions,names = c("nyt2","nyt3","nyt4","nyt5","nyt6"),main="Impressions distribution")

#b#
a1=which(nyt3$Age==0)
a1
age2_wt0<-nyt2[-a1,1]
hist(nyt2$Age)
hist(age2_wt0,freq = T)
hist(nyt3$Age)
hist(age3_wt0,freq = T)
hist(nyt4$Age)
hist(age4_wt0,freq = T)
hist(nyt5$Age)
hist(age5_wt0,freq = T)
hist(nyt6$Age)
hist(age6_wt0,freq = T)
hist(nyt2$Impressions)
hist(nyt3$Impressions)
hist(nyt4$Impressions)
hist(nyt5$Impressions)
hist(nyt6$Impressions)

#c#
?ecdf
plot(ecdf(nyt2$Age),do.points=F,verticals=T,main="ECDF of Age for 2-6 datasets")
plot(ecdf(nyt3$Age),do.points=F,verticals=T,add=TRUE,col="red")
plot(ecdf(nyt4$Age),do.points=F,verticals=T,add=TRUE,col="blue")
plot(ecdf(nyt5$Age),do.points=F,verticals=T,add=TRUE,col="green")
plot(ecdf(nyt6$Age),do.points=F,verticals=T,add=TRUE,col="yellow")
?plot
?legend
legend('topleft',c('nyt2$Age','nyt3$Age','nyt4$Age','nyt4$Age','nyt5$Age'),cex=0.7,fill=c('black','red','blue','green','yellow'))
plot(ecdf(nyt2$Impressions),do.points=F,verticals=T,main="ECDF of Impressions for 2-6 datasets")
plot(ecdf(nyt3$Impressions),do.points=F,verticals=T,add=TRUE,col="red")
plot(ecdf(nyt4$Impressions),do.points=F,verticals=T,add=TRUE,col="blue")
plot(ecdf(nyt5$Impressions),do.points=F,verticals=T,add=TRUE,col="green")
plot(ecdf(nyt6$Impressions),do.points=F,verticals=T,add=TRUE,col="yellow")

?qqnorm
qqnorm(nyt2$Age)
qqnorm(nyt2$Impressions)
qqline(nyt2$Impressions)
?qqnorm

?qqnorm
qqnorm(nyt3$Age)
qqnorm(nyt3$Impressions)
qqline(nyt3$Impressions)
?qqnorm

 #d#
shapiro.test(nyt2$Impressions)
ks.test(nyt2$Age,"pnorm")
ks.test(nyt2$Impressions,"pnorm")
ks.test(nyt3$Age,"pnorm")
ks.test(nyt3$Impressions,"pnorm")
install.packages("nortest")
library("nortest")
ad.test(nyt2$Age)
ad.test(nyt3$Impressions)

nyt11<-read.csv("/Users/Eileen/Desktop/DA/a3/dds_ch2_nyt/nyt11.csv",header=T)
nyt12<-read.csv("/Users/Eileen/Desktop/DA/a3/dds_ch2_nyt/nyt12.csv",header=T)
a2=which(nyt11$Age==0)
filtered_age_11<-nyt10[-a2,1]
filtered_age_12<-nyt11[-a2,1]
?hist
hist(filtered_age_11,xlim = range(0,100),probability = T,ylim=NULL,xlab = "Age distribution",main = "Histgram of Filtered Age-Nyt11")
hist(filtered_age_12,xlim = range(0,100),probability = T,ylim=NULL,xlab = "Age distribution",main = "Histgram of Filtered Age - Nyt12")
lines(density(filtered_age_11,bw="SJ"))
lines(density(filtered_age_12,bw="SJ"))
?density
plot(ecdf(filtered_age_11),do.points=F,verticals=T,main="ECDF of Age - filtered Nyt11")
plot(ecdf(filtered_age_12),do.points=F,verticals=T,main="ECDF of Age - filtered Nyt12")
?qqnorm
qqnorm(filtered_age_11,main="qq-norm plot of filtered age - nyt11")
qqnorm(filtered_age_12,main="qq-norm plot of filtered age - nyt12")
qqline(filtered_age_11)
qqline(filtered_age_12)
ad.test(filtered_age_11)
ad.test(filtered_age_12)