
##################################################
rm(list=ls())                                   ##
cat("\014")                                     ##
##
#######################################################################
setwd("")##
getwd()                                                              ##  
#######################################################################

#######################################################################
##loading  Data
#######################################################

load("Clst_21F.RData")

print(Clst_21F)
typeof( Clst_21F)
###################################################################################
#install packages
install.packages("ggplot2")
install.packages("cluster")
install.packages("factoextra")
install.packages("dplyr")
library(ggplot2)
library(cluster)
library(factoextra)
library(dplyr)
###################################################################################
#Q-1.Data Transformation
###################################################################################
str( Clst_21F)
head( Clst_21F)
summary( Clst_21F)

normn_pp<-function(x)
{return((x-mean(x))/sd(x))}

 Clst_21F$Foodnormn<-normn_pp( Clst_21F$Food)
 Clst_21F$Entrnormn<-normn_pp( Clst_21F$Entr)
 Clst_21F$Educnormn<-normn_pp( Clst_21F$Educ)
 Clst_21F$Trannormn<-normn_pp( Clst_21F$Tran)
 Clst_21F$Worknormn<-normn_pp( Clst_21F$Work)
 Clst_21F$Housnormn<-normn_pp( Clst_21F$Hous)
 Clst_21F$Othrnormn<-normn_pp( Clst_21F$Othr)

summary( Clst_21F)
###################################################################################
#Q-2Descriptive Data Analysis
###################################################################################
str( Clst_21F)
head( Clst_21F)
summary( Clst_21F)

hist( Clst_21F$Food)#before standardization
hist( Clst_21F$Foodnormn)#After standardization

hist( Clst_21F$Work)#before standardization
hist( Clst_21F$Worknormn)#After standardization


#before standardization
boxplot( Clst_21F[c(1:7)])

#After standardization
boxplot( Clst_21F[c(8:14)])
###################################################################################
#Q-3.1 Clustering
###################################################################################
str( Clst_21F)

clusterdata_pp<- Clst_21F[c(8,12)]#storing columns in on dataframe

str(clusterdata_pp)

cluster3_pp<-kmeans(clusterdata_pp,iter.max = 10,centers = 3,nstart = 10)
cluster3_pp
cluster4_pp<-kmeans(clusterdata_pp,iter.max = 10,centers = 4,nstart = 10)
cluster4_pp
cluster5_pp<-kmeans(clusterdata_pp,iter.max = 10,centers = 5,nstart = 10)
cluster5_pp
cluster6_pp<-kmeans(clusterdata_pp,iter.max = 10,centers = 6,nstart = 10)
cluster6_pp
cluster7_pp<-kmeans(clusterdata_pp,iter.max = 10,centers = 7,nstart = 10)
cluster7_pp



 Clst_21F$cluster3<-factor(cluster3_pp$cluster)
 Clst_21F$cluster4<-factor(cluster4_pp$cluster)
 Clst_21F$cluster5<-factor(cluster5_pp$cluster)
 Clst_21F$cluster6<-factor(cluster6_pp$cluster)
 Clst_21F$cluster7<-factor(cluster7_pp$cluster)
head( Clst_21F[c(8,12,15,16,17,18,19)])


###################################################################################
#Q-4.1
###################################################################################
str( Clst_21F)
 Clst_21F$cluster3

centers4_pp <- data.frame(cluster4_pp=factor(1:4), cluster4_pp$centers)

#k=4
ggplot(data= Clst_21F,aes(x=Foodnormn,y=Worknormn,color=cluster4))+geom_point()
ggplot(data= Clst_21F,aes(x=Foodnormn,y=Worknormn,color=cluster4,shape=cluster4))+
geom_point(alpha=.8)

#k=5
ggplot(data= Clst_21F,aes(x=Foodnormn,y=Worknormn,color=cluster5))+geom_point()
ggplot(data= Clst_21F,aes(x=Foodnormn,y=Worknormn,color=cluster5,shape=cluster5))+
  geom_point(alpha=.8)

#k=6
ggplot(data= Clst_21F,aes(x=Foodnormn,y=Worknormn,color=cluster6))+geom_point()
ggplot(data= Clst_21F,aes(x=Foodnormn,y=Worknormn,color=cluster6,shape=cluster6))+
  geom_point(alpha=.8)
  
###################################################################################
#Q-4.3
###################################################################################
#summarise
str( Clst_21F)

 Clst_21F_sum_pp<-  Clst_21F %>% 
group_by(cluster5) %>% 
summarise(Food = mean(Food), Entr = mean(Entr), Educ=mean(Educ), Tran=mean(Tran),
Work=mean(Work),Hous=mean(Hous), Othr=mean(Othr), N=n() )

 Clst_21F_sum_pp













