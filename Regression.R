
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

load("MLR_21F.RData")

print( Assign_MLR_21F)
typeof( Assign_MLR_21F)

MLR_data_pp<-as.data.frame(MLR_21F)
str(MLR_data_pp)
stat.desc(MLR_data_pp)
###################################################################################
#install packages
if(!require(pastecs)){install.packages("pastecs")}
  if(!require(pastecs)){install.packages("corrgram")}

library(pastecs)
library(corrgram)
###################################################################################
#Reduce Dimensionality
###################################################################################
#1
###################################################################################
summary(is.na(MLR_data_pp))# 1973 time2 will 
summary(is.null(MLR_data_pp$n.child))#false
Missing_Value_Ratio_pp<-sum(is.na(MLR_data_pp$time2))/2033*100
Missing_Value_Ratio_pp
MLR_Filtered_data_pp<-subset(MLR_data_pp, select = -c(time2) )
anyNA(MLR_Filtered_data_pp)

###################################################################################
#2
###################################################################################
stat.desc(MLR_Filtered_data_pp)
summary(MLR_Filtered_data_pp)
table(MLR_Filtered_data_pp$Pol)
str(MLR_Filtered_data_pp)

###################################################################################
#3
###################################################################################
group_numeric_PP<-as.numeric(as.factor( Assign_MLR_21F$group))
hs.grad_numeric_PP<-as.numeric(as.factor( Assign_MLR_21F$hs.grad))
nation_numeric_PP<-as.numeric(as.factor( Assign_MLR_21F$nation))
gender_numeric_PP<-as.numeric(as.factor( Assign_MLR_21F$gender))
m.status_numeric_PP<-as.numeric(as.factor( Assign_MLR_21F$m.status))
political_numeric_PP<-as.numeric(as.factor( Assign_MLR_21F$political))

MLR_Filtered_data_pp<-cbind(MLR_Filtered_data_pp,group_numeric_PP,hs.grad_numeric_PP,nation_numeric_PP,gender_numeric_PP,m.status_numeric_PP
                            ,political_numeric_PP)


cor(MLR_Filtered_data_pp[c(1,6,9:17,18:23)],method="spearman")
MLR_Filtered_data_pp<-subset(MLR_Filtered_data_pp, select = -c(time3) )
head(MLR_Filtered_data_pp)
str(MLR_Filtered_data_pp)

###################################################################################

###################################################################################
#2 Data trans
###################################################################################
str(MLR_Filtered_data_pp)

hs.grad_dummies_pp<-model.matrix(~hs.grad-1,data=MLR_Filtered_data_pp)
group_dummies_pp<-model.matrix(~group-1,data=MLR_Filtered_data_pp)
nation_dummies_pp<-model.matrix(~nation-1,data=MLR_Filtered_data_pp)
gender_dummies_pp<-model.matrix(~gender-1,data=MLR_Filtered_data_pp)
m.status_dummies_pp<-model.matrix(~m.status-1,data=MLR_Filtered_data_pp)
political_dummies_pp<-model.matrix(~political-1,data=MLR_Filtered_data_pp)

head(hs.grad_dummies_pp)
head(group_dummies_pp)
head(nation_dummies_pp)
head(gender_dummies_pp)
head(m.status_dummies_pp)
head(political_dummies_pp)


MLR_Filtered_data_pp<-cbind(MLR_Filtered_data_pp,group_dummies_pp,nation_dummies_pp,hs.grad_dummies_pp
,gender_dummies_pp,m.status_dummies_pp,political_dummies_pp)

head(MLR_Filtered_data_pp)

#MLR_Filtered_data_pp<-MLR_Filtered_data_pp[-c(18,19)]

###################################################################################
# 3 Outliers
###################################################################################

str(MLR_Filtered_data_pp)
# fixing the oultiers
par(mfrow=c(1,1))

#functions to find correct replace value
up_fix_pp <- function(colname) {
  up_fix_pp=quantile(colname,probs = c(.75))+1.5*IQR(colname)
  return (up_fix_pp)
}

down_fix_pp<- function(colname){
down_fix_pp=quantile(colname,probs = c(.25))-1.5*IQR(colname)
return (down_fix_pp)
}

#fixing scr outliers
boxplot(MLR_Filtered_data_pp$scr,main="scr")

MLR_Filtered_data_pp$scr<-with(MLR_Filtered_data_pp,
ifelse(up_fix_pp(MLR_Filtered_data_pp$scr)<scr,up_fix_pp(MLR_Filtered_data_pp$scr),scr))

MLR_Filtered_data_pp$scr<-with(MLR_Filtered_data_pp,
ifelse(down_fix_pp(MLR_Filtered_data_pp$scr)>scr,down_fix_pp(MLR_Filtered_data_pp$scr),scr))

boxplot(MLR_Filtered_data_pp$scr,main="scr")


#fixing n.child outliers
boxplot(MLR_Filtered_data_pp$n.child,main="n.child")

MLR_Filtered_data_pp$n.child<-with(MLR_Filtered_data_pp,
ifelse(up_fix_pp(MLR_Filtered_data_pp$n.child)<n.child,up_fix_pp(MLR_Filtered_data_pp$n.child),n.child))
MLR_Filtered_data_pp$n.child<-with(MLR_Filtered_data_pp,
ifelse(down_fix_pp(MLR_Filtered_data_pp$n.child)>n.child,down_fix_pp(MLR_Filtered_data_pp$n.child),n.child))

boxplot(MLR_Filtered_data_pp$n.child,main="n.child")

#fixing housing outliers

boxplot(MLR_Filtered_data_pp$housing,main="housing")

MLR_Filtered_data_pp$housing<-with(MLR_Filtered_data_pp,
ifelse(up_fix_pp(MLR_Filtered_data_pp$housing)<housing,up_fix_pp(MLR_Filtered_data_pp$housing),housing))
MLR_Filtered_data_pp$housing<-with(MLR_Filtered_data_pp,
ifelse(down_fix_pp(MLR_Filtered_data_pp$housing)>housing,down_fix_pp(MLR_Filtered_data_pp$housing),housing))

boxplot(MLR_Filtered_data_pp$housing,main="housing")

#fixing other outliers
boxplot(MLR_Filtered_data_pp$other,main="other")

MLR_Filtered_data_pp$other<-with(MLR_Filtered_data_pp,
ifelse(up_fix_pp(MLR_Filtered_data_pp$other)<other,up_fix_pp(MLR_Filtered_data_pp$other),other))
MLR_Filtered_data_pp$other<-with(MLR_Filtered_data_pp,
ifelse(down_fix_pp(MLR_Filtered_data_pp$other)>other,down_fix_pp(MLR_Filtered_data_pp$other),other))

boxplot(MLR_Filtered_data_pp$other,main="other")


#fixing score outliers
boxplot(MLR_Filtered_data_pp$score,main="score")
MLR_Filtered_data_pp$score<-with(MLR_Filtered_data_pp,
ifelse(up_fix_pp(MLR_Filtered_data_pp$score)<score,up_fix_pp(MLR_Filtered_data_pp$score),score))
MLR_Filtered_data_pp$score<-with(MLR_Filtered_data_pp,
ifelse(down_fix_pp(MLR_Filtered_data_pp$score)>score,down_fix_pp(MLR_Filtered_data_pp$score),score))

boxplot(MLR_Filtered_data_pp$score,main="score")



###################################################################################
# 4 Exploratory Analysis
###################################################################################
str(MLR_Filtered_data_pp)
MLR_Filtered_data_pp


cor(MLR_Filtered_data_pp[c(1,6,9:17,18:23)])

corrgram(MLR_Filtered_data_pp[c(1,6,9:17,18:23)],order=TRUE,lower.panel = panel.shade,upper.panel = panel.pie,
text.panel = panel.txt,main="Stats")

###################################################################################
# 5 Simple Linear Regression
###################################################################################
str(MLR_Filtered_data_pp)


# Testing normality
shapiro.test(MLR_Filtered_data_pp$Pol)
qqnorm(MLR_Filtered_data_pp$Pol, pch=6)
qqline(MLR_Filtered_data_pp$Pol)
densityplot( ~Pol, data=MLR_Filtered_data_pp, pch=46)

shapiro.test(MLR_Filtered_data_pp$score)
qqnorm(MLR_Filtered_data_pp$score, pch=6)
qqline(MLR_Filtered_data_pp$score)
densityplot( ~score, data=MLR_Filtered_data_pp, pch=46)

shapiro.test(MLR_Filtered_data_pp$scr)
qqnorm(MLR_Filtered_data_pp$scr, pch=6)
qqline(MLR_Filtered_data_pp$scr)
densityplot( ~scr, data=MLR_Filtered_data_pp, pch=46)

#histograms
hist(MLR_Filtered_data_pp$Pol)
hist(MLR_Filtered_data_pp$score)
hist(MLR_Filtered_data_pp$scr)



Linmodel1_pp <- lm(Pol ~ score, data=MLR_Filtered_data_pp)
Linmodel1_pp
summary(Linmodel1_pp)

xyplot(Pol ~ score, data=MLR_Filtered_data_pp, panel = function(x,y) {
  panel.xyplot(x, y)
  panel.abline(Linmodel1_pp)
}, main=list(label="Pol by score (with Regression Line)",
             cex=0.85))

Linmodel2_pp <- lm(Pol ~ scr, data=MLR_Filtered_data_pp)
Linmodel2_pp
summary(Linmodel2_pp)
xyplot(Pol ~ scr, data=MLR_Filtered_data_pp, panel = function(x,y) {
  panel.xyplot(x, y)
  panel.abline(Linmodel2_pp)
}, main=list(label="Pol by scr (with Regression Line)",
             cex=0.85))

par(mfrow=c(1,1))
plot(Linmodel1_pp)

plot(Linmodel2_pp)


###################################################################################
#6 models
###################################################################################
names(MLR_Filtered_data_pp)
str(MLR_Filtered_data_pp)

#linear regression
LM_all_pp1<-lm(Pol ~ group+hs.grad+nation+gender+age+m.status+political
               +n.child+income+food+housing+other
               +score+time1+scr,data = MLR_Filtered_data_pp,na.action = na.omit)


#backward model
bck_model_PP<-step(LM_all_pp1,direction = "backward",details=TRUE)
bck_model_PP
summary(bck_model_PP)


LM_all_pp2<-lm(Pol ~ hs.grad+nation+gender+age+m.status+political
               +n.child+income+food+housing+other+group
               +score+time1+scr,data = MLR_Filtered_data_pp,na.action = na.omit)


#full model
full_model_PP<-step(LM_all_pp2,direction = "both",details=TRUE)
full_model_PP
summary(full_model_PP)

###################################################################################
#7 Assumptions
###################################################################################
par(mfrow=c(2,2))
plot(full_model_PP)
par(mfrow=c(2,2))
plot(full_model_PP)



