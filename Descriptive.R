
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

load(" Assign_Explore-21F.RData")
print( Assign_Explore)


###################################################
#Q.1-1-a
###########################################################################

T_income_marital_PP<-aggregate( Assign_Explore$income,
by=list(m_sts= Assign_Explore$m.status),FUN=sum)
names(T_income_marital_PP)=c('M_Status','Total_Income')
print(T_income_marital_PP)


##############################################
#Q.1-1-b
#############################################

MAXT_income_marital_PP<-T_income_marital_PP[T_income_marital_PP$Total_Income==max(T_income_marital_PP$Total_Income),c('M_Status','Total_Income')]
MAXT_income_marital_PP

##############################################

##############################################
#Q-1-2-a
#############################################

nation_age_mean_PP<-tapply( Assign_Explore$age,  Assign_Explore$nation, mean)

nation_age_mean_PP['Asia']


################################################################
#Q-1-2-b
##################################################################

Asia_resp<-as.data.frame( Assign_Explore[ Assign_Explore$nation=='Asia'
,c("nation", "age","n.child")])

names(Asia_resp)=c("nation", "age","no_of_child")

Asia_resp_rounded<-round(weighted.mean(Asia_resp$age, w=Asia_resp$no_of_child),2)
Asia_resp_rounded

############################################################
#Q-1-3-a
##############################################################

Poli_aware<-aggregate( Assign_Explore$score,
by=list( Assign_Explore$gender),FUN =mean ,na.rm=TRUE)

names(Poli_aware)=c("Gender","mean_score")

Poli_aware

#or 

tapply( Assign_Explore$score, Assign_Explore$gender,mean)
##############################################################
#Q-1-3-b
##############################################################

Poli_aware[Poli_aware$mean_score==max(Poli_aware$mean_score),
c('Gender','mean_score')]

##############################################################
#Q-1-4
##############################################################

quantile( Assign_Explore$time1,c(0.34,0.63))

##############################################################
#
#
#
#   2.   Organizing Data
#
#############################################################
#Q-2-1-a
##############################################################

piedata=data.frame(table( Assign_Explore$political))

pie(piedata$Freq, labels = c(piedata$Freq,piedata$Var1),
main = "Number of respondents by Political Affiliation",
col = rainbow(length(piedata$Freq)))

legend("topright",legend, piedata$Var1, cex = 0.8,
fill = rainbow(length(piedata$Freq)),title="Political Affiliation")

##############################################################
#Q-2-1-b 
##############################################################

names(piedata)=c('Political','pol_percent') # already create above  just changing the col name

piedata[piedata$pol_percent==max(piedata$pol_percent),
           c('Political','pol_percent')]


##############################################################
#Q-2-1-c
##############################################################
##############################################################

piedata[piedata$pol_percent==min(piedata$pol_percent),
        c('Political','pol_percent')]

##############################################################
# Summary table
#2-2-a. 
##############################################################

summ_table_PP=table( Assign_Explore$nation, Assign_Explore$group)

sum_pp<-as.data.frame(prop.table(summ_table_PP,2)*100)

names(sum_pp)<-c('nation','group','percentage')

s_pp=sum_pp[sum_pp$group=='treat',c('nation','group','percentage')]

s_pp

##############################################################
#2-2-b. 
##############################################################

s_pp[s_pp$percentage==max(s_pp$percentage),
        c('nation','group','percentage')]

##############################################################
#2-2-c. 
##############################################################

s_pp[s_pp$percentage==min(s_pp$percentage),
     c('nation','group','percentage')]

##############################################################
#Q-2-3-a 
##############################################################

pol_data_PP<-tapply( Assign_Explore$scr, Assign_Explore$nation,mean)

pol_data_PP

barplot(pol_data_PP,                                                                                                                                                                                                                                                                         
        main="mean score by region",
        xlab = "Nation",
        ylab="Mean Score")

##############################################################
#Q-2-3-b
##############################################################

bar_max_min_pp<-aggregate.data.frame( Assign_Explore$scr, by=list(
nation= Assign_Explore$nation),FUN=mean)

names(bar_max_min_pp)<-c('Nation','Percentage')

bar_max_min_pp[bar_max_min_pp$Percentage==min(bar_max_min_pp$Percentage),
            c('Nation','Percentage')]

##############################################################
#Q-2-3-c
##############################################################

bar_max_min_pp[bar_max_min_pp$Percentage==max(bar_max_min_pp$Percentage),
            c('Nation','Percentage')]

##############################################################
#Q-2-4-a
##############################################################

Hist_pp=hist( Assign_Explore$food*100,breaks=5, main="Percentage of household income going to food",
xlab='Food in percentage',col="red")

################################################################
#Q-2-4-b
##############################################################

max(Hist_pp$count)

##############################################################
#Q-2-5-a
##############################################################

box_pp<-boxplot(income~m.status,data = Assign_Explore, main="Distribution of 
income separated by marital status",xlab="Marriage status",pch=20)

##############################################################
#Q-2-5-b
##############################################################
max(box_pp$conf[1,])
##############################################################
#Q-2-5-c
##############################################################
min(box_pp$conf[1,])
##############################################################
#Q-2-6-a
##############################################################

hist( Assign_Explore$income, main="histogram for income."
,col = "red", xlab ="Total income")
##############################################################
#Q-2-6-b
##############################################################
hist( Assign_Explore$scr, main="histogram for Standardized Score Test."
     ,col = "red", xlab ="Total standardized Score")

##############################################################
#Q-2-6-c
##############################################################

plot(scr ~ income, data= Assign_Explore, main="scr by Income")


##############################################################
#Q-2-6-d
##############################################################

#ans in docx
##############################################################
#Q-2-6-e
##############################################################
cor( Assign_Explore$scr, Assign_Explore$income)
#0.25 ??? ???? < 0.50 Weak linear relationship
#The correlation coefficient of income and standardized scr is 0.4568418.
#Since it is between 0.25 ??? o.456 < 0.50, we can conclude that the variables are 
#in Weak linear relationship.



##############################################################
#Q-3-1-a
##############################################################

qqnorm( Assign_Explore$score, main="Political Awareness Test 
Score.")
qqline( Assign_Explore$score)


##############################################################
#Q-3-1-b
##############################################################

shapiro.test( Assign_Explore$score)


##############################################################
#Q-3-1-c
##############################################################
#yes, it is very close to normal distribution.
#Because the points are on straight line(it should fall on straight line through
#first and third quartiles) according to the value of W=0.99 approximatley 1 and
#p value is greater than .05 a non significant evidence against H. . 
#

##############################################################
#Q-3-2-a
##############################################################
var.test(score ~ group, data =  Assign_Explore)

Ttest_PP<-t.test(score ~ group, data =  Assign_Explore, var.equal = TRUE)
Ttest_PP

#p-value = 0.1419 > 0.05 ,therefore no significant difference in means
#Strong Evidence that  differences are not significant.



##############################################################
#Q-3-2-b
##############################################################
#Because the data is continuous, independent, normally distributed
#and Variance is unknown, but equal as 1(below is the variance test).
var.test(score ~ group, data =  Assign_Explore)


##############################################################
#Q-3-2-c
##############################################################
#Strong Evidence that differences are not significant.

##############################################################
#Q-3-3-a
##############################################################
ANOVA_SN_PP=aov(score ~ nation, data =  Assign_Explore)
ANOVA_SN_PP
summary(ANOVA_SN_PP)
boxplot(score ~ nation, data =  Assign_Explore,
        main="Political Awareness Test varies 
by Region",
        range=0)



##############################################################
#Q-3-3-b
##############################################################
ANOVA_PP_PP=aov(Pol ~ political, data =  Assign_Explore)
ANOVA_PP_PP
summary(ANOVA_PP_PP)
boxplot(Pol ~ political, data =  Assign_Explore,
        main="Measure of Political Involvement (Pol) 
varies by Political Affiliation",
        range=0)



  

