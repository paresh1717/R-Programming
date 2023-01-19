
##################################################
rm(list=ls())                                   ##
cat("\014")                                     ##
##
#######################################################################
setwd("not given")##
getwd()                                                              ##  
#######################################################################

#######################################################################
##loading  Data
#######################################################

load("Tumor_21F.RData")// directory path not given 

print(Tumor_21F)
typeof(Tumor_21F)

Tumor_data_pp<-as.data.frame(Tumor_21F)
str(Tumor_data_pp)
stat.desc(Tumor_data_pp)
###################################################################################
#install packages
if(!require(pastecs)){install.packages("pROC")}
if(!require(pastecs)){install.packages("MASS")}
if(!require(pastecs)){install.packages("klaR")}
library(pROC)
library(MASS)
library(klaR)
###################################################################################
#Preliminary Data Preparation
###################################################################################
#1 checking na values and null values
###################################################################################
summary(is.na(Tumor_data_pp))#false 
summary(is.null(Tumor_data_pp))#false
###################################################################################
#2 checking low variance
###################################################################################
stat.desc(Tumor_data_pp)
summary(Tumor_data_pp)
###################################################################################
#3 High correlation filter
###################################################################################
str(Tumor_data_pp)# all numeric
cor(Tumor_data_pp,method="spearman")# no high correlation
###################################################################################
#4 No need of dummies though all the var are numeric
###################################################################################
###################################################################################
#5 Lets check outliers for the data
###################################################################################
boxplot(Tumor_data_pp)


# we need to remove the variables with outliers.
#Tumor_data_pp<-subset(Tumor_data_pp, select = -c(Pleura,Liver,Skin,Neck,Supra,Axil) )


###################################################################################
#2
###############################################################################

Tumor_tbl1_pp=table(Tumor_data_pp$Out
,Tumor_data_pp$Brain
,dnn=list("Out",
"Brain"
))

Tumor_tbl2_pp=table(Tumor_data_pp$Out
,Tumor_data_pp$Marrow
,dnn=list("Out",
          "Marrow"
))

Tumor_tbl1_pp
Tumor_tbl2_pp

prop.table(Tumor_tbl1_pp, 2)
prop.table(Tumor_tbl2_pp, 2)



#Check the Chi Squared Test 
chisqRct1_tumor_pp <- chisq.test(Tumor_data_pp$Out,Tumor_data_pp$Brain,
correct=FALSE)   

chisqRct2_tumor_pp <- chisq.test(Tumor_data_pp$Out,Tumor_data_pp$Marrow,
correct=FALSE)
                    
chisqRct1_tumor_pp
chisqRct2_tumor_pp

chisqRct1_tumor_pp$observed
chisqRct1_tumor_pp$expected

chisqRct2_tumor_pp$observed
chisqRct2_tumor_pp$expected

barplot(prop.table(Tumor_tbl1_pp, 2), xlab='out',ylab='Brain',main=" probability of a tumor 
in brain.",col=c("darkblue","darkred"),legend=rownames(Tumor_tbl1_pp), args.legend = list(x = "topleft"))

barplot(prop.table(Tumor_tbl2_pp, 2), xlab='out',ylab='Brain',main=" probability of a tumor 
in Marrow.",col=c("darkblue","darkred"),legend=rownames(Tumor_tbl1_pp), args.legend = list(x = "topleft"))

###################################################################################
#3 Model Development
###############################################################################

#Model1
start1_time_pp <- Sys.time()
glm1_tumor_pp<-glm(formula = Out~Age+Sex+
Bone+
Marrow+
Lung+
Brain+
Media+Pleura+Liver+Skin+Neck+Supra+Axil,
    family = "binomial", data = Tumor_data_pp, na.action = na.omit)
end1_time_pp <- Sys.time()
summary(glm1_tumor_pp)

stp_tumor_glm <- step(glm1_tumor_pp)
summary(stp_tumor_glm)

#model2(USER MODEL1)
start2_time_pp <- Sys.time()
glm2_tumor_pp<-glm(formula = Out~Age+Sex+
                    Marrow+
                    Lung+
                    Brain+
                    Media+Neck+Supra,
                  family = "binomial", data = Tumor_data_pp, na.action = na.omit)
end2_time_pp <- Sys.time()
summary(glm2_tumor_pp)

#model3(USER MODEL2)
#dropping var on previous results
start3_time_pp <- Sys.time()
glm3_tumor_pp<-glm(formula = Out~Sex+
                    Marrow+
                    Lung+
                    Brain+
                    Media+Neck+Supra,
                  family = "binomial", data = Tumor_data_pp, na.action = na.omit)
end3_time_pp <- Sys.time()

summary(glm3_tumor_pp)

###################################################################################
#4 Model Evaluation
###############################################################################
#MODEL1 
resp_Stp0_pp <- predict(glm1_tumor_pp, type="response")

Class_Stp0_pp <- ifelse(resp_Stp0_pp > 0.5,1,0)           


CNF_UM0_PP<-table(Class_Stp0_pp,Tumor_data_pp$Out,dnn=list("Predicated","Orignal"))

tot=sum(CNF_UM0_PP)
tn=CNF_UM0_PP[1]
fn=CNF_UM0_PP[2]
fp=CNF_UM0_PP[3]
tp=CNF_UM0_PP[4]

CNF_UM0_PP

#a. Accuracy
Accuracy_pp=(tp+tn)/tot
Accuracy_pp
#b. Specificity
Specificity_pp=tn/(tn+fp)
Specificity_pp
#c. Sensitivity
Sensitivity_pp=tp/(tp+fn)
Sensitivity_pp
#d. Precision
Precision_pp=tp/(tp+fp)
Precision_pp

end1_time_pp-start1_time_pp

#ROC CURVE
roc_stp0_pp<-plot(roc(Tumor_data_pp$Out,resp_Stp0_pp, direction="<"),
                  col="red", lwd=2, main='ROC Curve for Logistic, Tumor')

roc_stp0_pp$auc

#USER MODEL1
resp_Stp1_pp <- predict(glm2_tumor_pp, type="response")

Class_Stp1_pp <- ifelse(resp_Stp1_pp > 0.5,1,0)           


CNF_UM1_PP<-table(Class_Stp1_pp,Tumor_data_pp$Out,dnn=list("Predicated","Orignal"))

tot=sum(CNF_UM1_PP)
tn=CNF_UM1_PP[1]
fn=CNF_UM1_PP[2]
fp=CNF_UM1_PP[3]
tp=CNF_UM1_PP[4]

CNF_UM1_PP

#a. Accuracy
Accuracy_pp=(tp+tn)/tot
Accuracy_pp
#b. Specificity
Specificity_pp=tn/(tn+fp)
Specificity_pp
#c. Sensitivity
Sensitivity_pp=tp/(tp+fn)
Sensitivity_pp
#d. Precision
Precision_pp=tp/(tp+fp)
Precision_pp

end2_time_pp-start2_time_pp

#ROC CURVE
roc_stp1_pp<-plot(roc(Tumor_data_pp$Out,resp_Stp1_pp, direction="<"),
     col="red", lwd=2, main='ROC Curve for Logistic, Tumor')

roc_stp1_pp$auc

#uSER MODEL2
# creates probabilities
resp_Stp2_pp <- predict(glm3_tumor_pp, type="response") 

Class_Stp2_pp <- ifelse(resp_Stp2_pp > 0.5,1,0)           



CNF_UM2_PP=table(Class_Stp2_pp,Tumor_data_pp$Out,dnn=list("Predicated","Orignal"))

tot=sum(CNF_UM2_PP)
tn=CNF_UM2_PP[1]
fn=CNF_UM2_PP[2]
fp=CNF_UM2_PP[3]
tp=CNF_UM2_PP[4]

CNF_UM2_PP

#a. Accuracy
Accuracy_pp=(tp+tn)/tot
Accuracy_pp
#b. Specificity
Specificity_pp=tn/(tn+fp)
Specificity_pp
#c. Sensitivity
Sensitivity_pp=tp/(tp+fn)
Sensitivity_pp
#d. Precision
Precision_pp=tp/(tp+fp)
Precision_pp
#ROC CURVE
roc_stp2_pp<-plot(roc(Tumor_data_pp$Out,resp_Stp2_pp, direction="<"),
     col="red", lwd=2, main='ROC Curve for Logistic, Tumor')

roc_stp2_pp$auc

end3_time_pp-start3_time_pp




###################################################################################
#PART B 
###############################################################################
#1
###############################################################################
#Model1
start_time_pp<-Sys.time()
glmB1_tumor_pp<-glm(formula = Out~Age+Sex+
                     Bone+
                     Marrow+
                     Lung+
                     Brain+
                     Media+Pleura+Liver+Skin+Neck+Supra+Axil,
                   family = "binomial", data = Tumor_data_pp, na.action = na.omit)
end_time_pp<-Sys.time()
summary(glmB1_tumor_pp)

start_time1_pp<-Sys.time()
fwd_model_PP<-step(glmB1_tumor_pp,selection = "forward",details=TRUE)
end_time1_pp<-Sys.time()
summary(fwd_model_PP)
#confusion matrices
resp_StpB1_pp <- predict(fwd_model_PP, type="response")

Class_StpB1_pp <- ifelse(resp_StpB1_pp > 0.5,1,0)

Class_StpB1_pp

CF_FWD_Tumor_pp=table(Tumor_data_pp$Out,Class_StpB1_pp,dnn=list("Orignal","Predicated"))
CF_FWD_Tumor_pp

roc_stpB1_pp<-plot(roc(Tumor_data_pp$Out,resp_StpB1_pp, direction="<"),
                  col="red", lwd=2, main='ROC Curve for Logistic, Tumor')


##Calculate time in seconds
model_exe_time_pp<-end_time_pp-start_time_pp
fwdmodel_exe_time_pp<-end_time1_pp-start_time1_pp

#time for glm model
model_exe_time_pp
#time for forward model
fwdmodel_exe_time_pp

fwdmodel_exe_time_pp<model_exe_time_pp




##############################################################################################################################################################
#2
###############################################################################
str(Tumor_data_pp)
# all the variable are numeric

##################################################
### NAIVE BAYES model                                 
##################################################

str(Tumor_data_pp)


Tumor_data_pp$Out<-as.factor(Tumor_data_pp$Out)

startNav_time_pp <- Sys.time()
Tumor_data_Naive_pp <- NaiveBayes(Out~Age+Sex+
                                    Bone+
                                    Marrow+
                                    Lung+
                                    Brain+
                                    Media+Pleura+Liver+Skin+Neck+Supra+Axil,
                          data = Tumor_data_pp, na.action=na.omit)
endNav_time_pp <- Sys.time()



summary(Tumor_data_Naive_pp)
Tumor_data_Naive_pp


#Classifies
pred_tumor_pp <- predict(Tumor_data_Naive_pp,Tumor_data_pp)

#head(pred_tumor_pp)


#Creates Confusion Matrix
CF_NAV_Tumor_pp <- table(Actual=Tumor_data_pp$Out, Predicted=pred_tumor_pp$class)
CF_NAV_Tumor_pp

summary(CF_Tumor_pp)

#time it took to fit the model
NB_Time_pp <- endNav_time_pp - startNav_time_pp
NB_Time_pp


##################################################
### LDA                                         ##
##################################################

start_tum_LDA_time_pp <- Sys.time()
start_tum_LDA_time_pp
LDA_tumor_pp <- lda(Out~Age+Sex+
                      Bone+
                      Marrow+
                      Lung+
                      Brain+
                      Media+Pleura+Liver+Skin+Neck+Supra+Axil,
                    data = Tumor_data_pp, na.action=na.omit)

end_tum_LDA_time_pp <- Sys.time()
end_tum_LDA_time_pp
LDA_tumor_pp

#Classifies
pred_lda_pp <- predict(LDA_tumor_pp, data=Tumor_data_pp)


#Confusion Matrix
CF_LDA_tumor_pp <- table(Actual=Tumor_data_pp$Out, Predicted=pred_lda_pp$class)
CF_LDA_tumor_pp

#time calculated
LDA_tumor_Time_PP <- end_tum_LDA_time_pp - start_tum_LDA_time_pp
LDA_tumor_Time_PP


##################################################
### 4 Comparison od all three models in part B                                
##################################################

#Accuracy
CF_FWD_Tumor_pp
CF_NAV_Tumor_pp
CF_LDA_tumor_pp

#Forward
tot=sum(CF_FWD_Tumor_pp)
tn=CF_FWD_Tumor_pp[1]
fn=CF_FWD_Tumor_pp[2]
fp=CF_FWD_Tumor_pp[3]
tp=CF_FWD_Tumor_pp[4]


fp
fn

#a. Accuracy
Accuracy_pp=(tp+tn)/tot
Accuracy_pp

Precision_pp=tp/(tp+fp)
Precision_pp

fwdmodel_exe_time_pp

#NAV
tot=sum(CF_NAV_Tumor_pp)
tn=CF_NAV_Tumor_pp[1]
fn=CF_NAV_Tumor_pp[2]
fp=CF_NAV_Tumor_pp[3]
tp=CF_NAV_Tumor_pp[4]

fp
fn

#a. Accuracy
Accuracy_pp=(tp+tn)/tot
Accuracy_pp

Precision_pp=tp/(tp+fp)
Precision_pp

NB_Time_pp

#LDA
tot=sum(CF_LDA_tumor_pp)
tn=CF_LDA_tumor_pp[1]
fn=CF_LDA_tumor_pp[2]
fp=CF_LDA_tumor_pp[3]
tp=CF_LDA_tumor_pp[4]

fp
fn

#a. Accuracy
Accuracy_pp=(tp+tn)/tot
Accuracy_pp

Precision_pp=tp/(tp+fp)
Precision_pp

LDA_tumor_Time_PP


