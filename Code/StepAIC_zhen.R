setwd("~/Desktop/stat450/seniorworkers/senior worker/Code")
##data_priv: DATA SUMMARY
newdata$pub_priv <- factor(newdata$pub_priv)
data_priv <- newdata[newdata$pub_priv ==1 & (!is.na(newdata$pub_priv)), ] #1062

#1. categorical factor
data_priv$GENDER_R <- as.factor(data_priv$GENDER_R)
data_priv$ED_Level <- as.factor(data_priv$ED_Level)
data_priv$Full_part <- as.factor(data_priv$Full_part)
data_priv$NFE12 <- as.factor(data_priv$NFE12)
data_priv$FNFE12JR <- as.factor(data_priv$FNFE12JR)
data_priv$FNFAET12NJR <- as.factor(data_priv$FNFAET12NJR)
data_priv$FNFAET12JR <- as.factor(data_priv$FNFAET12JR)

#2. mean/barplot  or boxplots
library(ggplot2)
##gender
boxplot(pvnumM ~ GENDER_R, data = data_priv)
boxplot(pvlitM ~ GENDER_R, data = data_priv)
table(data_priv$GENDER_R) #male:658  #female:404

mean(data_priv[data_priv$GENDER_R == 1,]$pvlitM) #252.76
mean(data_priv[data_priv$GENDER_R == 2,]$pvlitM) #239.05

mean(data_priv[data_priv$GENDER_R == 1,]$pvnumM) #245.04
mean(data_priv[data_priv$GENDER_R == 2,]$pvnumM) #227.47

##ED_Level
unique(data_priv$ED_Level)
nrow(data_priv[data_priv$ED_Level == 8,]) #only one observation, changes it to 4
data_priv <- within(data_priv, ED_Level[ED_Level==8]<-4 )

ggplot(data=data_priv, aes(x=ED_Level, y=pvlitM)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  labs(x="Education Level",y="Average Admission Rate",size = 8) +
  ggtitle("Bar Plots for Education Level (Literature Scores)") 
#increase from 1-4(middle school - graduate)

ggplot(data=data_priv, aes(x=ED_Level, y=pvnumM)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  labs(x="Education Level",y="Average Admission Rate",size = 8) +
  ggtitle("Bar Plots for Education Level (Numeracy Scores)")
#increase from 1-4

##Full_part 
boxplot(pvnumM ~ Full_part, data = data_priv)
boxplot(pvlitM ~ Full_part, data = data_priv)
table(data_priv$Full_part) #899full; 163part

mean(data_priv[data_priv$Full_part == 1,]$pvlitM) #248.95
mean(data_priv[data_priv$Full_part == 2,]$pvlitM) #239.76

mean(data_priv[data_priv$Full_part == 1,]$pvnumM) #239.85
mean(data_priv[data_priv$Full_part == 2,]$pvnumM) #230.10


##NFE12: Non-formal education 
boxplot(pvnumM ~ NFE12, data = data_priv)
boxplot(pvlitM ~ NFE12, data = data_priv)
table(data_priv$NFE12) #0:673; 1:389

mean(data_priv[data_priv$NFE12 == 0,]$pvlitM) #242.11
mean(data_priv[data_priv$NFE12 == 1,]$pvlitM) #256.93

mean(data_priv[data_priv$NFE12 == 0,]$pvnumM) #231.11
mean(data_priv[data_priv$NFE12 == 1,]$pvnumM) #250.88

##FNFE12JR: Formal&non-fomal education(job-related)
boxplot(pvnumM ~ FNFE12JR, data = data_priv)
boxplot(pvlitM ~ FNFE12JR, data = data_priv)
table(data_priv$FNFE12JR) #0:757; 1:305

mean(data_priv[data_priv$FNFE12JR == 0,]$pvlitM) #242.91
mean(data_priv[data_priv$FNFE12JR == 1,]$pvlitM) #256.57

mean(data_priv[data_priv$FNFE12JR == 0,]$pvnumM) #233.42
mean(data_priv[data_priv$FNFE12JR == 1,]$pvnumM) #250.60


##FNFAET12NJR: Formal&non-formal Adult Education(non-job-related)
boxplot(pvnumM ~ FNFAET12NJR, data = data_priv)
boxplot(pvlitM ~ FNFAET12NJR, data = data_priv)
table(data_priv$FNFAET12NJR) #0:975; 1:87

mean(data_priv[data_priv$FNFAET12NJR == 0,]$pvlitM) #246.53
mean(data_priv[data_priv$FNFAET12NJR == 1,]$pvlitM) #258.93

mean(data_priv[data_priv$FNFAET12NJR== 0,]$pvnumM) #237.02
mean(data_priv[data_priv$FNFAET12NJR == 1,]$pvnumM) #253.26

##FNFAET12JR: Formal&non-formal Adult Education(job-related)
boxplot(pvnumM ~ FNFAET12JR, data = data_priv)
boxplot(pvlitM ~ FNFAET12JR, data = data_priv)
table(data_priv$FNFAET12JR) #0:757; 1:305

mean(data_priv[data_priv$FNFAET12JR == 0,]$pvlitM) #243.91
mean(data_priv[data_priv$FNFAET12JR == 1,]$pvlitM) #256.57

mean(data_priv[data_priv$FNFAET12JR== 0,]$pvnumM) #233.42
mean(data_priv[data_priv$FNFAET12JR == 1,]$pvnumM) #250.60

##FNFAET12JR: Formal&non-formal Adult Education
boxplot(pvnumM ~ FNFAET12, data = data_priv)
boxplot(pvlitM ~ FNFAET12, data = data_priv)
table(data_priv$FNFAET12) #0:670; 1:392

mean(data_priv[data_priv$FNFAET12 == 0,]$pvlitM) #241.954
mean(data_priv[data_priv$FNFAET12 == 1,]$pvlitM) #257.092

mean(data_priv[data_priv$FNFAET12== 0,]$pvnumM) #230.844
mean(data_priv[data_priv$FNFAET12 == 1,]$pvnumM) #251.191

##3. "FNFAET12" "FNFAET12JR" "FNFAET12NJR" t-test:
#1)pvlitM
t.test(pvlitM~FNFAET12, data = data_priv) #p-value = 1.476e-10
t.test(pvlitM~FNFAET12JR, data = data_priv) #p-value = 3.648e-07
t.test(pvlitM~FNFAET12NJR, data = data_priv) #p-value = 0.004202

#2)pvnumM
t.test(pvnumM~FNFAET12, data = data_priv) #p-value = 2.381e-14
t.test(pvnumM~FNFAET12JR, data = data_priv) #p-value = 1.005e-09
t.test(pvnumM~FNFAET12NJR, data = data_priv) #p-value = 0.0009316
##they are all statistically significant and FNFAET12 is the union of other two variables.

table(data_priv$FNFE12JR == data_priv$FNFAET12JR)
####They are exactly the same: use one of them

##4.STEPAIC
#1)proficiency test score: literacy
data_priv_lit<- data_priv[,c(-3,-4,-6,-8,-10,-11,-12,-15,-16,-19,-21,-22)]
library(MASS)
full.priv.lit<-lm(pvlitM~.,data=data_priv_lit)
summary(full.priv.lit)
mean((data_priv_lit$pvlitM - predict(full.priv.lit))^2) #1023.82
#cor(data_priv_lit)

##StepAIC
null.priv.lit <- lm(pvlitM~1,data=data_priv_lit)
step.lm <- stepAIC(null.priv.lit, scope=list(lower=null.priv.lit, upper=full.priv.lit), trace=T)
summary(step.lm)
mean((data_priv_lit$pvlitM - predict(step.lm))^2) #1026.57

stepfor.lm <- stepAIC(full.priv.lit, scope=list(lower=null.priv.lit), trace=T) 
summary(stepfor.lm)

#2)numercy
data_priv_num<- data_priv[,c(-3,-4,-6,-8,-10,-11,-12,-15,-16,-18,-21,-22)]
full.priv.num<-lm(pvnumM~.,data=data_priv_num)
summary(full.priv.num)


##StepAIC
null.priv.num <- lm(pvnumM~1,data=data_priv_num)
step.lm2 <- stepAIC(null.priv.num, scope=list(lower=null.priv.num, upper=full.priv.num), trace=T)
summary(step.lm2)

stepfor.lm2 <- stepAIC(full.priv.num, scope=list(lower=null.priv.num), trace=T) 
summary(stepfor.lm2)

#
library(MASS)
step(null.priv.lit, scope = list(lower=null.priv.lit,upper=full.priv.lit),
     direction="both", criterion = "AIC")

step(null.priv.num, scope = list(lower=null.priv.num,upper=full.priv.num),
     direction="both", criterion = "AIC")


step(null.priv.lit, scope = list(lower=null.priv.lit,upper=full.priv.lit),
     direction="both", criterion = "BIC",k=log(nrow(data_priv_lit)))


step(null.priv.num, scope = list(lower=null.priv.num,upper=full.priv.num),
     direction="both", criterion = "BIC",k=log(nrow(data_priv_num)))







