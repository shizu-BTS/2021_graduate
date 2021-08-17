#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 8 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------

getwd()
setwd("C:\\Users\\shizu\\stat_2020")
rm(list=ls())  #이전작업 객체 지우기nstall Packages-----
install.packages("car")
install.packages("mlogit")



#------And then load these packages, along with the boot package.-----

library(car)
library(mlogit)


#********************* Eel Example ********************

#load data
eelData<-read.delim("eel.dat", header = TRUE)

#look at first 6 cases of data
head(eelData)


# speclevels(eelData$Cured)flevels(eelData$Intervention)

y the baseline category
eelData(reference group=0)#####$# default : 알파벳 순서##
Cured<-relevel(factor(eelData$Cured, levels = c("Not Cured", "Cured"))
levels(eelData$Cured)
eelData$Cured<-eelData$Cured, "Not CureNot eelData$levels(eelData$Cured)
eelData$Cured<-relevel(eelData$Cured, "Cured")
levels(eelData$Cured)
eelData$Cured<-relevel(eelData$Cured, "Not Cured")
levels(eelData$Cured)

eelData$Intervention<-factor(eelData$Intervention, levels = c("No Treatment", "Intervention"))
Intervention<-relevel(eelData$Intervention, "No Treatment")


#Alterlevels(eelData$Intervention)na the two hierarchical models:

eelModel.1 <- glm(Cured ~ Intervention, data = eelData, family = binomial())
eelModel.2 <- glm(Cured ~ Intervention + Duration, data = eelData, family = binomial())

summary(eelModel.1)
summary(eelModel.2)

#Just to prove what the null deviance is
eelModel.0 <- glm(Cured ~ 1, data = eelData, family = binomial())
summary(eelModel.0)


modelChi <- eelModel.1$null.deviance - eelModel.1$deviance
chidf <- eelModel.1$df.null - eelModel.1$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob



#Compute odds ratio
exp(eelModel.1$coefficients)

#Computexp(eelModel.2$coefficients)e confidence intervals
exp(confint(eelModel.1))

#compare model1 and model 2
modelChi <- eelModel.1$deviance - eelModel.2$deviance
chidf <- eelModel.1$df.residual - eelModel.2$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob

anova(eelModel.1, eelModel.2)


#Diagnostics for model 1

eelData$predicted.probabilities<-fitted(eelModel.1)
eelData$standardized.residuals<-rstandard(eelModel.1)
eelData$studentized.residuals<-rstudent(eelModel.1)
eelData$dfbeta<-dfbeta(eelModel.1)
eelData$dffit<-dffits(eelModel.1)
eelData $leverage<-hatvalues(eelModel.1)

head(eelData[, c("Cured", "Intervention", "Duration", "predicted.probabilities")])
eelData[, c("leverage", "studentized.residuals", "dfbeta")]



#----- Testing multicollinearity ------

vif(eelModel.2) 
1/vif(eelModel.2)