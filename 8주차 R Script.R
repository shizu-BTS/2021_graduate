#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 9 of:
#
#Field, A. P. & Miles, J. N. V. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. London Sage
#
#(c) 2011 Andy P. Field & Jeremy N. V. Miles
#-----------------------------------------------------------------------------------------------------------

#----Set the working directory------

getwd()
setwd("C:\\Users\\shizu\\stat_2020")
rm(list=ls())  #이전작업 객체 지우기

#----Install Packages-----
install.packages("ggplot2")
install.packages("pastecs")
install.packages("Hmisc")


#------And then load these packages, along with the boot package.-----
library(ggplot2)
library(pastecs)
library(reshape)
library(Hmisc)
library(pastecs)

#DATA
spiderLong<-read.delim("spiderLong.dat", header = TRUE)
spiderWide<-read.delim("spiderWide.dat", header = TRUE)


#exploring data
#by(variable, group, output)
#stat.desc(pastecs packages), summary(), describe() (psych packages)
#basic=true(case #, null #, na #, min, max, rage, sum)
#desc=true(median, mean, variance, sd)
#norm=true(skew, kurt, normtest, norm test.p)

by(spiderLong$Anxiety, spiderLong$Group, stat.desc, basic=FALSE, norm=TRUE)

#draw an Error Bar chart:
spiderLongBar <- ggplot(spiderLong, aes(Group, Anxiety))
spiderLongBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs(x = "group", y = "anxiety")

#t-test

#long dataframe
ind.t.test<-t.test(Anxiety ~ Group, data = spiderLong, paired=FALSE)
ind.t.test

#wide dataframe
ind.t.test<-t.test(spiderWide$real, spiderWide$picture, Paired=FALSE)
ind.t.test


#t-test as a GLM
t.test.GLM<-lm(Anxiety ~ Group, data = spiderLong)
summary(t.test.GLM)

1+1

#Effect sizes
ind.t.test$statistic
ind.t.test$parameter
t<-ind.t.test$statistic[[1]]
t
df<-ind.t.test$parameter[[1]]
df
r <- sqrt(t^2/(t^2+df))
round(r, 3)

#dependent t test : compares two means, when these means have come from different groups of entitites(?????????????????? ??￥ ????, ???????????? ????????)

#independent t test : compares two means, when these means have come from one groups of entitites(12???????????? ?????????? ??￥????, ?????????????? ???? ????)

stat.desc(spiderWide, basic = FALSE, norm = TRUE)

dep.t.test2<-t.test(Anxiety ~ Group, data = spiderLong, paired = TRUE)
dep.t.test2

dep.t.test<-t.test(spiderWide$real, spiderWide$picture, paired = TRUE)
dep.t.test

#Effect sizes:
dep.t.test$statistic
dep.t.test$parameter
t<-dep.t.test$statistic[[1]]
df<-dep.t.test$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)

######################################chap 10 anova###########################333

#Install packages

install.packages("car")
install.packages("effects")
install.packages("compute.es")
install.packages("multcomp")
install.packages("granova")
install.packages("pastecs")

library(car)
library(effects)
library(compute.es)
library(multcomp)
library(granova)
library(ggplot2)
library(pastecs)

getwd()
setwd("C:\\Users\\shizu\\stat_2020")
rm(list=ls())  #이전작업 객체 지우기


#--------Viagra data----------

id<-(1:15)
libido<-c(3,2,1,1,4,5,2,4,2,3,7,4,5,3,6)
#dose<-c(rep(1,5),rep(2,5), rep(3,5))
#dose<-factor(dose, levels = c(1:3), labels = c("Placebo", "Low Dose", "High Dose"))
dose<-gl(3,5, labels = c("Placebo", "Low Dose", "High Dose"))
viagraData<-data.frame(dose, libido)

#Graph
line <- ggplot(viagraData, aes(dose, libido))
line + stat_summary(fun.y = mean, geom = "line", size = 1, aes(group=1), colour = "#FF6633")+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 0.75, colour = "#990000") + 
  stat_summary(fun.y = mean, geom = "point", size = 4, colour = "#990000") + 
  stat_summary(fun.y = mean, geom = "point", size = 3, colour = "#FF6633") + labs(x = "Dose of Viagra", y = "Mean Libido")

#Descriptives
by(viagraData$libido, viagraData$dose, stat.desc)

#Levene's test
#등분산검정 결과 (각집단의 분산이 동일한지?H1=집단의 분산이 다르다)
leveneTest(viagraData$libido, viagraData$dose, center = median)


#ANOVA
viagraModel<-aov(libido~dose, data = viagraData)
viagraModel
#viagraModel<-lm(libido~dose, data = viagraData)

summary(viagraModel)
summary.lm(viagraModel)
plot(viagraModel)








