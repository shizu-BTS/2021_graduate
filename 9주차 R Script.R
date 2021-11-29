#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 9 of:
#
#Field, A. P. & Miles, J. N. V. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. London Sage
#
#(c) 2011 Andy P. Field & Jeremy N. V. Miles
#-----------------------------------------------------------------------------------------------------------

#----Set the working directory------

setwd("C:\\")
getwd()


######################################chap 10 anova###########################333

#Install packages

install.packages("car")
install.packages("effects")
install.packages("compute.es")
install.packages("multcomp")
install.packages("granova")

library(car)
library(effects)
library(compute.es)
library(multcomp)
library(granova)
library(ggplot2)
library(pastecs)

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
leveneTest(viagraData$libido, viagraData$dose, center = median)


#ANOVA
viagraModel<-aov(libido~dose, data = viagraData)

#viagraModel<-lm(libido~dose, data = viagraData)

summary(viagraModel)
summary.lm(viagraModel)
plot(viagraModel)








