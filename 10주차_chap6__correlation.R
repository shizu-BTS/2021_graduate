#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 6 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------

#Set the working directory (you will need to edit this to be the directory where you have stored the data files for this Chapter)

getwd()
setwd("C:\\Users\\shizu\\stat_2020")
rm(list=ls())  #이전작업 객체 지우기

######Initiate packages

#If you don't have Hmisc installed then use:
install.packages("Hmisc") # 기본+중급수준의 데이터분석, 그래프그리기, clustering 등 가능
install.packages("boot")
install.packages("polycor") # pearson corr, polyseriaal(numeric+ordinal, ordinal+ordinal)
install.packages("ggm") # Markov models 활용
install.packages("corrplot")



#Initiate packages
library(Hmisc)
library(ggplot2)
library(boot)
library(polycor)
library(ggm)
library(Rcmdr)
library(corrplot)


#--------Entering data----------

adverts<-c(5,4,4,6,8)
packets<-c(8,9,10,13,15)
advertData<-data.frame(adverts, packets)

cov(advertData)
var(adverts)
var(advertData$adverts)
cor(advertData)
#--------Self Help Task----------

scatter<-ggplot(advertData, aes(adverts, packets))
scatter + geom_point()
scatter + geom_point(size = 3) + labs(x = "Adverts", y = "Packets") + scale_y_continuous(limits=c(0, 15), breaks=0:15) + scale_x_continuous(limits=c(0, 9), breaks=0:9)




#-----Dealing with misisng cases

adverts<-c(5,4,4,6,8)
packetsNA<-c(8,9,10,NA,15)
age<-c(5, 12, 16, 9, 14)
advertNA<-data.frame(adverts, packetsNA, age)

cor(advertNA, use = "everything",  method = "pearson")  # everything = missing value 있는 경우 상관계수가 NA로 산출됨
cor(advertNA, use = "complete.obs",  method = "pearson")
cor(advertNA, use = "pairwise.complete.obs",  method = "pearson")
cor(advertNA, use = "complete.obs",  method = "kendall") #complete.obs -> mising value가 있는 케이스는 통째로 제거    # pairwise.comple.obs -> 분석에 사용되는 변수만 제거


#--------Pearson r---------- 

### cor(x,y, use = "everything", method = "correlation type")
### cor.test(x,y, alternative = "string", method = "correlation type", conf.level = 0.95)

examData = read.delim("Exam Anxiety.dat",  header = TRUE)
examData
## use= 결측값 처리   everything(default) 결측값이 있으면 NA로 계산, all.obs=결측값 있으면 오류, complete.obs= 결측값이 있는 케이스 모두 제거, pairwise.compete.obs= 상관계수가 계산되는 변수들 가운데 결측값 있는 케이스만 제거

cor(examData)
cor(examData$Exam, examData$Anxiety, use = "complete.obs", method = 'pearson')
examData2 <- examData[, c("Exam", "Anxiety", "Revise")]
cor(examData2)
cor(examData[, c("Exam", "Anxiety", "Revise")])
cor(examData2)^2 * 100  # compute R square

library(Hmisc)
examMatrix<-as.matrix(examData[, c("Exam", "Anxiety", "Revise")]) #convert dataframe into a matrix (only numeric)
Hmisc::rcorr(examMatrix)
Hmisc::rcorr(as.matrix(examData[, c("Exam", "Anxiety", "Revise")]))

rcorr(examMatrix)  # ggm package also has a function rcorr. so put packagename:: in front of command

cor.test(examData$Anxiety, examData$Exam)
cor.test(examData$Revise, examData$Exam)
cor.test(examData$Anxiety, examData$Revise)


#### corrplot 활용하기 : correlation 시각화

library(corrplot)
corMatrix <-cor(examData2)
plot(examData2)
corrplot(corMatrix, method="number")
corrplot(corMatrix, method="circle")
corrplot(corMatrix, method="ellipse")



#--------Spearman's Rho----------# row row row your boat gently down the stream
# if my data are not parametric? non-normaly distributed, ordinal variable...

liarData = read.delim("The Biggest Liar.dat",  header = TRUE)
liarData

liarData = read.delim(file.choose(),  header = TRUE) # use the dialog box and select the file

cor(liarData$Position, liarData$Creativity, method = "spearman")
cor.test(liarData$Position, liarData$Creativity, method = "spearman") # default = two-tailed test
cor.test(liarData$Position, liarData$Creativity, alternative = "less", method = "spearman")  # less : less than zero(left test) #greater: more than zero(righ test)
cor.test(liarData$Position, liarData$Creativity, alternative = "greater", method = "spearman") 
liarMatrix<-as.matrix(liarData[, c("Position", "Creativity")])
rcorr(liarMatrix)

#--------Kendall's Tau---------- # large number of tied ranks, small # of data

cor(liarData$Position, liarData$Creativity, method = "kendall")
cor.test(liarData$Position, liarData$Creativity, alternative = "less", method = "kendall")

cor(advertData$adverts, advertData$packets, method = "pearson")
cor.test(advertData$adverts, advertData$packets, alternative = "greater", method = "kendall")



#-------Partial-----

maleExam<-subset(examData, Gender == "Male", select= c("Exam", "Anxiety"))
femaleExam<-subset(examData, Gender == "Female", select= c("Exam", "Anxiety"))
cor(maleExam)
cor(femaleExam)

library(ggm)
pc<-pcor(c("Exam", "Anxiety", "Revise"), var(examData2)) #(var1, var2, control1, control2)
pc
pc^2
pcor.test(pc, 1, 103)  #object, #of control var, #of case



