#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 7 of:
#
#Field, A. P. & Miles, J. N. V. (2012). Discovering Statistics Using R:
#-----------------------------------------------------------------------------------------------------------

###2020 fall, Prof. choi#######


#----Set the working directory------
getwd()
setwd("C:\\Users\\shizu\\stat_2020")
rm(list=ls())  #이전작업 객체 지우기


#----Install Packages-----
install.packages("QuantPsyc")
install.packages("car")
install.packages("boot")


#------And then load these packages, along with the boot package.-----
library(QuantPsyc)
library(car)
library(boot)


#Jane superbrain box
pubs<-read.delim("pubs.dat", header = TRUE)

pubReg <- lm(mortality ~ pubs, data = pubs)

summary(pubReg)

## 회귀분석 기본가정 검정
resid(pubReg)
rstandard(pubReg)
rstudent(pubReg)


# 오차의 독립성 검정 durbin-watson
# d=0 & p=1, d=2& p=0, d=4 & p=-1
# see dubinwatson table (# of cases, # of ivs -> d통계량 cutoff)
reg1 = residuals(pubReg)
durbinWatsonTest(reg1)

# 잔차의 등분산성(그림)
par(mfrow=c(2,2))
plot(pubReg)

# 잔차의 정규성 검정
# shapiro-wilks test : 자료의 정규성 검정방식
# H0= 표본의 모집단이 정규분포를 따른다. -> null 가설을 채택.  ->
shapiro.test(reg1)

## 회귀분석 기본 가정 검정을 위한 패키지 : base (cook's distance, leverage값 등)
##outlier탐색
install.packages("base")
library(base)
# cooks distance 
cooks.distance(pubReg)
# leverage 값
hatvalues(pubReg)
#잔차 확인
resid(pubReg)   ##y hat minus y
rstandard(pubReg) # 표준화된 잔차(mu=0, sd=1), 절대값 2 이상이면 이상치
rstudent(pubReg)


# 다중공선성 vif, tolerance
#----Obtaining the VIF---
vif(pubReg) ## 2개 이상의 독립변수인 경우 실행됨 (pubReg는 실행 안됨)

#----The tolerance is 1/VIF---
1/vif(pubReg)  ## 2개 이상의 독립변수인 경우 실행됨 (pubReg는 실행 안됨)





##################################################
#####  1. Simple Linear Regression ###############
##################################################

#----run the command to access the album1 data-----
album1<-read.delim("Album Sales 1.dat", header = TRUE)

#----run the simple linear regression model---
albumSales.1 <- lm(sales ~ adverts, data = album1, na.action=na.fail)   # na.fail(fail with missing value ), na.omit(casewise deletion)
summary(albumSales.1)
sqrt(0.3346) ## pearson correlation coefficient
cor.test(album1$sales, album1$adverts)
#how to interprete coeffecients
##r square 값 -> y 변량의 설명량. y의 총변량을 1이라고 뒀을 때, 모델이 설명하는 량의 비율. y변량의 33프로를 x가 설명한다. 

###################################################
#####  2. Multipe Linear Regression ###############
###################################################

#----access the album2 data----
album2<-read.delim("Album Sales 2.dat", header = TRUE)

#---Run the multiple regression model----
albumSales.2<-lm(sales ~ adverts, data = album2)
albumSales.3<-lm(sales ~ adverts + airplay + attract, data = album2)
summary(albumSales.2)
summary(albumSales.3)

#---We can obtain standardized parameter estimates with the lm.beta() function---
lm.beta(albumSales.3) #how to interprete coefficients pp.281~283

#---Confidence intervals are obtained with the confint() function----
confint(albumSales.3) # include 0 means ~


#----To compare the R2 in two models, use the ANOVA command---
anova(albumSales.2, albumSales.3)
####소논문 쓸때 잘활용 할 수 있음. (변수 투입의 max를 결정하는 데 도움)


###################################################
#####  3. Assesiing the outlier  ## ###############
###################################################
##assessing the outlier


#----Obtain casewise diagnostics and add them to the original data file.---

album2$residuals<-resid(albumSales.3)
album2$standardized.residuals <- rstandard(albumSales.3)
album2$studentized.residuals <- rstudent(albumSales.3)
album2$cooks.distance<-cooks.distance(albumSales.3)
album2$dfbeta <- dfbeta(albumSales.3)
album2$dffit <- dffits(albumSales.3)
album2$leverage <- hatvalues(albumSales.3)
album2$covariance.ratios <- covratio(albumSales.3)

#Save file
write.table(album2, "Album Sales With Diagnostics.dat", sep = "\t", row.names = FALSE)
#look at the data (and round the values)
round(album2, digits = 3)

#residual interpretation pp.268~269
#----List of standardized residuals greater than 2--------------
album2$standardized.residuals>2| album2$standardized.residuals < -2

#---Create a variable called large.residual, which is TRUE (or 1) if the residual is greater than 2, or less than -2.----------
album2$large.residual <- album2$standardized.residuals > 2 | album2$standardized.residuals < -2

#---Count the number of large residuals-------------
sum(album2$large.residual)


#---Display the value of sales, airplay, attract, adverts, and the standardized residual, for those cases which have a residual greater than 2 or less than -2.-------------
album2[album2$large.residual,c("sales", "airplay", "attract", "adverts", "standardized.residuals")]
# interpretation pp.290~291
#-----Cook's distance, leverage and covariance ratio for cases with large residuals.---------
album2[album2$large.residual , c("cooks.distance", "leverage", "covariance.ratios")]

###################################################
#####  4. Assessing Assumption  ###################
###################################################

## Independent errors: For any two observations the residual terms should be uncorrelated : lack of autocorrelation

install.packages("car")
library("car")

#----The Durbin-Watson test is obtained with either dwt() or durbinWatsonTest()---292page
durbinWatsonTest(albumSales.3)
dwt(albumSales.3)

## Multicollinearity tesitng
#----Obtaining the VIF---
vif(albumSales.3)

#----The tolerance is 1/VIF---
1/vif(albumSales.3)

cor.test(album2$adverts, album2$airplay)
#----The mean VIF---
mean(vif(albumSales.3))


##############################################
############5. Creating Dummy Variables#######
##############################################

album100<-read.delim("Album Sales 1.dat", header = TRUE)

album100


###########################
###Dummy coding ###########
###########################

## INDICATOR (a,b,c,d)
## sales 범주변수. a그룹은 100개 미만, b그룹은 100~200, c그룹은 200~300, D그룹은 300이상
##더미변수를 만들려면 n-1개 ->더미 3개 만들고, reference =a그룹
####   d1   d2   d3
####a  0    0     0
####b  1     0    0
####c  0     1    0
####d  0    0     1

album100$sales_d1<-ifelse(album100$sales>=100&album100$sales<200,1,0)
album100$sales_d2<-ifelse(album100$sales>=200&album100$sales<300,1,0)
album100$sales_d3<-ifelse(album100$sales>=300,1,0)

albumSales.4<-lm(adverts ~ sales_d1 + sales_d2 + sales_d3, data = album100)
summary(albumSales.4)
##d2의 회귀계수= 513.70, a그룹과 c 그룹의 y의 차이 (d2=1일때와 0일때의 y의 변화량 when d1 &d3=0)
#---Histogram of studentized residuals---

hist(album2$studentized.residuals)
hist(rstudent(albumSales.3))

#--Plot of residuals against fitted (predicted) values, with a flat line at the mean--
plot(albumSales.3$fitted.values,rstandard(albumSales.3))
abline(0, 0)

#same as above
plot(albumSales.3)

#Publication quality graphs

album2$fitted <- albumSales.3$fitted.values

histogram<-ggplot(album2, aes(studentized.residuals)) + opts(legend.position = "none") + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x = "Studentized Residual", y = "Density")
histogram + stat_function(fun = dnorm, args = list(mean = mean(album2$studentized.residuals, na.rm = TRUE), sd = sd(album2$studentized.residuals, na.rm = TRUE)), colour = "red", size = 1)
ggsave(file = paste(imageDirectory,"07 album sales ggplot Hist.png",sep="/"))

scatter <- ggplot(album2, aes(fitted, studentized.residuals))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red")+ labs(x = "Fitted Values", y = "Studentized Residual") 
ggsave(file=paste(imageDirectory,"07 Album sales ggplot scatter.png",sep="/"))

qqplot.resid <- qplot(sample = album2$studentized.residuals, stat="qq") + labs(x = "Theoretical Values", y = "Observed Values") 
qqplot.resid
ggsave(file=paste(imageDirectory,"07 Album sales ggplot QQ.png",sep="/"))


#---R tends to give values to too many decimal places, you can usefully round these values to 2 decimals.
round(rstandard(albumSales.3), 2)

