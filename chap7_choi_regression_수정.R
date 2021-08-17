#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 7 of:
#
#Field, A. P. & Miles, J. N. V. (2012). Discovering Statistics Using R:
#-----------------------------------------------------------------------------------------------------------

###2018 fall, Prof. choi#######


#----Set the working directory------
setwd("D:/RRR/chap7")


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
resid(pubReg)
rstandard(pubReg)
rstudent(pubReg)

PearsonResidual <- (resid(pubReg)-mean(resid(pubReg)))/sd(resid(pubReg))



##################################################
#####  1. Simple Linear Regression ###############
##################################################

#----run the command to access the album1 data-----
album1<-read.delim("Album Sales 1.dat", header = TRUE)

#----run the simple linear regression model---
albumSales.1 <- lm(sales ~ adverts, data = album1, na.action=na.fail)   # na.fail(fail with missing value ), na.omit(casewise deletion)
summary(albumSales.1)
sqrt(0.3346) ## pearson correlation coefficient
#how to interprete coeffecients


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
lm.beta(albumSales.3) #how to interprete coeffecients pp.281~283

#---Confidence intervals are obtained with the confint() function----
confint(albumSales.3) # include 0 means ~


#----To compare the R2 in two models, use the ANOVA command---
anova(albumSales.2, albumSales.3)


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

## INDICATOR (a,b,c)

album100$sales_d1<-ifelse(album100$sales<100,1,0)
album100$sales_d2<-ifelse(album100$sales>=100&album100$sales<300,1,0)
album100$sales_d3<-ifelse(album100$sales>=300,1,0)



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

