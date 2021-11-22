
#R markdown 에서 pdf 생성을 위한 LaTex 설치
tinytex::install_tinytex()

#
dev.off()


id<-c(1:20)
weight<-c(72, 72, 70, 43, 48, 54, 51, 52, 73, 45, 60, 62, 64, 47, 51, 74, 88, 64, 56, 56)
height<-c(176, 172, 182, 160, 163, 165, 168, 163, 182, 148, 170, 166, 172, 160, 163, 170, 182, 174, 164, 160)
df<-cbind(id, weight, height)
df_df<- data.frame(df)
library(dplyr)

df_df %>%
  mutate(y_hat=130.678+0.621*weight) -> df_hat

df_hat %>% 
  mutate(residual=height-y_hat) ->df_hat


y(height)=beta_0 +beta_1 * wight + residual


reg <- 
  lm(height~weight, data=df_df)
summary(reg)

cor.test(height, weight, data=df_df)
sqrt(0.7297)
lm.beta(reg)

#####  1. Simple Linear Regression ###############
##################################################

#----run the command to access the album1 data-----
album1<-read.delim("Album Sales 1.dat", header = TRUE)

#----run the simple linear regression model---
albumSales.1 <- lm(sales ~ adverts, data = album1, na.action=na.fail)   # na.fail(fail with missing value ), na.omit(casewise deletion)
summary(albumSales.1)
sqrt(0.3346) ## pearson correlation coefficient
#how to interprete coeffecients
