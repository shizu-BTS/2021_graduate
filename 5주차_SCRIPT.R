getwd()
setwd("C:\\Users\\Owner\\Documents\\new")
setwd("C:\\Users\\Owner\\Dropbox\\박사과정\\조교\\2019-2 플립러닝\\2. 수업설계\\05. 자료 개발\\01. 2-5주차\\05. 강의 활용 자료\\5주차")


install.packages("foreign")
library(foreign)
a <- read.spss("f11_h_youth.sav", reencode='utf-8', to.data.frame=TRUE)

View(a)
str(a)

install.packages("dplyr")
library(dplyr)


b <- dplyr::select(a, BYSID, GENDER, F11Y02001, F11Y05034, F11Y05048) 
## F11Y02001 : 현재 학력, F11Y05034: 임금근로자 월평균 임금, F11Y05048: 임금근로자 업무만족도

names(b) <- c("ID", "gender", "edu", "income", "sat")       ## 변수명 수정

str(b)
b$edu <- as.numeric(b$edu)

b$edu_n[b$edu<0] <- NA                                                  ## 결측치 처리
b$edu_n[b$edu<=5 & b$edu>0] <- 1                               ## 고졸 이하 (1)
b$edu_n[b$edu>5] <- 2                                                     ## 대졸 이상 (2)

table(b$edu_n)

b$edu_n <- factor(b$edu_n)

## 변수분포(교차표) 확인
table(b$gender, b$edu_n)                                       ## 성별, 학력(새변수) 교차표
addmargins(table(b$gender, b$edu_n))                 ## 합계가 출력되는 교차표


summary(b$income)

b$income[b$income<0] <- NA
summary(b$income)


## 그래프 그리기

library(ggplot2)

b_gg <- ggplot(data=b, aes(x= edu, y=income))
b_gg + geom_point()

b_gg1 <- ggplot(data=b, aes(x= gender, y=income))
b_gg1 + geom_boxplot()

b_gg2 <- ggplot(data=b, aes(x= edu_n, y=income))
b_gg2 + geom_boxplot()


b_gg3 <- ggplot(data=b, aes(x=income))
b_gg3 + geom_histogram()
                
b_gg4 <- ggplot(data=b, aes(x= edu, y=income))
b_gg4 + stat_summary(fun.y=mean, geom="bar")


b_gg5 <- ggplot(data=b, aes(x=edu, y=income))
b_gg5 + stat_summary(fun.y=mean, geom="line")

b_gg5 + stat_summary(fun.y=mean, geom="line") + stat_summary(fun.y=mean, geom="point")
