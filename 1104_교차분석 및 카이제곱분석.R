
#Set the working directory (you will need to edit this to be the directory where you have stored the data files for this Chapter)
setwd("C:\\Users\\Owner\\Dropbox\\박사과정\\조교\\2019-2\\통계(학부)_플립러닝\\2. 수업설계\\05. 자료 개발\\03. 11-14주차\\06. In-class\\11주차_교차분석")


###데이터 전처리
## gglot2의 내장 데이터 diamonds


getwd()
setwd("C:\\Users\\shizu\\stat_2020")
rm(list=ls())  #이전작업 객체 지우기

install.packages("ggplot2")

library(ggplot2)
data(diamonds)
data.frame(diamonds)

## 교차분석
# 1)빈도분석

table(diamonds$color, diamonds$cut)

# 2) 교차분할표 생성


install.packages("gmodels", dependencies=TRUE)
library(gmodels)

CrossTable(x=diamonds$color, y=diamonds$cut)

.libPaths()

## 3) 카이제곱 검정 :CrossTable() 이용 - 교차분석 + 가설검정
## p값이 통상 0.05 보다 작으면 관련성이 없다. 
## 동일 집단의 두 변인(학력수준과 대학진학 여부)은 서로 관련성이 있는가 없는가?

## 귀무가설 : 부모의 학력수준과 자녀의 대학진학 여부는 서로 관련성이 없다 p >=0.05
 ##대립 가설 : 부모의 학력수준과 자녀의 대학진학 여부는 관련성이 있다. p <0.05(독립적이 않음)

## 데이터 전처리
library(foreign)
df_data <- read.csv("cleanDescriptive.csv", header=TRUE)
df_data

install.packages("dplyr")
library(dplyr)
df_n <- select(df_data, level2, pass2)
names(df_n) <- c("Level", "Pass")

CrossTable(df_n$Level, df_n$Pass, chisq=TRUE)
chisq.test(df_n$Level, df_n$Pass)

