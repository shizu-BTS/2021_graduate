#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 9 of:
#
#Field, A. P. & Miles, J. N. V. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. London Sage
#
#(c) 2011 Andy P. Field & Jeremy N. V. Miles
#-----------------------------------------------------------------------------------------------------------

#----Set the working directory------

setwd("C:\\Users\\Owner\\Documents\\new")

#----Install Packages-----
install.packages("ggplot2")
install.packages("pastecs")
install.packages("WRS")
install.packages("Hmisc")


#------And then load these packages, along with the boot package.-----
library(ggplot2)
library(pastecs)
library(reshape)
library(Hmisc)

library(Rcmdr)
library(WRS)


#Enter data

spiderLong<-read.delim("SpiderLong.dat", header = TRUE)
spiderWide<-read.delim("SpiderWide.dat", header = TRUE)


#computing t-test from means and SDs
##조건표시 : [행, ]=조건에 맞는 행 모두 추출
##[, 열] = 조건에 맞는 열 모두 추출

x1 <- mean(spiderLong[spiderLong$Group=="Real Spider",]$Anxiety)
x2 <- mean(spiderLong[spiderLong $Group=="Picture", ]$Anxiety)
sd1 <- sd(spiderLong[spiderLong $Group=="Real Spider", ]$Anxiety)
sd2 <- sd(spiderLong[spiderLong $Group=="Picture", ]$Anxiety)
n1 <- length(spiderLong[spiderLong $Group=="Real Spider", ]$Anxiety)
n2 <- length(spiderLong[spiderLong $Group=="Picture", ]$Anxiety)

##t를 수동으로 만드는 함수 생성
## sqrt : 제곱근
# abs : 절대값
# pt : the probability that a score is less that the specified t
# paste :  나열된 원소 사이에 공백을 두고 결과값을 출력, sep=" ": 띄어쓰기를 기준으로 각 원소를 연결

ttestfromMeans<-function(x1, x2, sd1, sd2, n1, n2)
{	df<-n1 + n2 - 2
	poolvar <- (((n1-1)*sd1^2)+((n2-1)*sd2^2))/df
	t <- (x1-x2)/sqrt(poolvar*((1/n1)+(1/n2)))
	sig <- 2*(1-(pt(abs(t),df)))
	paste("t(df = ", df, ") = ", t, ", p = ", sig, sep = "")

}

ttestfromMeans(x1, x2, sd1, sd2, n1, n2)



## 기본 데이터 탐색
#(describe the data)
## basic = TRUE : 관측치 개수, null 개수, NA 개수, 최소값, 최대값, 범위, 합
##desc = TRUE : 중앙값, 평균, 분산, 표준편차, 변이계수
##norm = TRUE : 왜도, 첨도, 정규성 검정통계량, 정규성 검정 P-value
##p = 0.90 :  신뢰계수 90% (유의수준 10%) 값 => 90% 신뢰구간은 평균 ± CI.mean.0.9 값
##(위의 예 Price의 90% 신뢰구간은 19.51 ± 1.66)


stat.desc(spiderLong, basic =TRUE, desc = TRUE, norm = TRUE)
#spiderLong$Group 변수는 범주형변수이므로 결측치 처리됨!



## t-test 기본 명령어 구조

## y=NULL: 단일 모집단 평균경우 입력 X
# alternative : 양쪽꼬리 검정(two.sided), 왼쪽검정(less), 오른쪽 검정(graeter)
# mu: 단일 모집단 평균 비교일때 비교 기준이 되는 값
## paire=TRUE : 쌍체비교
## var.equal : 등분산성 충족 여부
## conf.level = 신뢰구간(기본값 0.95)
#na.action = 결측치 배제 여부

t.test(x, y = NULL, alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)


# 단일 모집단 평균 비교
ind.t.test1 <- t.test(spiderLong$Anxiety, mu=43)
ind.t.test1

ind.t.test11 <- t.test(spiderLong$Anxiety, mu=43, conf.level = 0.99)
ind.t.test11


# 두 모집단 평균 비교

#등분산성 가정 검정
var.test(spiderWide$real, spiderWide$picture)

#정규성 가정(paste package)
shapiro.test(spiderWide$real)
shapiro.test(spiderWide$picture)


# 구조1 t.test(y~x, data=데이터 프레임명)
ind.t.test2 <-t.test(Anxiety ~ Group, data = spiderLong)
ind.t.test2

## 구조2 : t.test(x, y, alternative, paried, var.equal, conflevel, na.action)

ind.t.test3 <-t.test(spiderWide$real, spiderWide$picture)
ind.t.test3

ind.t.test3 <-t.test(spiderWide$real, spiderWide$picture, var.equal = FALSE)
ind.t.test3

##쌍체비교
# 구조 1
dep.t.test2<-t.test(Anxiety ~ Group, data = spiderLong, paired = TRUE)
dep.t.test2

#구조2
dep.t.test<-t.test(spiderWide$real, spiderWide$picture, paired = TRUE)
dep.t.test


#----------------------Smart Alex Task 1------------------------------------

penisData<-read.delim("Penis.dat", header = TRUE)



#----------------------Smart Alex Task 2------------------------------------

Field_Hole<-read.delim("Field&Hole.dat", header = TRUE)
