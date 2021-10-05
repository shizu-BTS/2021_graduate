#######################################
###professor Choi, shizu@snu.ac.kr ####
#######################################




#######################################
########dplyr(데이터 전처리)###########
#######################################

install.packages()
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")

install.packages(c("ggplot2", "tibble", "tidyr", "readr", "purrr", "dplyr", "stringr", "forcats"))
library(tibble)
install.packages("tibble", dependencies=TRUE, repos="https://cran.rstudio.com")
install.packages(c("Lahman", "nycflights13", "gapmidner"))
library(tidyverse)
### 1.dplyr소개   ##############################################################
##주로 데이터 전처리 과정에서 자주 사용
##데이터 전처리는 연구의 분석 목적에 따라 데이터를 재구조화, 
 #필터링(특정 변수값을 가진 데이터만 선택), 특정변수만 선택하거나(select),
 #정렬(ordering), 새로운 변수를 만들어(mutate, trasformate)내는 등의 작업임

### 2.chain operator, %>%  #####################################################
##dplyr의 강점은 chain operator를 사용하여 코드를 심플하게 짤수 있다는 점
##chain operator는 "and then"의 문법과 같이 의미로 사용. 원래는 괄호()가 이 기능수행
##직관적인 코딩을 가능하게 하며, 소괄호 갯수 실수를 줄일 수 있음
##단축어: ctrl+shif+M 

##chain oeprator 미사용시
x<-c(30, 20, 10, 0)
sqrt(mean(abs(x)))
##chain oeprator 사용시 (X를 가져오고 and then, absolute value산출 and then~~~)
x %>% 
  abs() %>% 
  mean() %>% 
  sqrt()
  

#특정함수에 대한 도움말 참조
help(abs)

## 숫자형 벡터 처리 함수
# abs(x) : 절대값
# sqrt(x) : 제곱근
# ceiling(x) : x보다 크거나 같은 정수
# floor(x) : x보다 작거나 같은 정수
# truc(x) : 소숫점 이하 절삭
# round(x, digits=3) : 소수점 n자리로 반올림
# log(x, base=n) :밑이 n인 log 
# exp(x) : 지수 변환
# factorial(x) : factorial(3! = 3*2*1)


### 3.dplyr의 주요 기능  #######################################################
### 3.1.filter           #######################################################
##filter는 특정 변수가 특정값을 가지는(예: 성별=여자) 행(row)을 선택하는 기능
##즉 dataset을 횡으로 절단하는 기능


##분석에 앞서서 R 내장 데이터를 불러오자

install.packages("nycflights13")
library(nycflights13)
library(dplyr)
head(flights) # head 자료 수개를 보여줌
flight_df <-data.frame(flights) #data frame으로 변환
#시작하기 전에 데이터구조 파악 필수! : 데이터가 336776개, 19개의 변수가 있는 비행기 출도착 관련
str(flight_df)



##month=2인 자료만 subset
flight_df %>% 
  filter(month==2) %>% 
  count(month)

table(flight_df$month)


##month=2 or day=1  자료만 subset
flight_df %>% 
  filter(month==2 | day==1) %>% #shift+\
  head(5)

##month=2 and day=1  자료만 subset
flight_df %>% 
  filter(month==2, day==1)  #쉼표나 & 모두 사용 가능

##month=2가 아닌 자료만 subset
flight_df %>% 
  filter(month!=2)  #느낌표는 not의 의미(!+=), 등호는 한번만 쓰는것을 주의

##month가 5이상인 자료만 subset
flight_df %>% 
  filter(month >=5)   #  >+=   

##month가 5, 7, 10인 자료만(복수의 조건) subset
flight_df %>% 
  filter(month %in% c(5,7,10))

##na 값 표시 또는 제거 해서 subset
flight_df %>% 
  filter(is.na(month)) #na인 row만 표시

flight_df %>% 
  filter(!is.na(month)) #na가 아닌 row만 표시

##데이터 저장: dplyr의 대부분의 명령어는 row data를 변경하지 않음. 따라서 별도의 데이터 셋으로 저장하는 절차가 필요

filter_flight_df <-
  flight_df %>% 
  filter(month %in% c(5,7,10)) ##filter_flight_df(no of cases 87110으로 줄은 것을 확인)

flight_df %>% 
  filter(month %in% c(5,7,10))->flight_df_filter

### 3.2.select #######################################################
##select는 특정 변수를 선택하는 기능(열(colomn) 선택)
##즉 dataset을 종으로 절단하는 기능

##month, day 변수만 선택해서 저장
select_flight_df<-
  flight_df %>%
  select(month, day)

str(select_flight_df)


##year에서 day까지의 변수만 선택해서 저장 (연속변수 선택)
select_flight_df<-
  flight_df %>%
  select(year:day) 

str(select_flight_df)

##year에서 day까지의 변수만 제외해서 선택해서 저장 
select_flight_df<-
  flight_df %>%
  select(! year:day) #year~day 까지 변수 제외하고 선택
str(select_flight_df)

select_flight_df<-
  flight_df %>%
  select(-c(year, month)) #year, month 변수 제외하고 선택


### 3.3.arrange #######################################################
##arrange는 정렬 기능(특정변수(들) 기준으로 오름차순 또는 내림차순으로 정렬)

##month, day 순으로 오름차순
arrange_flight_df<-
  flight_df %>%
  arrange(month, day) 

##month는 오름차순, day는 내림차순
arrange_flight_df<-
  flight_df %>%
  arrange(month, -day) 
#또는
arrange_flight_df<-
  flight_df %>%
  arrange(month, desc(day))

##month, day 순으로 내림차순
arrange_flight_df<-
  flight_df %>%
  arrange(-month, -day) 
#또는
arrange_flight_df<-
  flight_df %>%
  arrange(desc(month), desc(day))


### 3.4.mutate ######################################################
##mutate는 새로운 변수를 만들어줌(파생변수(derived variables)생성)

#연산을 통해 새로운 변수 생성

str(flight_df)

mutate(newVriableName=operationsOfOldVariables)
flight_df %>%
  mutate(mean_distance=distance/hour, 
         ratio_delay=arr_delay/(hour*60+minute)) -> flght_df_mutate

#ifelse를 활용하여 category변수 생성
#ifelse(조건, 조건이 true일때, 조건이 false)
 flight_df %>%
  mutate(arr_delay_group=ifelse(arr_delay>0, "delay", "no delay"))
         
 
 
#별도 저장을 하지 않으면 명령어 실행시에만 변수가 생성됨
#별도 저장을 하는 명령어를 추가해야 함

 
mutate_flight_df<-
  flight_df %>%
  mutate(arr_delay_group=ifelse(arr_delay>0, "delay", "no delay"))

str(mutate_flight_df)

#별도 저장은 마지막에 써도 가능(화살표는 반대, logic을 생각하면 직관적 이해 가능)
flight_df %>%
  mutate(arr_delay_group=ifelse(arr_delay>0, "delay", "no delay"))->mutate1_flight_df

### 3.5. group_by와 summarise ######################################################
##group_by는 특정 변수로 grouping하는 것 (성별(남/여)에 따라 데이터를 split)
##summarize는 group_by와 쌍으로 자주 쓰임. 그룹별로 특정 변수의 값을 요약(평균, 분산 등)할때 사용
flight_df %>%
  group_by(month)

mutate1_flight_df %>% 
  group_by(arr_delay_group) %>% 
  summarise(max=max(arr_delay),
            min=min(arr_delay), 
            mean=mean(arr_delay), 
            med=median(arr_delay), 
            per25=quantile(arr_delay, 0,25))
            

table(flight_df$arr_delay)
#missing 값이 있어서 제대로 그루핑이 안되고 있음을 확인. filter를 통해 na 값을 없애고 분석해보자

flight_df %>%
  filter(!is.na(arr_delay)) %>%  #na가 아닌 row만 표시
  mutate(arr_delay_group=ifelse(arr_delay>0, "delay", "no delay")) %>% 
  group_by(arr_delay_group) %>% 
  summarise(max=max(arr_delay),
            min=min(arr_delay), 
            mean=mean(arr_delay), 
            med=median(arr_delay), 
            per20=quantile(arr_delay, 0,25))->final
final

##summarise : 요약통계량 계산
#mean(x,na.rm=TRUE) : 결측값제외하고 평균
#median(x,na.rm=TRUE) : 중앙값
#sd(x,na.rm=TRUE) : 표준편차
#min(x,na.rm=TRUE) : 최솟값
#max(x,na.rm=TRUE) : 최대값
#IQR(x,na.rm=TRUE) : 사분위수 : Q3-Q1
#sum(x,na.rm=TRUE) : 합

#n() 관측치 개수 계산, x변수 입력 하지 않음
#n_disinct(x) : 중복없는 유일한 관측치 개수 계산
###실습######
flight_df %>% 
  filter(!is.na(arr_delay)) %>%  #na가 아닌 row만 표시
  mutate(arr_delay_group=ifelse(arr_delay>0, "delay", "no delay")) %>% 
  group_by(arr_delay_group) %>%
  summarise(n=n())

mutate1_flight_df %>% 
  group_by(arr_delay_group) %>% 
  summarise(countofarrdelaygroup=n())

mutate1_flight_df %>% 
  filter(!is.na(arr_delay)) %>% 
  group_by(arr_delay_group) %>% 
  summarise(n=n())
##rename : 변수 이름 변경
rename(dfname, newariable=oldarialbe)

str(mutate1_flight_df)
mutate2_flight_df<- rename(mutate1_flight_df, destination=dest)
str(mutate2_flight_df)


### 3.6. 마치며  ######################################################

## dplyr의 chain operator는 직관적인 사고의 흐름대로 코드를 심플하게 짤수 있다는 장점
## 단, 코드의 순서가 매우 중요함. 우리가 연산을 하는 순서가 다르면 답이 다른것처럼 먼저 작성된 코드 순서대로 
##   실행이 되기 때문에 코드를 짜기 전에 미리 데이터 소팅, 추출, 연산의 순서를 머릿속에 정리할 필요가 있음
## 또한 다른 chain operator는 다른 패키지(예: ggplot2)를 가져와서 쓸수도 있기 때문에 매우 유용함

install.packages('ggplot2')
library(ggplot2)

flight_df %>%
  filter(!is.na(arr_delay)) %>% 
  group_by(month) %>% 
  summarise(max=max(arr_delay)) %>% 
  ggplot(.,aes(x=month, y=max))+geom_line()
