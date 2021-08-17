## 3주차
## [복습]

getwd()
setwd("C:\\Users\\Owner\\Documents\\new") ##자료 저장 directory 설정



install.packages("readxl")
install.packages("foreign")
library(readxl)
library(foreign)

## 1) 자료 입력 및 출력
a <- c("love", "like", "hate", "dislike")
df_a <- data.frame(a)

data_csv <- read.table("data_csv.csv", header = T, sep=",") 
data_excel <- read_excel("data_excel.xlsx")

data_spss <- read.spss("data_sav.sav", use.value.labels=T, to.data.frame=T)
            
write.table(data_csv, "data_csv2.csv", sep=",", row.names = F, quote=F)
writexl::write_xlsx(data_excel, "data_excel2.xlsx")

write.foreign(data_spss, "data_spss2.dat", "data_spss2.sav", package="SPSS")

## 2) 변수생성
data_csv$score_n[data_csv$score > 80] <- 1

data_csv$type_n <- c(4,5,6)[match(data_csv$type, c(1,2,3))]

###[1] charcter로 변환
data_csv$score2 <- as.character(data_csv$score2)

###[2] 쉼표 제거 : gsub(“제거할 내용“, “제거방식”, 객체$변수)
data_csv$score2 <- gsub(",", "", data_csv$score2)

###[3] numeric으로 변환
data_csv$score2 <- as.numeric(data_csv$score2)


data_csv$score2_n <- ifelse(data_csv$score2>900, 1, 0)

data_csv$score2_n <- ifelse(data_csv$score2>900, 2, ifelse(data_csv$score2>800, 1, 0))
                            

## 3) 자료 통계량
View(data_csv)
str(data_csv)
summary(data_csv)
summary(data_csv$score)


table(data_csv$edu)
addmargins(table(data_csv$edu))

table(data_csv$edu, data_csv$employment)
addmargins(table(data_csv$edu, data_csv$employment))

### 4) 자료 추출 및 정리(dplyr 패키지)

install.packages(c("dplyr", "hflights"))

library(dplyr)
library(hflights)

dim(hflights)  
hflights_df <- tbl_df(hflights)
hflights_df

filter(hflights_df, Month==1, DayofMonth==1)

filter(hflights_df, Month==1 | Month==2)
select(hflights_df, Year, Month, DayOfWeek)

select(hflights_df, Year:DayOfWeek)
select(hflights_df, -(Year:DayOfWeek))
mutate(hflights_df, gain= ArrDelay - DepDelay, gain_per_hour = gain/(AirTime/60))

arrange(hflights_df, ArrDelay, Month, Year)
arrange(hflights_df, desc(Month))

summarise(hflights_df, delay=mean(DepDelay, na.rm = TRUE))

a1 <- group_by(hflights, Year, Month, DayofMonth)
a2 <- select(a1, Year:DayofMonth, ArrDelay, DepDelay)
a3 <- summarise(a2, arr= mean(ArrDelay, na.rm = TRUE), 
                dep = mean(DepDelay, na.rm= TRUE))
a4 <- filter(a3, arr >30 | dep >30)

hflights_df %>%
  group_by(Year, Month, DayofMonth) %>%
  summarise(arr= mean(ArrDelay, na.rm=TRUE), 
            dep = mean(DepDelay, na.rm=TRUE)) %>%
  filter(arr>30 | dep >30)


### 5) 자료 결합 및 변환
##merge
wg1998 <- data.frame(matrix(c(1,2,3,0,2,1), nrow=3)) 
names(wg1998) <- c("pid", "wage1998") 

wg1999 <- data.frame(matrix(c(1,2,3,1,1,2), nrow=3)) 
names(wg1999) <- c("pid", "wage1999")

mg1 <- merge(wg1998, wg1999, by="pid") 
mg1

###cbind
wg1998 <- data.frame(matrix(c(1,2,3,0,2,1), nrow=3)) 
names(wg1998) <- c("pid", "wage1998") 

wg1999 <- data.frame(matrix(c(4,5,6,1,1,2), nrow=3)) 
names(wg1999) <- c("pid", "wage1999")

cb1 <- cbind(wg1998, wg1999)
cb1

###rbind
wg1998 <- data.frame(matrix(c(1,2,3,0,2,1), nrow=3)) 
names(wg1998) <- c("pid", "wage1998") 

wg1999 <- data.frame(matrix(c(1,2,3,1,1,2), nrow=3)) 
names(wg1999) <- c("pid", "wage1999")

names(wg1998)[names(wg1998)=="wage1998"] <- "wage" 
names(wg1999)[names(wg1999)=="wage1999"] <- "wage"

wg1998$year <- 1998 
wg1999$year <- 1999

rb1 <- rbind(wg1998, wg1999)
rb1

###reshape
wf1 <- reshape(rb1, direction="wide", timevar="year", idvar="pid") 
wf1 

lf1 <- reshape(mg1, direction="long", varying=c("wage1998", "wage1999"), sep="", timevar="year", idvar="pid") 
lf1 


### 6) 작업공간 저장 및 객체 지우기
save.image("a.RData")
load("a.RData")

ls()
rm(wf1)
rm(list=ls())