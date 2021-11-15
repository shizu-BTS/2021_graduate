#######################################
###professor Choi, shizu@snu.ac.kr ####
#######################################




#######################################
###########   ggplot2    ##############
#######################################


######Initiate packages

#If you don't have ggplot2 installed then use:
install.packages(c("ggplot2", "plyr", "reshape"))
install.packages("Hmisc")
install.pacakges("Rtool")

#Initiate ggplot2
library(ggplot2)
library(reshape)
library(plyr)
library(dplyr)

##작업공간 설정하기 : r project 사용하지 않는 경우만 ####

getwd()
setwd("C:\\Users\\shizu\\stat_2020")


#--------Scatterplots----------#two variables, continuous

examData <- read.delim("Exam Anxiety.dat",  header = TRUE)
names(examData)
str(examData)


##aes는 그래프별로 별도 지정하거나, 한꺼번에 지정할 수 있음
ggplot(examData, aes(Anxiety, Exam))+
  geom_point()+
  geom_smooth()+
  
  

ggplot(examData)+
  geom_point(aes(x=Anxiety, y=Exam))

ggplot(examData, aes(Anxiety, Exam))+
  geom_point()


#Simple scatter

ggplot(examData)+
  geom_point(aes(x=Anxiety, y=Exam, colour = Gender))

ggplot(examData)+
  geom_point(aes(x=Anxiety, y=Exam), shape=19, size=1, colour="black")

ggplot(examData)+
  geom_point(aes(x=Anxiety, y=Exam, colour = Gender), size=1, shape=10)

#plot symbol(points) +refer to cheetsheet
help(pch)




#Simple scatter / labs(x=“x축 이름”, y=“y축이름“) /ggtitle("title")


ggplot(examData)+ 
  geom_point(aes(x=Anxiety, y=Exam)) + 
  labs(title="exam", x = "Exam Anxiety", y = "Exam Performance %")

#data labeling 
ggplot(examData)+ 
  geom_point(aes(x=Anxiety, y=Exam)) + 
  labs(title="exam", x = "Exam Anxiety", y = "Exam Performance %")+
  geom_text(aes(label=Code, size=10, vjust=2, hjust=0))#vjust : 위로, hjust : 오른쪽

#shape별 value를 알고 싶을때
help(geom_text)


##집단별 색깔을 달리한 scatter plot
scatter <- ggplot(examData, aes(x=Anxiety, y=Exam, colour=Gender))
scatter + 
  geom_point() + 
  labs(x = "Exam Anxiety", y = "Exam Performance %") + 
  ggtitle("exam anxiety")



##집단별 층위를 나누어서 산포도를 그릴때

ggplot(examData)+
  geom_point(aes(x=Anxiety, y=Exam)) + 
  facet_grid(Gender~.)+ #row로 구분. 점의 위치를 확인할것
  labs(title= "exam anxiety", x = "Exam Anxiety", y = "Exam Performance %")

ggplot(examData)+
  geom_point(aes(x=Anxiety, y=Exam)) + 
  facet_grid(.~Gender)+ #column으로 구분
  labs(title= "exam anxiety", x = "Exam Anxiety", y = "Exam Performance %") 
#두변수의 조합으로 면 분할을 하고싶으면 A~B의 형태로 옵션 지정

#Simple scatter with smooth/ 
ggplot(examData, aes(Anxiety, Exam))+
  geom_point() + 
  geom_smooth() +  # 데이터를 잘설명할 수 있는 smooth line 생성
  labs(title= "exam anxiety", x = "Exam Anxiety", y = "Exam Performance %") 


#Simple scatter with regression line(red)
ggplot(examData, aes(Anxiety, Exam))+
  geom_point() + 
  geom_smooth(method = "lm", colour = "Red", se = F) + #se=standard error(표준오차)
  labs(title= "exam anxiety", x = "Exam Anxiety", y = "Exam Performance %") 


#Simple scatter with regression line(red)+ CI(confidential interval(신뢰구간))

ggplot(examData, aes(Anxiety, Exam))+
  geom_point() + 
  geom_smooth(method = "lm", colour = "Red")+ 
  labs(title= "exam anxiety", x = "Exam Anxiety", y = "Exam Performance %") 


#Simple scatter with regression line + coloured CI (alpha=transparancy)
ggplot(examData, aes(Anxiety, Exam))+ 
  geom_point() + 
  geom_smooth(method = "lm", colour = "Red", fill="Red", alpha = 0.1) + 
  labs(title= "exam anxiety", x = "Exam Anxiety", y = "Exam Performance %") 


#Grouped scatter with regression line + CI
ggplot(examData)+
  geom_point(aes(x=Anxiety, y=Exam, colour = Gender)) +
  geom_smooth(method = "lm", color="grey", aes(x=Anxiety, y=Exam, fill = Gender), alpha = 0.1) +
  labs(x = "Exam Anxiety", y = "Exam Performance %", colour = "Gender") 

##(학생실습과제)
##"FacebookNarcissism.dat" 자료로 NPQC_R_Total(X축), Rating(y축)의 산포도 그리기
## 1) 산포도 그리기(Rating_Type에 따라 색상 다르게)
## 2) 산포도와 함께 회귀선 그리기(Rating_Type에 따라 회귀선 색상 다르게, method는 lm으로)
## 3) 그림 title은 "geom_smooth(aes(colour = Rating_Type))" 으로 제시


#--------HISTOGRAM------------------------------------------------------

##Load the data file into R. This is a tab-delimited file hence use of read.delim

festivalData <- read.delim("DownloadFestival.dat",  header = TRUE)
str(festivalData)

##Histogram with Outlier

ggplot(festivalData) + 
  geom_histogram(aes(day1), binwidth = 0.1) + #binwidth를 0.2, 0.3등으로 변경해보자
  labs(legend.position="none")+ 
  labs(x = "Hygiene (Day 1 of Festival)", y = "Frequency")


#1 outlier cased떄문에 그래프가 이상한걸 확인
festivalData %>% 
  arrange(-day1) ->festvalData_a

festivalData2 = read.delim("DownloadFestival(No Outlier).dat",  header = TRUE)

ggplot(festivalData2)+
  geom_histogram(aes(day1), binwidth=0.4, color="red")+
  labs(x="Hygiene of Day 1", y="Frequency")

#Density without outlier(추가)
ggplot(festivalData2)+
  geom_density(aes(day1), color="red") +
  labs(x = "Hygiene (Day 1 of Festival)", y = "Density Estimate")

#집단별로 색깔 다르게, alpha=투명도
ggplot(festivalData2) + 
  geom_density(aes(x=day1, fill = gender), alpha = 0.3) + 
  labs(x = "Hygiene (Day 1 of Festival)", y = "Density Estimate")

ggplot(festivalData2)+
  geom_histogram(aes(day1, fill=gender), alpha=0.3) +
  labs(x = "Hygiene (Day 1 of Festival)", y = "Density Estimate")

##면분할을 해보면(facet_grid)

ggplot(festivalData2)+
  geom_histogram(aes(day1), alpha=0.3) +
  facet_grid(.~gender)
  labs(x = "Hygiene (Day 1 of Festival)", y = "Density Estimate")

#--------BOXPLOTS----------

ggplot(festivalData)+
  geom_boxplot(aes(x=gender, y=day1)) + 
  labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")

#with outlier removed

festivalData2 = read.delim("DownloadFestival(No Outlier).dat",  header = TRUE)

ggplot(festivalData2)+
  geom_boxplot(aes(x=gender, y=day1)) + 
  labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")

##(학생실습과제) days 2 and 3 각각 box plot 그려보기##



#--------Bar Charts(stat_summary)

chickFlick = read.delim("ChickFlick.dat",  header = TRUE)
str(chickFlick)

ggplot(chickFlick)+
  geom_point(aes(film, arousal))+
  labs(x = "Film", y = "Arousal") 

ggplot(chickFlick)+
  geom_boxplot(aes(film, arousal))+
  labs(x = "Film", y = "Arousal") 


ggplot(chickFlick)+
  stat_summary(
    aes(film, arousal), 
    fun.y = mean, 
    geom = "bar", 
    fill = "pink", 
    colour = "Black")+
  geom_point(aes(film, arousal))+
  labs(x = "Film", y = "Arousal") 

#stat과 geom의 관계를 이해. 모든 geom에서 stat을 쓸수도, 모든 stat에서 geom을 쓸수도, 아니면 stat_summary를 이용하는 방법도 있음
str(examData)
ggplot(examData)+
  stat_count(aes(Gender))

ggplot(examData)+
  geom_bar(aes(Gender))

?geom_bar  #기본stat이 count인것을 확인할 수 있음

ggplot(examData)+
  geom_bar(aes(x=Gender, y=Exam), stat="identity")
 

                  브릿지 존스\   메멘토
평균 arrousal        19.5         23.2



ggplot(chickFlick)+
  stat_summary(
    aes(film, arousal), 
    fun.y = mean, 
    geom = "bar", 
    fill = "pink", 
    colour = "Black")+
  geom_point(aes(film, arousal))+
  labs(x = "Film", y = "Arousal") 


ggplot(chickFlick, aes(film, arousal))+
  stat_summary(
    fun.y = mean, 
    geom = "bar",
    width=0.4,
    fill = "White", 
    colour = "Black") + 
  stat_summary(
    fun.data = mean_cl_normal, 
    geom = "errorbar", 
    width=0.4, 
    color="Red") +
  labs(x = "Film", y = "Mean Arousal") 

install.packages("Hmisc", dependencies=TRUE, repos="https://cran.rstudio.com")
library(Hmisc)
##position dodge

1

##ggplot 안에 aes(colour=gender)를 두었을때와 외부에 두었을때의 차이
ggplot(chickFlick, aes(film, arousal, fill=gender))+
  stat_summary(
    fun.y = mean, 
    geom = "bar", 
    position="dodge") +
  stat_summary(
    fun.data = mean_cl_normal, 
    geom = "errorbar", 
    position=position_dodge(width=0.90),   
    width = 0.1) +
  labs(x = "Film", y = "Mean Arousal", fill = "Gender")

##facet_wrap과 facet_grid의 차이
#facet grid : combinations of two var. display all facet even if some are empty
#facet_wrap : 1 var. 만일 two variable일때는 rectangular facet으로 각각 구분

 ggplot(chickFlick, aes(film, arousal, fill = film))+
   stat_summary(
     fun.y = mean, 
     geom = "bar") + 
   stat_summary(
     fun.data = mean_cl_normal, 
     geom = "errorbar", 
     width = 0.2) + 
   facet_wrap(~gender) + #facet_wrap(~) : 변수로 감싸주는 방식으로 그래프 분리
   labs(x = "Film", y = "Mean Arousal") + 
   theme(legend.position="none")##theme : legend, axis, panel 등 다양한 옵션 변경 가능
 
 ggplot(chickFlick, aes(film, arousal, fill = film))+
   stat_summary(
     fun.y = mean, 
     geom = "bar") + 
   stat_summary(
     fun.data = mean_cl_normal, 
     geom = "errorbar", 
     width = 0.2) + 
   facet_grid(gender~.) + #facet_wrap(~) : 변수로 감싸주는 방식으로 그래프 분리
   labs(x = "Film", y = "Mean Arousal") + 
   theme(legend.position="none")##theme : legend, axis, panel 등 다양한 옵션 변경 가능
 

 
##(학생실습과제) chickFlick 자료, x축 film, y축 arousal.
### 1) 막대 그래프(막내 내부는 흰색, 테두리는 검은색) + 오차 막대 errorbar 형태, mean_cl_normal로 오차 표현, 오차막대 빨간색상으로, 너비는 0.2)
### 2) 막대 그래프(막내 내부는 흰색, 테두리는 검은색) + 오차 막대 errorbar 형태, mean_cl_boot로 오차 표현,  오차막대 빨간색상으로, 너비는 0.2)


#--------Line Charts------------------------------------------

hiccupsData <- read.delim("Hiccups.dat",  header = TRUE)
hiccups<-stack(hiccupsData)
str(hiccups)
names(hiccups)<-c("Hiccups","Intervention")

bts$btsposition=factor(btsposition, levels=c("vocal", "rap"))
hiccups$Intervention=factor(hiccups$Intervention, levels=c("Carotid", "Rectum", "Tongue", "Baseline"))
str(hiccups)
hiccups$Intervention_Factor<-factor(hiccups$Intervention, levels(hiccups$Intervention)[c(1, 4, 2, 3)])
str(hiccups)


line <- ggplot(hiccups,  aes(Intervention_Factor, Hiccups))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = 1),colour = "Red", linetype = "dashed") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Intervention", y = "Mean Number of Hiccups")

## textdata로 추가 실습

textData <- read.delim("TextMessages.dat",  header = TRUE)
textData$id = row(textData[1])

#textMessages = reshape(textData, idvar = c("id", "Group"), varying = c("Baseline", "Six_months"), v.names = "Grammar_Score", timevar = "Time", times = c(0:1), direction = "long")

textMessages<-melt(textData, id = c("id", "Group"), measured = c("Baseline", "Six_months"))
names(textMessages)<-c("id", "Group", "Time", "Grammar_Score")
textMessages$Time<-factor(textMessages$Time, labels = c("Baseline", "6 Months"))

print (textMessages)

## group에 따라 선을 다르게 그림

line <- ggplot(textMessages, aes(Time, Grammar_Score, colour = Group))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group= Group)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Time", y = "Mean Grammar Score", colour = "Group") 


## (학생실습과제) textMessages객체의 Time(x), Grammar_Score(y), 자료 색상은 Group에 따라 다르게 표현
## 1) X에 따른 Y값의 평균을 점으로 표현(GROUP에 따라 점의 모양 다르게, 점의 크기는 4)
 ## *** 점의 모양 다르게 하는 옵션 : aes(shape=Group)
## 2) X에 따른 y값의 평균 사이를 선으로 이음 : Group 변수에 따라 서로 다른 선들이 그려지도록, 선 유형도 group에 따라 다르게,
## 3) 오차 막대 표(mean_cl_boot), 너비는 0.2

## 1) + 2) + 3) 합쳐서 그림 그리기



#--------Cleveland dot plot----------------------------------------------
#막대 그래프의 대안으로 점그래프 (dot plot)의 우수성을 알린 Cleveland & McGill(1984)의 논문에서 비롯됨. 가독성에서 우수성을 갖고 있고, OECD 등 국제 보고서에서 자주 발견됨

#MASS 패키지에 포함된 Cars93 데이터 프레임을 이용해보도록 하겠음
install.packages("MASS")
library(MASS)
library(dplyr)
library(ggplot2)
str(Cars93)
Cars93_df<-data.frame(Cars93)
table(Cars93$Type)
table(Cars93$Model)

Cars93_df %>% 
  filter(Type %in% c("Large","Midsize", "Small")) %>% 
  select(Model,Type, Min.Price, Max.Price) ->Cars93_df_se

str(Cars93_df_se)
table(Cars93_df_se$Type)

Cars93_df_se %>% 
  ggplot(.,aes(x=Max.Price, y=reorder(Model,Max.Price), shape=Type))+
  geom_point(size=3, colour="grey")+
  theme_bw()+ #background 색 없앰
  theme(panel.grid.major.x=element_blank(), #x축 선 없애기
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_line(colour="grey90", linetype="dashed"))+
  ggtitle("Cleveland dot plot")

##facet으로 wrapping
Model_Order<-
  Cars93_df_se$Model[order(Cars93_df_se$Type, -Cars93_df_se$Max.Price, decreasing=TRUE)]

Cars93_df_se$Model <-factor(Cars93_df_se$Model, levels=Model_Order)

Cars93_df_se %>% 
  ggplot(.,aes(x=Max.Price, y=Model))+
  geom_point(size=2, aes(colour=Type))+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank())+
  facet_grid(Type~.,scales="free_y", space="free_y")+
  ggtitle("cleveland dot plot with facets")


###
library(tidyverse)
install.packages("babynames")
library(babynames)
str(babynames)

top_names<-
  babynames %>% 
  filter(year >=1950, year <1990) %>% 
  mutate(decade=(year %/% 10)*10) %>% 
  group_by(decade) %>% 
  count(name, wt=n, sort=TRUE) %>% 
  ungroup

str(top_names)



top_names %>% 
  group_by(decade) %>% 
  top_n(15) %>% 
  ungroup %>%
  mutate(decade=as.factor(decade)) %>% 
  ggplot(aes(name, n, fill=decade))+ 
  geom_col()+
  facet_wrap(~decade, scales="free_y")+
  coord_flip()+
  scale_y_continuous(expand=c(0,0))

###위의 그래프를 연도별 빈도가 높은 이름 순으로 정렬하여 보여주고 싶다면?
install.packages("tidytext")
library(tidytext)

top_names %>% 
  group_by(decade) %>% 
  top_n(15) %>% 
  ungroup %>%
  mutate(decade=as.factor(decade),
         name= reorder_within(name, n, decade)) %>%  ##reorder_within(order할 변수, order 기준, within 기준) 
  ggplot(aes(name, n, fill=decade))+ 
  geom_col()+
  facet_wrap(~decade, scales="free_y")+
  coord_flip()+
  scale_x_reordered()
  scale_y_continuous(expand=c(0,0))

  
##다시 cars93으로 돌아와서 reorder_within을 이용해서 tidy하게 바꿔보자
str(Cars93_df_se)

Cars93_df_se %>%
  mutate(Model=reorder_within(Model, Max.Price, Type)) %>% 
  ggplot(aes(x=Max.Price, y=Model))+
  geom_point(size= 2, aes(color=Type))+
  facet_grid(Type~.,scales="free_y", space="free_y")+
  scale_x_reordered()+
  ggtitle("cleveland dot plot with facets")+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank())
  



Cars93_df_se %>%
  group_by(Type) %>% 
  top_n(n=5, wt=Max.Price) %>%
  ungroup %>% 
  mutate(Model=reorder_within(Model, Max.Price, Type)) %>% 
  ggplot(aes(x=Max.Price, y=Model))+
  geom_point(size= 2, aes(color=Type))+
  facet_grid(Type~.,scales="free_y", space="free_y")+
  scale_x_reordered()+
  ggtitle("cleveland dot plot with facets")
