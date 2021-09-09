#######################################
###professor Choi, shizu@snu.ac.kr ####
#######################################




#######################################
###########   ggplot2    ##############
#######################################

#scalar#vector#list#array#data.frame#c#rbind#cbind#$#mean#na.rm=TRUE

######Initiate packages

#If you don't have ggplot2 installed then use:
install.packages(c("ggplot2", "plyr", "reshape"))


#Initiate ggplot2
library(ggplot2)
library(reshape)
library(plyr)
library(dplyr)

#--------Scatterplots----------

examData <- read.delim("Exam Anxiety.dat",  header = TRUE)
names(examData)
str(examData)

#Simple scatter
scatter <- ggplot(examData, aes(Anxiety, Exam, colour = Gender))
scatter + geom_point()

scatter <- ggplot(examData, aes(Anxiety, Exam, colour = Gender))
scatter + geom_point(shape=24, size=1, colour="black")

#plot symbol(points)
help(pch)




#Simple scatter / labs(x=“x축 이름”, y=“y축이름“) /ggtitle("title")
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + 
  geom_point() + 
  labs(x = "Exam Anxiety", y = "Exam Performance %") + 
  ggtitle("exam anxiety")

#data labeling 
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + 
  geom_point() + 
  labs(x = "Exam Anxiety", y = "Exam Performance %") + 
  ggtitle("exam anxiety")+
  geom_text(aes(label=Code, size=10, vjust=2, hjust=0)) #vjust : 위로, hjust : 오른쪽

#shape별 value를 알고 싶을때
help(geom_text)


##집단별 색깔을 달리한 scatter plot

scatter <- ggplot(examData, aes(x=Anxiety, y=Exam, colour=Gender))
scatter + 
  geom_point() + 
  labs(x = "Exam Anxiety", y = "Exam Performance %") + 
  ggtitle("exam anxiety")

##집단별 모양을 달리한 scatter plot
scatter <- ggplot(examData, aes(x=Anxiety, y=Exam, shape=Gender))
scatter + 
  geom_point() + 
  labs(x = "Exam Anxiety", y = "Exam Performance %") + 
  ggtitle("exam anxiety")

##집단별 층위를 나누어서 산포도를 그릴때

scatter <- ggplot(examData, aes(x=Anxiety, y=Exam))
scatter + 
  geom_point() + 
  facet_grid(Gender~.)+
  labs(x = "Exam Anxiety", y = "Exam Performance %") + 
  ggtitle("exam anxiety")

#Simple scatter with smooth/ 
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + 
  geom_smooth() + 
  labs(x = "Exam Anxiety", y = "Exam Performance %")+ 
  ggtitle("exam anxiety")


#Simple scatter with regression line(red)
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "Red", se = F) + 
  labs(x = "Exam Anxiety", y = "Exam Performance %") 


#Simple scatter with regression line(red)+ CI

scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + 
  geom_smooth(method = "lm", colour = "Red")+ 
  labs(x = "Exam Anxiety", y = "Exam Performance %") 


#Simple scatter with regression line + coloured CI
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + 
  geom_smooth(method = "lm", colour = "Red", alpha = 0.1, fill = "Red") + 
  labs(x = "Exam Anxiety", y = "Exam Performance %") 


#Grouped scatter with regression line + CI

scatter <- ggplot(examData, aes(Anxiety, Exam, colour = Gender))
scatter + geom_point() + geom_smooth(method = "lm", aes(fill = Gender), alpha = 0.1) + labs(x = "Exam Anxiety", y = "Exam Performance %", colour = "Gender") 

##(학생실습과제)
##"FacebookNarcissism.dat" 자료로 NPQC_R_Total(X축), Rating(y축)의 산포도 그리기
## 1) 산포도 그리기(Rating_Type에 따라 색상 다르게)
## 2) 산포도와 함께 회귀선 그리기(Rating_Type에 따라 회귀선 색상 다르게, method는 lm으로)
## 3) 그림 title은 "geom_smooth(aes(colour = Rating_Type))" 으로 제시

  #--------HISTOGRAM------------------------------------------------------

##Load the data file into R. This is a tab-delimited file hence use of read.delim

festivalData <- read.delim("DownloadFestival.dat",  header = TRUE)
##Histogram with Outlier
festivalHistogram <- 
  ggplot(festivalData, aes(day1)) + 
  labs(legend.position="none")+ 
  geom_histogram(binwidth = 0.4) + 
  labs(x = "Hygiene (Day 1 of Festival)", y = "Frequency")

#1 outlier cased떄문에 그래프가 이상한걸 확인
festivalData %>% 
  arrange(-day1) ->festvalDara_a


festivalData2 = read.delim("DownloadFestival(No Outlier).dat",  header = TRUE)

ggplot(festivalData2, aes(day1))+
  geom_histogram(binwidth=0.4)+
  labs(x="Hygiene of Day 1", y="Frequency")

#Density without outlier(추가)
festivalDensity <- ggplot(festivalData2, aes(day1))
festivalDensity + geom_density() + labs(x = "Hygiene (Day 1 of Festival)", y = "Density Estimate")

#집단별로 색깔 다르게, alhpha=투명도
festivalDensity + geom_density(aes(fill = gender), alpha = 0.5) + labs(x = "Hygiene (Day 1 of Festival)", y = "Density Estimate")

#--------BOXPLOTS----------

festivalBoxplot <- ggplot(festivalData, aes(gender, day1))
festivalBoxplot + geom_boxplot() + labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")

#with outlier removed

festivalData2 = read.delim("DownloadFestival(No Outlier).dat",  header = TRUE)

ggplot(festivalData2, aes(gender, day1))+
  geom_boxplot() + 
  labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")


##(학생실습과제) days 2 and 3 각각 box plot 그려보기##



#--------Bar Charts----------

chickFlick = read.delim("ChickFlick.dat",  header = TRUE)

ggplot(chickFlick, aes(film, arousal))+
  stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + 
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange") +
  labs(x = "Film", y = "Mean Arousal") 

ggplot(chickFlick, aes(film, arousal, fill = gender ))+
  stat_summary(fun.y = mean, geom = "bar", position="dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + 
  labs(x = "Film", y = "Mean Arousal", fill = "Gender")


 ggplot(chickFlick, aes(film, arousal, fill = film))+
   stat_summary(fun.y = mean, geom = "bar") + 
   stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) + 
   facet_wrap(~gender) + 
   labs(x = "Film", y = "Mean Arousal") + 
   theme(legend.position="none")


##(학생실습과제) chickFlick 자료, x축 film, y축 arousal.
### 1) 막대 그래프(막내 내부는 흰색, 테두리는 검은색) + 오차 막대 errorbar 형태, mean_cl_normal로 오차 표현, 오차막대 빨간색상으로, 너비는 0.2)
### 2) 막대 그래프(막내 내부는 흰색, 테두리는 검은색) + 오차 막대 errorbar 형태, mean_cl_boot로 오차 표현, 오차막대 빨간색상으로, 너비는 0.2)


#--------Line Charts------------------------------------------

hiccupsData <- read.delim("Hiccups.dat",  header = TRUE)
hiccups<-stack(hiccupsData)
names(hiccups)<-c("Hiccups","Intervention")
hiccups$Intervention_Factor<-factor(hiccups$Intervention, levels(hiccups$Intervention)[c(1, 4, 2, 3)])


line <- ggplot(hiccups,  aes(Intervention_Factor, Hiccups))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = 1),colour = "Red", linetype = "dashed") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Intervention", y = "Mean Number of Hiccups")

## textdata로 추가 실ㅅ

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
  geom_point(size=3, colour="blue")+
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
