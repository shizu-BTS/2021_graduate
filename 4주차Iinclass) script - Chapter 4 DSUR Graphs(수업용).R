#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 4 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------

#Set the working directory (you will need to edit this to be the directory where you have stored the data files for this Chapter)

setwd("setwd("C:\\")")

######Initiate packages

#If you don't have ggplot2 installed then use:
install.packages(c("ggplot2", "plyr"))


#Initiate ggplot2
library(ggplot2)
library(reshape)
library(plyr)


#--------Scatterplots----------

examData <- read.delim("Exam Anxiety.dat",  header = TRUE)
names(examData)

#Simple scatter
scatter <- ggplot(examData, aes(Anxiety, Exam, colour = Gender))
scatter + geom_point()

#Simple scatter / labs(x=“x축 이름”, y=“y축이름“)
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + labs(x = "Exam Anxiety", y = "Exam Performance %") 

#Simple scatter with smooth/ labs(title = "그림제목")
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + geom_smooth() + labs(x = "Exam Anxiety", y = "Exam Performance %", title = "labs(title)")


#Simple scatter with regression line(red)
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "Exam Anxiety", y = "Exam Performance %") 


#Simple scatter with regression line(red)+ CI

scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red")+ labs(x = "Exam Anxiety", y = "Exam Performance %") 


#Simple scatter with regression line + coloured CI
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", alpha = 0.1, fill = "Red") + labs(x = "Exam Anxiety", y = "Exam Performance %") 


#Grouped scatter with regression line + CI

scatter <- ggplot(examData, aes(Anxiety, Exam, colour = Gender))
scatter + geom_point() + geom_smooth(method = "lm", aes(fill = Gender), alpha = 0.1) + labs(x = "Exam Anxiety", y = "Exam Performance %", colour = "Gender") 




##(학생실습과제)
##"FacebookNarcissism.dat" 자료로 NPQC_R_Total(X축), Rating(y축)의 산포도 그리기
## 1) 산포도 그리기(Rating_Type에 따라 색상 다르게)
## 2) 산포도와 함께 회귀선 그리기(Rating_Type에 따라 회귀선 색상 다르게, method는 lm으로)
## 3) 그림 title은 "geom_smooth(aes(colour = Rating_Type))" 으로 제시



#--------HISTOGRAMS----------

##Load the data file into R. This is a tab-delimited file hence use of read.delim

festivalData <- read.delim("DownloadFestival.dat",  header = TRUE)
##Histogram with Outlier
festivalHistogram <- ggplot(festivalData, aes(day1)) + labs(legend.position="none")
festivalHistogram + geom_histogram(binwidth = 0.4) + labs(x = "Hygiene (Day 1 of Festival)", y = "Frequency")

#Locate outlier(추가)

festivalData<-festivalData[order(festivalData$day1),]


#Density without outlier(추가)

festivalData2 = read.delim("DownloadFestival(No Outlier).dat",  header = TRUE)

festivalDensity <- ggplot(festivalData2, aes(day1))
festivalDensity + geom_density() + labs(x = "Hygiene (Day 1 of Festival)", y = "Density Estimate")


festivalDensity + geom_density(aes(fill = gender), alpha = 0.5) + labs(x = "Hygiene (Day 1 of Festival)", y = "Density Estimate")

#--------BOXPLOTS----------

festivalBoxplot <- ggplot(festivalData, aes(gender, day1))
festivalBoxplot + geom_boxplot() + labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")

#with outlier removed

festivalData2 = read.delim("DownloadFestival(No Outlier).dat",  header = TRUE)
festivalBoxplot2 <- ggplot(festivalData2, aes(gender, day1))
festivalBoxplot2 + geom_boxplot() + labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")


##(학생실습과제) days 2 and 3 각각 box plot 그려보기##



#--------Bar Charts----------

chickFlick = read.delim("ChickFlick.dat",  header = TRUE)

bar <- ggplot(chickFlick, aes(film, arousal))
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs(x = "Film", y = "Mean Arousal") 

bar <- ggplot(chickFlick, aes(film, arousal, fill = gender ))
bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Film", y = "Mean Arousal", fill = "Gender")


bar <- ggplot(chickFlick, aes(film, arousal, fill = film))
bar + stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) + facet_wrap(~gender) + labs(x = "Film", y = "Mean Arousal") + theme(legend.position="none")





##(학생실습과제) chickFlick 자료, x축 film, y축 arousal.
### 1) 막대 그래프(막내 내부는 흰색, 테두리는 검은색) + 오차 막대 errorbar 형태, mean_cl_normal로 오차 표현, 오차막대 빨간색상으로, 너비는 0.2)
### 2) 막대 그래프(막내 내부는 흰색, 테두리는 검은색) + 오차 막대 errorbar 형태, mean_cl_boot로 오차 표현, 오차막대 빨간색상으로, 너비는 0.2)







#--------Line Charts----------

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


