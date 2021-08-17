data(anscombe)
anscombe

options(digits=2) 
sapply(anscombe, mean)

sapply(anscombe, sd)

attach(anscombe)
cor(x1, y1)
cor(x2, y2)
cor(x3, y3)
cor(x4, y4)
detach(anscombe)

attach(anscombe)
lm(y1 ~ x1)

lm(y2 ~ x2)

lm(y3 ~ x3)

lm(y4 ~ x4)

par(mfrow=c(2,2))

attach(anscombe)

plot(x1, y1): abline(lm(y1~x1), col="blue", lty=3)

plot(x2, y2): abline(lm(y1~x1), col="blue", lty=3)
plot(x3, y3): abline(lm(y1~x1), col="blue", lty=3)
plot(x4, y4): abline(lm(y1~x1), col="blue", lty=3)

detach(anscombe)


## ggplot2

getwd()
setwd("C:\\Users\\Owner\\Documents\\new")

install.packages("ggplot2")
library(ggplot2)
data("mpg")
View(mpg)

ggplot(data=mpg, aes(x=displ, y=hwy))
a <- ggplot(data=mpg, aes(x=displ, y= hwy))

## working directory에 저장됨
ggsave("title.png")
ggsave("title.png", width=5, height=5)

a <-ggplot(data=mpg, aes(x=displ, y= hwy))
a <-ggplot(data=mpg, aes(x=displ, y= hwy, color="red"))

a <- ggplot(data=mpg, aes(x=displ, y=hwy))
a + geom_point()

a <- ggplot(data=mpg, aes(x=displ, y=hwy))
a + geom_point()
a + geom_point() + facet_grid(. ~fl)
a + geom_point() + facet_grid(. ~fl) + ggtitle("New Plot")


## R 실습

a <- ggplot(data=mpg, aes(x=displ))
a + geom_histogram()

a<- ggplot(data=mpg, aes(x=manufacturer, y=displ))
a + geom_boxplot()

a<- ggplot(data=mpg, aes(x=displ, y=hwy))
a + geom_point()

a<- ggplot(data=mpg, aes(x=displ, y=hwy))
a + stat_summary(fun.y=mean, geom="bar")

a<- ggplot(data=mpg, aes(x=displ, y=hwy))
a + stat_summary(fun.y=mean, geom="line")


