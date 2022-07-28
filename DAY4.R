###
install.packages("sqldf")
library(sqldf)
library(googleVis)
Fruits
library(dplyr)

head(Fruits,2)
sqldf("select * from Fruits")     #모든 변수(*)들 보여줌
sqldf("select Sales, Expenses from Fruits")
sqldf("select * from Fruits where Fruit = \'Oranges\'")

## 1) 2008년 데이터만 추출
sqldf("select * from Fruits where Year = \'2008\'")
## 2) East만 추출
sqldf("select * from Fruits where Location = \'East\'")

#출력되는 행 수 제한 : limit사용
sqldf("select * from Fruits limit 3")     #   = head(Fruit, 3)

#정렬하기 : order by 사용
sqldf("select * from Fruits order by Sales")     #오름차순
sqldf("select * from Fruits order by location, Sales")     #Location (선) 오름차순, Sales (후) 오름차순
sqldf("select * from Fruits order by Location, Year desc")     #Location (선)오름차순, Year (후) 내림차순

###-서브쿼리(Sub Quary)

#단일행 (=, >=, <=, >, <)서브쿼리 사용, 1행만 리턴
sqldf("select * from Fruits where Sales > (select Sales from Fruits where Expenses = 78)")     #Expenses = 78인 값 중 Sales 값이 가장 큰 값 1개 리턴 
sqldf("select * from Fruits where Sales > (select Sales from Fruits where Profit > 15)")     #Profit > 15인 값 중 제일 큰 Sales 값  1개 리턴
sqldf("select * from Fruits where Sales > (select Sales from Fruits where Sales > 95)")     #Sales > 95인 값 중 제일 큰 Sales 값 1개 리턴

var1 <- matrix(c("가", "나", "다"))
var1
ca <- c("가", "나", "다", "라", "마", "바", "사")
ca
lv <- c(3, 7, 11, 31, 49, 78, 43)
lv
id <- c(3233 ,3789, 4939, 2336, 4555, 7889, 9999)
id

data <- data.frame(CA = ca, LV = lv, ID = id)
Var2 <- as.data.frame(var1)
sqldf("select * from data where CA in (select V1 from Var2)")

setwd("D:/R_Data")
STUDENT <- read.csv("Sql1.csv", header = T)
PROFESSOR <- read.csv("Sql2.csv", header = T)

sqldf("select s.Name STUDENT_Name p.Name PROFESSOR_Name from STUDENT s PROFESSOR p where s.ProfNo = p.ProfNo")

#데이터 변경 : update
sqldf("select * from Fruits")
FR <- Fruits
sqldf(c("UPDATE FR SET Profit = 50 WHERE FR = \'Apples\' AND Year = 2008", "select * from FR"))

#Graph
a <- c(24, 35, 85, 72, 62, 14, 53, 84, 26, 45)
b <- c(74, 25, 24, 55, 62, 45, 53, 77, 58, 90)
c <- c(21, 15, 75, 56, 35, 48, 53, 88, 45, 96)
plot(a, type = "b")   #직선형 그래프
plot(a, type = "s")   #계단형 그래프
plot(a, b)   #x축 : a, y축 : b
plot(a, b, pch = 19)   #검은색 점으로 변환

par(mfrow = c(1, 3))   #그래프 3개 한 화면에 출력
plot(c, type = "b")
dev.off()   #plot 지우기

par(mfrow = c(1,1))
a <- c(260,300,250,280,310)
b <- c(180,200,210,190,170)
c <- c(210,250,260,210,270)

plot(a, type="o", col="red", ylim=c(0,400), axes=F, ann=F)
axis(1, at=1:5, lab=c("A", "B", "C", "D", "E"))
axis(2, ylim=c(0,400))
title(main="Ball Type", col.main="red", font.main=4)
title(xlab="Day", col.lab="black")
title(ylab="Price", col.lab="blue")
lines(b, type="o", pch=21, col="gray", lty=2)
lines(c, type="o", pch=22, col="blue", lty=2)
#pch는 0~25까지 번호마다 표시가 할당되어 있음. 22번은 사각형 모양임
legend(4,410, c("BaseBall","SoccerBall","BeachBall"), cex=0.8,
       col=c("red","gray","blue"), pch=21, lty=1:3)

abc <- c(110, 300, 150, 280, 310) # 시즌별(=A,B,C,D,E) BaseBall 판매현황
def <- c(180, 200, 210, 190, 170) # 시즌별 SoccerBall 판매현황
ghi <- c(210, 150, 260, 210, 70) # 시즌별 BeachBall 판매현황
boxplot(abc,def,ghi)   #짙은색이 median 임

install.packages("ggmap")
library(ggmap)
library(ggplot2)
mykey <-  "AIzaSyDiTAW3azHKb51ayX_Xxo0UJkc4M91yKtE"
register_google(key = mykey)

gc <- geocode("Paris", source = "google")
gc
center <- as.numeric(gc)
gmap <- get_googlemap(center = center)
ggmap(gmap)

gc <- geocode(enc2utf8("충북대학교"), source = "google")
gc
center <- as.numeric(gc)
gmap <- get_googlemap(center = center, maptype = "roadmap", markers = gc, scale = 2, zoom = 12)
ggmap(gmap)

### Linear Regression
install.packages("UsingR")
library(UsingR)
data(package="UsingR")
data(father.son)
father.son
head(father.son, 3)

Fsh <- father.son
head(Fsh, 3)

result <- lm(sheight~fheight, data = Fsh)
summary(result)

ggplot(Fsh, aes(x = fheight, y = sheight)) +
  geom_point() +
  stat_smooth(method = lm, level = 0.95, col = "BLUE")
  #geom_abline(intercept = 33.88660, slope = 0.51409, col = "RED")
  

# Multi Regression
library(reshape2)
head(tips, 2)
Result <- lm(tip~., data = tips)   # ( . )은 전체를 뜻함
summary(Result)

head(mtcars, 2)
summary(mtcars)
# 기술통계랑 구하기
install.packages("psych")
library(psych)

describe(mtcars$mpg)
boxplot(mtcars$mpg)
ggplot(mtcars, aes(mpg)) + 
  geom_boxplot()

str(mtcars)
mtcars$vs <- factor(mtcars$vs)
mtcars$am <- factor(mtcars$am)


# 다중회귀분석
Result <- lm(mpg~., data = mtcars)
summary(Result)
Result <- step(Result, direct = "bith")
