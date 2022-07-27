dev.off()   #plot 지울때
rm(list=ls())   #저장된 변수 지울 때
cat("\f")   #console 창 내용지움

install.packages("googleVis")

library(googleVis)
data(package = "googleVis")
Cats

Fruits

head(Fruits)     #앞에서 6개 출력
head(Fruits, 2)     #앞에서 2개만 출력
tail(Fruits)     #뒤에서 6개 출력
tail(Fruits, 2)     #뒤에서 2개만 출력

#연도별 판매액의 합계
aggregate(Sales~Year, Fruits, sum)

aggregate(Profit~Fruit, Fruits, sum)

aggregate(Profit~Location, Fruits, mean)

aggregate(Sales~Year+Location,Fruits, mean)

head(Fruits, 2)
set1 <- subset(Fruits, select = c(Year, Sales, Expenses, Profit))     #원하는 수치데이터들만 뽑아서 배열로 만듦
Fruits[, c("Sales", "Expenses", "Profit")]
cor(Fruits$Sales, Fruits$Profit)
plot(Fruits$Sales, Fruits$Profit, pch = 19)
cor(set1)
plot(set1, pch = 19)

#2008년 데이터만 추출
subset(Fruits, subset=(Year==2008))
subset(Fruits, subset=(Year!=2008))     #2008이외의 값들 출력

#판매액이 90이상인 데이터만 추출
subset(Fruits, subset=(Sales>=90))

#오렌지 중 순이익이 15이상인 것만 추출
subset(Fruits, subset=(Fruit=="Oranges" & Profit>=15))

head(iris)

#다음은 iris data로 추출해보세요
subset(iris, select = c(Sepal.Length, Sepal.Width))     #1) sepal 관련 변수만 추출하기
subset(iris, select = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)) ## = subset(iris, select = -Species)     #2)수치데이터만 추출하기
subset(iris, subset = (Species == "setosa"))     #3)SETOSA데이터만 추출하기
      table(iris$Species)
subset(iris, subset = (Sepal.Length < 4.5 & Petal.Length <= 1.5))     #4) Sepal.Length가 4.5미만이고 Petal.Length가 1.5이하인 자료추출
      summary(iris$Sepal.Length)
      summary(iris$Petal.Length)
subset(iris, subset = (Petal.Length < mean(Petal.Length) | Sepal.Width >= mean(Sepal.Width)))

#dplyr
install.packages("dplyr")
library(dplyr)

G.Fruit <- group_by(Fruits, Fruit)
summarise(G.Fruit, mean(Sales), sd(Sales), min(Sales), max(Sales))     #summarise 랑 summarize 둘다 똑같음

#연도별 판매액의 합계
G.Year <- group_by(Fruits, Year)
summarize(G.Year, sum(Sales))
     #위아래 같음
Fruits %>%
  group_by(Year) %>%
  summarise(sum(Sales))


#과일별 순이익의 합계
G.Fruit <- group_by(Fruits, Fruit)
summarise(G.Fruit, sum(Profit))
     # 위아래 같음
Fruits %>%
  group_by(Fruit) %>%
  summarise(sum(Profit))


#지역별 평균 순이익
G.Loc <- group_by(Fruits, Location)
summarise(G.Loc, mean(Profit))
     # 위아래 같음
Fruits %>%
  group_by(Location) %>%
  summarise(mean(Profit))


#년도별, 지역별 평균 판매액
G.YL <- group_by(Fruits, Year, Location)
summarise(G.YL, mean(Sales))
     # 위아래 같음
Fruits %>%
  group_by(Year, Location) %>%
  summarise(M.Sales = mean(Sales), SD.Sales = sd(Sales), Count = n())


### arrange
## 1) 판매액에 따라 내림차순 정렬
arrange(Fruits, Sales)

Fruits %>%
  arrange(Sales)

## 2) 판매액, 년도(내림차순)에 따라 정렬
arrange(Fruits, desc(Year))

Fruits %>%
  arrange(Sales, desc(Year))
## 3) 순이익, 과일별, 지역별에 따른 정렬
arrange(Fruits, Profit, Fruit, Location)

Fruits %>%
  arrange(Profit, Fruit, Location)


### filter
head(Fruits, 2)

## 1) 과일명이 Apple인 대상 추출
filter(Fruits, Fruit == "Apples")
     # 동일
Fruits %>%
  filter(Fruit == "Apples")


## 2) 2008년 West 대상 추출
filter(Fruits, Year == 2008 & Location == "West")
     # 동일
Fruits %>%
  filter(Year == 2008 & Location == "West")


## 3) Expenses가 평균 이상인 대상 추출
filter(Fruits, Expenses >= mean(Expenses))
     # 동일
Fruits %>%
  filter(Expenses >= mean(Expenses))


## 4) West이고 평균이하의 판매액을 가진 대상 추출
filter(Fruits, Location == "West" & Sales <= mean(Sales))
     # 동일
Fruits %>%
  filter(Location == "West" & Sales <= mean(Sales))


### select

## 1) Year, Location, Fruit변수만 선택해서 Datadf라는 데이터프레임을 만드시오
Datadf <- select(Fruits, Year, Location, Fruit, Date)
Datadf

Datadf <- Fruits %>%
  select(Year, Location, Fruit, Date)


## 2) Data를 제외하고 del_Date 라는 데이터 프레임을 만드시오
del_Date <- select(Fruits, -Date)
del_Date

del_Date <- Fruits %>%
  select(-Date)
del_Date


## 3) Sales, Expenses, Profit 변수만 선택하여 df 라는 데이터프레임을 만드시오
df <- select(Fruits, Sales:Profit)
df

df <- Fruits %>%
  select(Sales:Profit)
df



### mutate

head(Fruits)
## 방법1
N.Fruits <- mutate(Fruits, N.Profit=Sales-Expenses, Tax = Sales*0.1, O.Profit = N.Profit-Tax)

Fruits
## 방법2
N.Profit1 <- Fruits$Sales - Fruits$Expenses
Tax1 <- Fruits$Sales*0.1
O.Profit1 <- N.Profit1 - Tax1

N.Fruits1 <- cbind(Fruits, N.Profit1, Tax1, O.Profit1)

# 방법3
Fruits$N.Profit1 <- Fruits$Sales - Fruits$Expenses
Fruits$Tax1 <- Fruits$Sales*0.1
Fruits$O.Profit1 <- N.Profit1 - Tax1


### 종합연습문제

install.packages("reshape2")
library(reshape2)

tips

## 1) 토요일에 저녁시간에 방문한 고객의 평균 Tip의 액수는?
mutate()
## 2) 남자들 중 담배를 피는 이와 피지 않는 이들의 평균 팁의 액수는?

## 3) total_bill, tip, size를 추출해서 그 변수들간의 상관성은?
