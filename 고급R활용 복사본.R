
## 1. 교재 1장 연습문제 1번(p.34)의 데이터를 이용하여
data1 <- data.frame(id=c(1:10),
                    dose=c(25,30,20,30,40,35,25,20,30,45),
                    sex=c('f','m','f','m','m','f','f','f','m','f'),
                    bp=c(140,150,135,120,110,130,120,145,130,125),
                    age=c(58,48,62,45,48,50,54,52,60,72))

head(data1)

install.packages("tidyverse")
library(tidyverse)
## A. 성별로 용량, 혈압, 나이의 평균 및 표준편차를 구하시오. (3점)
data1.f <- data1 %>% filter(sex=='f') 
data1.m <- data1 %>% filter(sex=='m')



data1.new <- data1 %>% group_by(sex) %>%
                      summarise(Mean.dose=mean(dose),
                                SD.dose=sd(dose),
                                Mean.bp=mean(bp),
                                SD.bp=sd(bp),
                                Mean.age=mean(age),
                                SD.age=sd(age))
data1.new

#성별 용량 평균 & 표준편차
mean(data1.f$dose)
sd(data1.f$dose)
mean(data1.m$dose)
sd(data1.m$dose)

#성별 혈압 평균 & 표준편차
mean(data1.f$bp)
sd(data1.f$bp)
mean(data1.m$bp)
sd(data1.m$bp)

#성별 나이 평균 & 표준편차
mean(data1.f$age)
sd(data1.f$age)
mean(data1.m$age)
sd(data1.m$age)


## B. 성별로 혈압의 상자그림을 그리고 비교하시오. (3점)
boxplot(bp~sex, data=data1, col="white", main="성별 혈압의 상자그림", ylab="혈압", xlab="성별")
boxplot(bp~sex, data=data1, col="white", main="성별 혈압의 상자그림", ylab="혈압", xlab="성별")$stat
par(family="AppleGothic")

## C. 성별로 혈압에 차이가 있는지 검정하시오. (3점)
t.test(bp~sex, data=data1)

## D. 혈압이 130 미만이면 “Low”, 130 이상이면 “High” 값을 가지는 새로운 변수를 만드시오. (3점)
data1$bp.group <- ifelse(data1$bp>=130, "High", "Low")
data1



## 2. 교재 1장 연습문제 2번(p.34)의 데이터를 이용하여 각 분야별 월수입의 평균과 표준편차를 구하시오. (4점)
data2 <- data.frame(area=c(rep(1,3),rep(2,4),rep(3,4)),
                    mon.sal=c(87,90,62,80,70,65,57,100,115,120,102))

data2.1 <- data2 %>% filter(area==1)
data2.2 <- data2 %>% filter(area==2)
data2.3 <- data2 %>% filter(area==3)

mean(data2.1$mon.sal)
sd(data2.1$mon.sal)
mean(data2.2$mon.sal)
sd(data2.2$mon.sal)
mean(data2.3$mon.sal)
sd(data2.3$mon.sal)

## 3. 교재 3장 연습문제 8번(p.120)의 데이터를 이용하여 일원배치 분산분석을 수행하시오. (5점)
x <- c(rep(1,10), rep(2,10), rep(3,10), rep(4,10))
y <- c(11, 10, 8, 11, 10, 10, 11, 11, 10, 9,
       11, 11, 9, 11, 12, 13, 9, 10, 11, 10,
       12, 14, 10, 12, 10, 12, 12, 12, 11, 12,
       11, 10, 8, 11, 11, 9, 10, 11, 11, 10)

data3 <- data.frame(x,y)
oneway.test(y~x, data=data3, var.equal = T)

bartlett.test(y~x, data=data3)

## 4. R에 datarium 패키지에 내장된 depression 데이터셋은 우울증 환자들의 우울점수 자료이다. 
## 변수 t0은 우울증 치료 전에 측정한 우울점수이고, 변수 t2는 우울증 치료 3개월 후에 측정한 우울점수이다. 
## A. x축을 t0, y축을 t2로 하는 산점도를 그리시오. (3점) 
install.packages("datarium")
library(datarium)
data4 <- depression
plot(data4$t0, data4$t2)

## B. t0가 독립변수이고 t2가 결과변수인 선형회귀분석을 수행하여 회귀직선의 절편과 기울기를 구하시오. (3점)
obj <- lm(t2~t0, data=data4)
summary(obj)

## C. 4-A에서 그린 산점도 위에 4-B에서 구한 회귀직선을 그리시오. (3점)
abline(obj)
