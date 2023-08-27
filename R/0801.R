# 벡터는 c()함수로 만든다
x <- c(1,2,3)
y <- c('a','b','c')
z <- c(TRUE, TRUE, FALSE, TRUE)

# 연속적인 숫자로 이루어진 벡터
v1 <- 50:90 # 콜론을 이용하면 연속된 정수로
v1

v2 <-c(1,2,5,50:90)
v2

# 일정한 간격의 숫자로 이루어진 벡터 seq(1,101,3) # 시작, 종료, 간격

v3 <- seq(1,101,3)
v3
v4 <- seq(0.1, 1.0, 0.1)
v4

# 반복된 숫자로 이루어진벡터 rep(1, times =5)
v5 <- rep(1,times=5)
v5
v6 <- rep(1:5, times = 3)
v6
v7<- rep(c(1,5,9), times = 3)
v7

# 벡터에 이름 부여 및 사용하기 names()함수
sales <- c(640,720, 680, 540)
names(sales) <- c('m1','m2','m3','m4')
sales
sales[1]
sales['m2']
sales[c('m1','m4')]

# 적금 만기 금액 계산하기
# 복리 식 만기금액 = 원금x(1+연이율/100)^기간

money <- c(5000000,4500000,4000000,5500000,6000000)
iyul <- c(0.035, 0.03, 0.04, 0.05, 0.045)
day <- c(2,2,5,7,4)
names(money) <- c('kim','lee','park','choi','seo')
names(iyul) <- c('kim','lee','park','choi','seo')
names(day) <- c('kim','lee','park','choi','seo')

man <- money[1:5]*(1+iyul[1:5])^day[1:5]
man

# sort() 오름차순 내림차순 정렬
d <-c(1,7,4,2,3)
sort(d)
sort(d, decreasing = TRUE)
sort


# 월별 매출액 분석
sales <- c(750,740,760,680,700,710,850, 890,700,720, 690,730)
names(sales)<- paste(1:12,'월', sep="")
sales['7월']
max.month <- sort(sales, decreasing = T)
max.month[1]
sum(sales[1:6]) # 합계
max.month # 내림차순

##########

d <- c(1,2,3,4,5,6,7,8,9,10)
sum(d)
sum(2*d)
length(d)
mean(d[1:5])
max(d)
min(d)
sort(d)
sort(d, decreasing = F)
sort(d, decreasing = T)
v1 <- median(d)
v1
v2 <- sum(d)/length(d) # 55/10
v2


d <- 1:9
d >= 5
d[d>5]
d>5
sum(d>5)
d ==5

condi <- d>5 & d<8
condi
d[condi]


# 일주일간 요일별, 총매출액, 평균매출액, 평균매출 이상인 요일

price <- c(2000, 2500, 3000)
day <- c('월','화','수','목','금','토','일')
ess <- c(4,5,3,6,5,4,7)
ame <- c(63,68,64,68,72,89,94)
caf <- c(61, 70, 59, 71, 71, 92, 88)

names(ess) <- day
names(ame) <- day
names(caf) <- day

ess <- ess*2000
ame <- ame*2500
caf <- caf*3000

all_day <- ess+ame+caf
all <- sum(ess, ame, caf)
mean(all_day)
all_day[all_day >= mean(all_day)]


## 팩터(문자형 데이터가 저장되는 벡터)

bt <- c('A','B','B','O','AB','A') #문자형 벡터 bt정의
bt.new <- factor(bt) # 원본데이터를 손상않고 bt.new정의
bt[5]
bt.new[5] #똑같이 출력되는데 
levels(bt.new) 
as.integer(bt.new) #범주형팩터를 숫자로 바꿔줌
bt.new[7] <- 'B'
bt.new[8] <- 'C' #C는 범주에 없기때문에 경고메세지가 뜨면서 NA값이 bt.new[8]에 들어감
bt.new


## 리스트 자료형이 다른 값들을 한곳에 저장
h.list <- c('bowling', 'tennis','ski')
person <- list(name = 'Tom', age = 25, student = TRUE, hobby = h.list)
person$name
person$hobby
person[[4]]
person[1]
person[[1]]

accident <- c(31, 26, 42, 47, 50, 54, 70, 66, 43, 32, 32, 22)
names(accident) <- c('M1','M2','M3','M4','M5','M6','M7','M8','M9','M10','M11','M12')
sum(accident)
max(accident)
min(accident)
accident[accident>=50]
# 사고건수가 50건이 넘는 달의 이름
month.50 <- accident[accident>=50]
names(month.50)
names(accident[accident>=50])

length(accident[accident<50])
