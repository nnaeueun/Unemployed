# 단일변수 범주형 데이터 분석
# 타이타닉 선실(1등석/2등석/3등석) 탑승객 수

install.packages('carData')
library(carData)
summary(TitanicSurvival)
str(TitanicSurvival)

# 1. 데이터준비 str, class, head
room.class <- TitanicSurvival$passengerClass # 선실정보
room.class

# 2. 도수분포 계산 table()
tbl <- table(room.class)
tbl
sum(tbl) # 전체 수

# 3. 막대그래프작성 barplot()
barplot(tbl, main = '선실별 탑승객 정보', xlab = '선실등급', ylab = '탑승객 수', col=c('blue','green','yellow'))

# 4. 원그래프 작성 pie()
tbl/sum(tbl) # 원그래프는 비율을 먼저 계산해야한다.
pie(tbl, main = "선실별 탑승객", col = c('blue', 'green','yellow'))
