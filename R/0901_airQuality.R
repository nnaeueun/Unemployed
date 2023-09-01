# 공기오염 데이터
# 로컬 데이터 가져옴
setwd('C:/Users/po020/Desktop/교육/R통계')
files = c('ds.2015.csv','ds.2016.csv','ds.2017.csv','ds.2018.csv','ds.2019.csv')

# 1. 데이터 가져오기
ds <- NULL
for(f in files){
  tmp <- read.csv(f, header=T)
  ds <- rbind(ds,tmp) # ds에 데이터를 합침
  print(f)
}
head(ds)
str(ds)
unique(ds$loc)
range(ds$mdate) # 2015년 1월부터 2019년 12월까지임

# 2. 결측치 확인
# 열 별 결측치 확인
# 결측치 갯수와 결측치가 전체 데이터 셋에서 몇 개인지 구함
for(i in 3:8){
  cat(names(ds)[i], sum(is.na(ds[,i])), sum(is.na(ds[,i]))/nrow(ds),'\n')
}

# 결측치 없는 데이터 셋 만듦
ds <- ds[,-8] # PM25열 제거(18% 결측치)
ds <- ds[complete.cases(ds),] # 결측치 있는 행 제거

# 3. 날짜 시간 별 변수 추가, 변수 명 추가
mdate = as.character(ds$mdate)
head(mdate)
ds$year <- as.numeric(substr(mdate, 1,4)) # 연도
ds$month <- as.numeric(substr(mdate,5,6)) # 월
# ds$date <- as.numeric(substr(mdate,7,8)) # 일일
ds$hour <- as.numeric(substr(mdate,9,10)) # 시간
ds$locname <- NA #locname 열 추가
head(ds)
ds$locname[ds$loc==111123] <- '서울'
ds$locname[ds$loc==336111] <- '목포'
ds$locname[ds$loc==632132] <- '강릉'
head(ds)

# 시각화
boxplot(PM10~locname, data=ds, main='미세먼지 농도 분포', ylim=c(1,100), col=rainbow(3))

# 연도별 평균 농도 시각화
library(ggplot2)
tmp.year <- aggregate(ds[,7], by=list(year=ds$year, loc=ds$locname), FUN='mean')
tmp.year$loc = as.factor(tmp.year$loc)
head(tmp.year)

ggplot(tmp.year, aes(x=year, y=x, colour=loc, group=loc))+
  geom_line()+
  geom_point(size=6, shape=19, alpha=0.5)+
  ggtitle('연도별 PM10 농도변화')+
  ylab('농도')

# 월별 지역별 PM10농도 추이
tmp.month <- aggregate(ds[,7], by=list(month=ds$month, loc=ds$locname), FUN='mean')
tmp.month$loc = as.factor(tmp.month$loc)
head(tmp.month)
ggplot(tmp.month, aes(x=month, y=x, colour=loc, group=loc))+
  geom_line()+
  geom_point(size=3, shape=19, alpha=0.5)+
  ggtitle('월별 PM10 농도변화')+
  ylab('농도')
# 봄에서 미세먼지 농도 높아지고, 우기인 여름에는 미세먼지 농도 낮아짐

# 오염물질 간 상관관계
set.seed(1234)
plot(ds[sample(nrow(ds),5000),3:7], lower.panel=NULL)
cor(ds[,3:7])
# 특별히 상관성을 가진 오염물질은 없다

# 미세먼지 최고점과 최저점 확인
tmp.yml <- aggregate(ds[,7], by=list(year=ds$year, month=ds$month, loc=ds$locname), FUN='mean')
# 미세먼지 가장 많은 달
idx <- which(tmp.yml$x==max(tmp.yml$x))
tmp.yml[idx,]
# 미세먼지 가장 적은 달
idx <- which(tmp.yml$x==min(tmp.yml$x))
tmp.yml[idx,]
