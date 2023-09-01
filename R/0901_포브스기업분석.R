# 포브스 데이터 분석 사례
# 포브스에서 선정 2004년 세계 2000대 기업 리포트
library(HSAUR)

data('Forbes2000')
ds <- Forbes2000 # 데이터셋으로 받아서 다루쟈
class(ds)
str(ds)# 2000행 8열
head(ds)

# 1. 결측치 확인해보기
ds[ ! complete.cases(ds), ] # 모든 열에서 결측치 확인


# 2. 빈도수 구하기
table(ds$country)

# 내림차순으로 정렬해서 탑10국 구해씀
tmp <- sort(table(ds$country), decreasing=T)
top.10.contry <- tmp[1:10]

# 3. 막대그래프 그리기
par(mar=c(8,4,4,2)) # 그래프 여백 조정
barplot(top.10.contry, main='포브스 선정 기업 상위 10개국',
        col=rainbow(10), las=2)



# 업종별 기업 분포
head(ds)
table(ds$category)
tmp <- sort(table(ds$category), decreasing = T)
top.10.category <- tmp[1:10]
top.10.category
barplot(top.10.category, main='포브스 선정 많은 업종',
        col='pink', las=2)


# 업종별 기업 자산 분포
head(ds)

# A%in%B 연산 : A에 있는 값 중 B에 속하는 값 찾기
tmp <- ds[ds$category %in% names(top.10.category),]
levels(tmp$category)

# 상위 10개 업종에 포함되는 행만 추출해 tmp에 저장
tmp$category <- factor(tmp$category)
levels(tmp$category)

# 그래프 그림 y축은 자산, x축은 카테고리
par(mar=c(10,4,4,2)) # 그래프 여백 지정
boxplot(assets ~ category, data=tmp, ylim=c(0,100),xlab='',las=2, col=rainbow(10))
par(mar=c(5,4,4,2))

# 기업가치 상위 10대 기업
# 마켓벨류를 기준으로 내림차순 정렬
tmp <- ds[order(ds$marketvalue, decreasing = T),]
# 그 중에서 가치top 10으로 컬럼은 지정하여서
tmp[1:10,c('name','country','category','marketvalue')]

# 한국기업 정보
korea <- subset(ds, country=='South Korea')
korea[,c('rank','name','category','marketvalue')]


# 기업가치와 타 변수와의 상관관계
tmp <- ds[,5:8] # 수치형 변수만 골라씀
tmp <- tmp[complete.cases(tmp),] # 결측치 제거
plot(tmp,lower.panel=NULL) # 모든 변수간 산점도 그리는데 아래 그래프는 생략시켜라
cor(tmp) # 상관계수
# sales와 marketvalue가 0.64로 양의 상관관계