# 데이터 전처리
# 결측치 다루기

# 1. na.rm=TRUE
z <- c(1,2,3,NA,5,NA,8)
sum(z) # NA값 포함된 연산 결과 NA반환
sum(is.na(z)) # 결측값 2개
sum(z, na.rm=TRUE) # 결측치 제외한 숫자 합

# 2. na.omit
z1 <- c(1,2,3,NA,5,NA,8)
z2 <- c(5,8,1,NA,3,NA,7)

z1[is.na(z1)] <- 0 # NA를 0으로 치환
z1

z3 <- as.vector(na.omit(z2)) # z2에 결측치를 제거하고 새 벡터
z3


# 매트릭스와 데이터프레임의 결측값

# NA를 포함하는 test데이터 생성
# 걍 아이리스 데이터에 NA값을 쑤셔넣은거임
x <- iris
x[1,2]<-NA; x[1,3]<-NA
x[2,3]<- NA; x[3,4]<-NA
head(x)

# for를 이용해서 결측치 개수 확인
for(i in 1:ncol(x)){
  this.na <- is.na(x[,i]) # 컬럼에 결측치를 this.na변수에 저장한거
  cat(colnames(x)[i], '\t', sum(this.na),'\n')
  # cat()함수 이용해서 출력하는거 칼럼이름 출력하고 결측치의 수를 합하는거임
}

# apply를 이용해서 결측치 개수 확인
col_na <- function(y){
  return(sum(is.na(y)))
} # 일단 함수 만듦

# apply() 함수를 호출해서 써먹는 함수
na_count <- apply(x, 2, FUN=col_na)
na_count


# 행 별로 결측치 개수 반환하기
rowSums(is.na(x)) # 행 별 NA개수
sum(rowSums(is.na(x))>0) # NA가 포함된 행 수
sum(is.na(x)) # 데이터 셋 전체에서 NA수


# NA없는 데이터셋만들기 complete.cases()
# NA가 없는 행의 인덱스를 반환
head(x)
x[!complete.cases(x),] # NA가 있는 행
y <- x[complete.cases(x),] # NA가 포함된 행 제거
head(y)


# carData패키지 UN데이터셋 결측값 제거하고 분석해보기
library(carData)
head(UN)
str(UN)

# 열 별 NA 개수 구하기
col_na <- function(y){
  return(sum(is.na(y)))
}

apply(UN, 2, FUN=col_na)
# region에는 14, group 14, fertility 14, ...

# 결측치 제거하고 새 변수
a <- UN[complete.cases(UN),]
str(a)

# 여성 평균 수명(lifeExpF)
mean(a$lifeExpF)
mean(UN$lifeExpF, na.rm=T) # NA제외 계산

# 도시지역 평균비율(pctUrban)과 영아 사망률 평균비율(infantMortality)
tmp <- UN[,c('pctUrban','infantMortality')]
tmp <- tmp[complete.cases(tmp),] # NA가 있는 행 제거
colMeans(tmp)
tmp

# 아시아의 평균 영아사망률
# subset() 조건에 맞는 데이터 추출
tmp <- subset(UN, region=='Asia')
mean(tmp$fertility, na.rm=T)
