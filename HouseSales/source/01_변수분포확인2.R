setwd('C:/Users/po020/Desktop/교육/데이터분석 실무자와 빅데이터 인프라부터 데이터 분석 경험하기')

files = c('HouseSales/data/1st_train_mdf.csv')

# 1. 데이터 가져오기 및 확인
ds <- read.csv(files)
summary(ds)

# 2. NA를 0으로
# 결측치를 제거하기보다 NA인거는 아예 엾음을 뜻해서.
ds[is.na(ds)] <- 0


# 3. 수치형변수와 문자형 변수 나눔
ds.int <- NULL
ds.char <- NULL
col.int <- NULL
col.char <- NULL

for (i in 1:ncol(ds)){
  if(class(ds[,i])=='integer'){
    ds.int <- cbind(ds.int,ds[,i])
    col.int <- cbind(col.int, names(ds[i]))
  }
  else{
    ds.char <- cbind(ds.char,ds[,i])
    col.char <- cbind(col.char, names(ds[i]))
  }
}
colnames(ds.int) <- col.int
colnames(ds.char) <- col.char
ds.int <- as.data.frame(ds.int)
ds.char <- as.data.frame(ds.char)
str(ds.int) # 1460,38
str(ds.char) # 1460,43
head(ds.int)
head(ds.char)


# 4. 시각화
# 4.1. 수치형 시각화
par(mfrow=c(1,2))
for(i in 1:ncol(ds.int)){
  hist(ds.int[,i], main=names(ds.int[i]), xlab=names(ds.int[i]), col='steelblue3')
  boxplot(ds.int[,i], main=names(ds.int[i]), xlab=names(ds.int[i]), col='steelblue3')
}

# 4.2. 문자형 시각화
# 범주형 막대그래프
for(i in 1:ncol(ds.char)){
  barplot(table(ds.char[,i]), main=names(ds.char[i]), las=2, col='steelblue3')
}
par(mfrow=c(1,1))