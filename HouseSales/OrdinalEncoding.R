setwd('C:/Users/po020/Desktop/교육/데이터분석 실무자와 빅데이터 인프라부터 데이터 분석 경험하기')

files = c('1st_train_mdf.csv')

# 1. 데이터 가져오기 및 확인
ds <- read.csv(files)
head(ds)
str(ds) # 1460,81
summary(ds)
colnames(ds)
#unique(ds$BsmtFinType1)
head(ds,1)

# 2. Ordinal Encoding****

oe <- ds
for(i in 1:ncol(oe)){
  if(oe[1,i] %in% c('Po','Fa','TA','Gd','Ex')){
    oe[,i] <- factor(oe[,i],
                      levels = c('Po','Fa','TA','Gd','Ex'),
                      labels = c(1:5))
    oe[,i] <- as.integer(oe[,i])
  }
}
head(oe)
class(oe$KitchenQual)

# 변수 타입별로 컬럼 나누기
ds.int <- NULL
col.int <- NULL
ds.char <- NULL
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

# 수치형 히스토그램, 박스플롯
par(mfrow=c(1,2))
for(i in 1:ncol(ds.int)){
  hist(ds.int[,i], main=names(ds.int[i]), xlab=names(ds.int[i]), col='steelblue3')
  boxplot(ds.int[,i], main=names(ds.int[i]), xlab=names(ds.int[i]), col='steelblue3')
}
par(mfrow=c(1,1))
summary(ds.int) # 사분위 수
# 수치형 상관계수
library(corrplot)
ds.cor <- cor(ds.int)
which(ds.cor > 0.5 & ds.cor < 1)
head(ds.cor)
corrplot(ds.cor[-1,-1])
# 범주형 막대그래프
for(i in 1:ncol(ds.char)){
  barplot(table(ds.char[,i]), main=names(ds.char[i]), las=2, col='steelblue3')
}

# 건물 년식으로 시계열 데이터
library(dplyr)
ds.ord <- arrange(ds, ds$YearBuilt)
plot(ds.ord$SalePrice)
ds.ord.int <- NULL
for (i in 1:ncol(ds.ord)){
  if(class(ds.ord[,i])=='integer'){
    ds.ord.int <- cbind(ds.ord.int,ds.ord[,i])
  }
}
colnames(ds.ord.int) <- col.int
ds.ord.int <- as.data.frame(ds.ord.int)
head(ds.ord.int)

# 시계열 그래프
for(i in 1:ncol(ds.int)){
  plot(ds.ord.int$YearBuilt, ds.ord.int[,i], xlim=c(1980,2010),
       main=names(ds.ord.int[i]), ylab=names(ds.ord.int[i]), xlab='YearBuilt',
       col = 'steelblue')
}
plot(ds.ord.int$YearBuilt, ds.ord.int$TotalBsmtSF,xlim=c(1990,2020), ylim=c(200,2500))

# lm으로 영향 마니 끼치는 애 누군지.
summary(lm(ds$YearBuilt~.,data=ds.int))
ds$ExterCond


# 4. 결측치 확인
for(i in 1:ncol(ds)){
  cat(names(ds)[i], sum(is.na(ds[,i])), sum(is.na(ds[,i]))/nrow(ds),'\n')
}
# Alley 1369 0.9376712
# PoolQC 1453 0.9952055 
# Fence 1179 0.8075342 
# MiscFeature 1406 0.9630137 


# 결측치 없는 데이터 셋 만듦
ds1 <- subset(ds, select=-c(Alley, PoolQC, Fence, MiscFeature)) # 결측치가 80%넘는 컬럼 삭제
head(ds1)
# ds1 <- ds1[complete.cases(ds1),] # 결측치 있는 행 제거 하니까 넘 많이 없어지는걸??

# 숫자형 -> 사분위 수 구하기
summary(ds1)
ds.int <- NULL
col <- NULL
for(i in 1:ncol(ds1)){
  if(class(ds1[,i])=="integer"){
    ds.int <- cbind(ds.int,ds1[,i])
    col <- cbind(col, names(ds1[i]))
  }
}
colnames(ds.int) <- col
head(ds.int)
summary(ds.int)
class(ds1[,3])
