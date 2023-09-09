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

# 2. 변수 타입별로 컬럼 나누기
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
# 그래프 이미지 저장
output_directory <- "C:/Users/po020/Desktop/교육/데이터분석 실무자와 빅데이터 인프라부터 데이터 분석 경험하기/HouseSales/plot/01_hist_box"

# 수치형 히스토그램, 박스플롯
par(mfrow=c(1,2))
for(i in 1:ncol(ds.int)){
  hist(ds.int[,i], main=names(ds.int[i]), xlab=names(ds.int[i]), col='royalblue')
  boxplot(ds.int[,i], main=names(ds.int[i]), xlab=names(ds.int[i]), col='royalblue')
  
  # # 그래프 이미지 저장
  # filename <- file.path(output_directory, paste("수치형",i,'.png', sep=""))
  # png(filename)
  # hist(ds.int[,i], main=names(ds.int[i]), xlab=names(ds.int[i]), col='royalblue')
  # boxplot(ds.int[,i], main=names(ds.int[i]), xlab=names(ds.int[i]), col='royalblue')
  # dev.off()
  # 
  # cat("그래프",i,"저장완료\n")
}
par(mfrow=c(1,1))
summary(ds.int) # 사분위 수
table(ds$OverallQual)
unique(ds$OverallCond)
# 수치형 상관계수
library(corrplot)
ds.cor <- cor(ds.int)
which(ds.cor > 0.5 & ds.cor < 1)
head(ds.cor)
corrplot(ds.cor[-1,-1])

sum(is.na(ds$GarageYrBlt))

# 범주형 막대그래프
ds.char[is.na(ds.char)] <- 'NA'
for(i in 1:ncol(ds.char)){
  barplot(table(ds.char[,i]), main=names(ds.char[i]), las=2, col='royalblue', border = NA)
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
for(i in 1:ncol(ds.ord.int)){
  plot(ds.ord.int$YearBuilt, ds.ord.int[,i],
       main=names(ds.ord.int[i]), ylab=names(ds.ord.int[i]), xlab='YearBuilt',
       col = 'royalblue')
}

plot(ds.ord.int$YearBuilt, ds.ord.int$TotalBsmtSF,xlim=c(1990,2020), ylim=c(200,2500))





#### 리모델링 관련
yR <- ds$YearRemodAdd - ds$YearBuilt
mean(yR) # 13.59795년 뒤에 리모델링을 한다.

barplot(table(yR[yR>1]))

which(yR>100)
ds[1138,]

yR <- ds$YrSold- ds$YearBuilt
# 지어진 지 36.54795년만에 팔리지만 중앙 값 35년
median(yR)
boxplot(yR)
barplot(table)

a <- ds$YrSold - ds$YearRemodAdd

##

sum(is.na(ds$LowQualFinSF))
sum(is.na(ds$FireplaceQu))
sum(is.na(ds$Fence))
