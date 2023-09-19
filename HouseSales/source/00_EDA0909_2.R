setwd('C:/Users/po020/Desktop/교육/데이터분석 실무자와 빅데이터 인프라부터 데이터 분석 경험하기/HouseSales')

files = c('data/1st_train_encoding3.csv')
ds <- read.csv(files)
colnames(ds)
#ds <- ds[,-1]
str(ds)

# overallCond vs overallqual
{
summary(ds$OverallCond) # 평균 5.575, 최대값 9, 중앙값 5 분산 1.238
summary(ds$OverallQual) # 평균 6.099, 최대값 10, 중앙값 6 분산 1.917
var(ds.int$OverallCond) # 1.238
var(ds.int$OverallQual) # 1.917


par(mfrow=c(2,2))
barplot(table(ds$OverallCond), main='OverallCond', col = 'royalblue', border=NA, xlim=c(1,10))
boxplot(ds$OverallCond, main ='OverallCond', col = 'royalblue', ylim=c(1,10))
barplot(table(ds$OverallQual), main='OverallQual', col = 'royalblue', border=NA)
boxplot(ds$OverallQual, main ='OverallQual', col = 'royalblue')
}

# 변수 타입별로 나누어보기
{
  ds.int <- NULL
  col.int <- NULL
  ds.char <- NULL
  col.char <- NULL
  for (i in 1:ncol(ds)){
    if(class(ds[,i])=='integer'){
      ds.int <- cbind(ds.int,ds[,i])
      col.int <- cbind(col.int, names(ds[i]))
    }
    else if(class(ds[,i])=='character'){
      ds.char <- cbind(ds.char,ds[,i])
      col.char <- cbind(col.char, names(ds[i]))
    }
  }
  colnames(ds.int) <- col.int
  colnames(ds.char) <- col.char
  ds.int <- as.data.frame(ds.int)
  ds.char <- as.data.frame(ds.char)
  str(ds.int) # 1460,53
  str(ds.char) # 1460,28
}

str(ds.int) #1460 53
str(ds.char) #1460 29 YMsold여깄음
summary(ds$MiscVal)
plot(ds$MiscVal[ds$MiscVal>0])

# 
ds_lm <- lm(ds$SalePrice~ds$YearBuilt, data=ds)
summary(ds_lm) # -2.530e+06,1.375e+03
summary(ds$SalePrice)
var(ds$SalePrice)
plot(ds$YearBuilt, ds$SalePrice, pch=16, col='royalblue')
abline(-2.530e+06,1.375e+03, col='red', lwd=2)
cor(ds$SalePrice, ds$YearBuilt, method='spearman')

hist(ds.int$GarageArea, col='royalblue', main = 'GarageArea')
barplot(table(ds$MoSold), col='royalblue', main = 'MoSold', border=NA)
# 상관계수
cor.mat <- cor(ds.int, method = 'spearman')
corrplot(cor(ds.int, method = 'spearman'),order='hclust')


