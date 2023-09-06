#install.packages('party')
#install.packages('caret')
library(party)
library(caret)

setwd('C:/Users/po020/Desktop/교육/데이터분석 실무자와 빅데이터 인프라부터 데이터 분석 경험하기')

files = c('HouseSales/data/1st_train_encoding.csv')

# 1. 데이터 가져오기 및 확인
ds <- read.csv(files)
summary(ds)

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
SaleCondition <- ds.char$SaleCondition
SaleCondition <- as.factor(SaleCondition)
head(SaleCondition)
ds.int <- cbind(ds.int, SaleCondition)
sum(is.na(ds.int))
tail(ds.int)
tree_model <- ctree(ds.int[,58]~., data = ds.int)
plot(tree_model)

barplot(table(ds.int[,58]), col = 'royalblue', border = NA, las=2, main = 'Sale Condition')


cor(ds.int,method = 'spearman')

ds <- ds.int[,-58]
ds[is.na(ds)]<-0
head(ds)
ds.int <- ds.int[,-c(1,2)]
a <- cor(ds.int, method='spearman')

library(corrplot)
corrplot(a>0.5)
which(a>0.5)

b <- a[a>0.5]
a
cor(ds.int$BsmtQual[ds.int$BsmtQual>0], ds.int$BsmtCond[ds.int$BsmtQual>0], method = 'spearman')
# 0.3169468
# 0.199183 (0제외하고 보면)
cor(ds.int$GarageQual[ds.int$GarageQual>0], ds.int$GarageCond[ds.int$GarageCond>0], method = 'spearman')
# 0.8171323
# 0.547505 (0제외하고 보면)
head(ds.int)

mahalanobis(ds.int, colMeans(ds.int), cov(ds.int))

#install.packages('philentropy')
library(philentropy)
distance(ds.int,method='euclidean')
getDistMethods()
colMeans(ds.int)
head(ds.int)
