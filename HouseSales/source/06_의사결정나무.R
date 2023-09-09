# 거래 유형

setwd('C:/Users/po020/Desktop/교육/데이터분석 실무자와 빅데이터 인프라부터 데이터 분석 경험하기')

files = c('HouseSales/data/1st_train_encoding.csv')

# 1. 데이터 가져오기 및 확인
ds <- read.csv(files)
colnames(ds)
summary(ds)

ds <- ds[,-1]
head(ds)


#
unique(ds$SaleCondition)
ds$SaleCondition <- factor(ds$SaleCondition,
                           levels = c('Normal', 'Abnorml','AdjLand','Alloca','Family','Partial'),
                           labels = c('Normal', 'Abnormal','AdjLand','Alloca','Family','Partial'))
plot(ds$SaleCondition, col='royalblue', border=NA, main = 'SaleCondition', las=2)
table(ds$SaleCondition)

sc <- ds$SaleCondition

## char 형 factor변환
for (i in 1:ncol(ds)){
  if(class(ds[,i])=='character'){
    ds[,i] <- as.factor(ds[,i])
  }
}
class(ds[,9])

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

colnames(ds.int)
ds.int <- cbind(ds.int,sc)

model1 <- ctree(ds.int$sc~., data=ds.int, controls = ctree_control(maxdepth = 3))
plot(model1, type='simple')

library(party)
library(C50)
library(rpart)
library(rpart.plot)
tree_model <- ctree(ds$SaleCondition~., data = ds)
tree_model <- ctree(ds$SaleCondition~., data = ds,
                    controls = ctree_control(maxdepth = 10))
summary(tree_model)
tree_model
plot(tree_model)
tree_model

#fit <- rpart(ds$SaleCondition~., data=ds, method='class', control=rpart.control(maxdepth=1))
rpart.plot()