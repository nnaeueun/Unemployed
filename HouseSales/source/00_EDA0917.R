{
  setwd('C:/Users/po020/Desktop/교육/데이터분석 실무자와 빅데이터 인프라부터 데이터 분석 경험하기/HouseSales')
  
  files = c('data/1st_train_encoding3.csv')
  ds <- read.csv(files)
  colnames(ds)
  ds <- ds[,-c(1,2)]
  colnames(ds)
}

average_by_month <- aggregate(ds$SalePrice, by = list(ds$MoSold), FUN = mean)

cormat <- cor(ds.int,method='spearman')

cor_threshold <- 0.5
high_cor_pairs <- which(abs(cormat) >= cor_threshold & cormat != 1, arr.ind=TRUE)

# 변수 이름 추출
variable_names <- colnames(cormat)
high_cor_variable_pairs <- data.frame(
  Variable1 = variable_names[high_cor_pairs[, 1]],
  Variable2 = variable_names[high_cor_pairs[, 2]],
  Correlation = cormat[high_cor_pairs]
)

high_cor_variable_pairs


ds$SaleCondition <- as.factor(ds$SaleCondition)
count_by_month <- table(ds$MoSold, ds$SaleCondition)

ds.int$MoSold <- as.integer(ds.int$MoSold)
model1 = lm(ds.int$MoSold~., data = ds.int)
summary(model1)
plot(ds$MoSold)
points(model1$fitted.values, col='red')
model1$fitted.values

summary(aov(ds.int$OverallQual~ds.int$MoSold, data = ds.int))


ds <- cbind(ds, dummy_mosold)


# 데이터 프레임에 MoSold를 더미 변수로 변환 (1월부터 12월까지)
for (month in 1:12) {
  ds[paste0("MoSold_", month)] <- ifelse(ds$MoSold == month, 1, 0)
}
colnames(ds)

ds <- ds[,-c(52,53,54)]
ds1 <- ds[,-c(53:63)]
colnames(ds1)
model1 <- glm(ds1$MoSold_1~., data=ds1)
summary(model1)


ds.int$MoSold <- as.factor(ds.int$MoSold)
plot(ds.int$MoSold, ds$LotArea,main=names(ds[5]))

for(i in 1:ncol(ds.int)){
  plot(ds.int$MoSold, ds.int[,i], main=names(ds.int[i]),
       xlab='Month', ylab=names(ds.int[i]),
       col = 'royalblue')
}

library(corrplot)
corrplot(cor(ds.int, method='spearman'))
cor(ds)

lo1 <- glm(ds$MoSold_1~ds$OverallQual+ds$BsmtQual+ds$GarageCars+ds$BedroomAbvGr+ds$KitchenAbvGr+ds$TotRmsAbvGrd)
summary(lo1)
lo2 <- glm(ds$MoSold_2~ds$OverallQual+ds$BsmtQual+ds$GarageCars+ds$BedroomAbvGr+ds$KitchenAbvGr+ds$TotRmsAbvGrd)
summary(lo2)
plot(ds$MoSold_1)
points(lo2$y, col='red')
ds$MoSold_1

barplot(table(ds.int$MoSold,ds.char$HouseStyle), legend=TRUE)

par(mfrow=c(1,2))
for(i in 1:ncol(ds.char)){
  barplot(table(ds$MoSold_6,ds.char[,i]), legend=TRUE,
          main=names(ds.char[i]))
  barplot(table(ds$MoSold_2,ds.char[,i]), legend=TRUE,
          main=names(ds.char[i]))
}
