{
  setwd('C:/Users/po020/Desktop/교육/데이터분석 실무자와 빅데이터 인프라부터 데이터 분석 경험하기/HouseSales')
  
  files = c('data/1st_train_encoding3.csv')
  ds <- read.csv(files)
  colnames(ds)
  ds <- ds[,-c(1,2)]
  colnames(ds)
}
# 이진변수 수치화
{
  unique(ds$Street) #Pave Grvl 포장 자갈
  unique(ds$CentralAir) # Y N
  ds$Street <- factor(ds$Street,
                      levels = c('Grvl', 'Pave'),
                      labels = c(0,1))
  ds$CentralAir <- factor(ds$CentralAir,
                          levels = c('N','Y'),
                          labels = c(0,1))
  ds$Street <- as.integer(ds$Street)
  ds$CentralAir <- as.integer(ds$CentralAir)
}
# 수치형데이터만 추출
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

# PCA하고 중요하다고 생각하는 변수 시각화
# 1 GarageYrBlt     0.551
# 2 BsmtQual        0.465
# 3 ExterQual       0.465
# 4 GarageArea      0.465
# 5 GarageCars      0.465
# 6 GarageFinish    0.465
# 7 KitchenQual     0.465
# 8 OverallQual     0.465
# 9 SalePrice       0.465
# 10 YearBuilt       0.465
# 11 BsmtFinSF1      0.334
# 12 BsmtFinType2    0.278
# 13 BsmtUnfSF       0.264

imp <- ds[, c(
  "GarageYrBlt", "BsmtQual", "ExterQual", "GarageArea", "GarageCars",
  "GarageFinish", "KitchenQual", "OverallQual", "SalePrice", "YearBuilt",
  "BsmtFinSF1", "BsmtFinType2", "BsmtUnfSF"
)]

par(mfrow=c(4,4))
par(mfrow=c(1,1))
plot(imp)

plot(ds$YearBuilt, ds$GarageArea, ylim=c(180,1450))
cor(ds$YearBuilt, ds$GarageArea, method = 'spearman')
# 0.5282813
plot(ds$YMsold, ds$GarageArea, ylim=c(180, 1450))
plot
ds$YMsold <- as.Date(ds$YMsold)

cor(ds$YearBuilt, ds$BsmtQual, method = 'spearman')
# 0.7743734
cor(ds$YearBuilt, ds$ExterQual, method = 'spearman')
# 0.6789975

corrplot(cor(imp, method = 'spearman'), order='FPC')
###
?corrplot()
plot(ds$BsmtUnfSF, ds$BsmtFinSF1)

# 데이터프레임에서 필요한 열만 선택
data_subset <- ds[, c("Neighborhood", "SaleCondition")]

# 막대 그래프 그리기
# table(data_subset)
# barplot(table(data_subset), beside=TRUE, legend=TRUE,
#         main = "Sale Condition by Neighborhood",
#         xlab = "Neighborhood", ylab = "Count",
#         col = c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightcyan"))
  
ggplot(data_subset, aes(x = Neighborhood, fill = SaleCondition)) +
  geom_bar(position = "dodge") +
  labs(title = "Sale Condition by Neighborhood", x = "Neighborhood", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ds$MSSubClass
data_subset <- ds[, c("MSSubClass", "SaleCondition")]
ggplot(data_subset, aes(x = MSSubClass, fill = SaleCondition)) +
  geom_bar(position = "dodge") +
  labs(title = "Sale Condition by MSSubClass", x = "MSSubClass", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data_subset,aes(x=MSSubClass,fill=SaleCondition))+
  geom_bar(position ="dodge")
data_subset$MSSubClass <- as.character(data_subset$MSSubClass)
table(data_subset)
