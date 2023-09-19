# 수치형데이터에서 중요한 변수 뽑아보기
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
  ds$CentralAir <- as.inteager(ds$CentralAir)
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

# 0 개수 확인 및 0이 0.5넘는 변수 제거
{
  ds <- NULL
  ds.name <- NULL
  zero.name <- NULL
  for(i in 1:ncol(ds.int)){
    cat(names(ds.int)[i], sum(ds.int[,i]==0), sum(ds.int[,i]==0)/nrow(ds.int),'\n')
    if(sum(ds.int[,i]==0)/nrow(ds.int) < 0.5){
      ds.name <- cbind(ds.name, names(ds.int[i]))
      ds <- cbind(ds, ds.int[,i])
    }else{
      zero.name <- cbind(zero.name, names(ds.int[i]))
    }
  }
  zero.name[1,] #BsmtFinSF2, LoqQualFinSF, BsmtHalfBath, EnclosedPorch, X4SsnPorch, ScreenPorch, PoolArea, PoolQC, Fence, MiscVal
  ds <- as.data.frame(ds)
  colnames(ds) <- ds.name
}
head(ds) # 38행
# #제거된 변수
# "MasVnrArea"    "BsmtFinSF2"    "X2ndFlrSF"     "LowQualFinSF"  "BsmtFullBath" 
# [6] "BsmtHalfBath"  "HalfBath"      "WoodDeckSF"    "EnclosedPorch" "X3SsnPorch"   
# [11] "ScreenPorch"   "PoolArea"      "PoolQC"        "Fence"         "MiscVal"


# 중요 변수 뭔지 
# 1. 스피어만상관계수
cor.mat <- cor(ds, method = 'spearman')
corrplot(cor.mat, order = 'hclust', addrect = 9)

# 2. 상관계수로 PCA
pp <- prcomp(cor.mat, scale=T)
pc <- summary(pp) # PC10까지 전체 분산 0.90 이상 설명한다

pc <- pc$importance[2,c(1:10)]

pr <- pp$rotation
aa <- NULL
aa.name <-NULL
bb <- NULL
for(i in 1:10){ # PC11까지 보겠다는 것
  aa <- sort(abs(pr[,i]),T)
  aa.name <- cbind(aa.name, names(aa[1:5]))
  bb<- cbind(bb, aa[c(1:5)])
  print(head(aa,5))
}
head(bb)
head(aa.name)



table(aa.name[,1])*0.4

barplot(table(aa.name), las=2, col = 'royalblue', border=NA) # 28개 변수 선택됨

plot(ds$MSSubClass)


## 가중치 부여해서 계산
table(aa.name[,2])*pc[2]
for(i in 1:10){ #PC10까지 weight
  table(aa.name[,i])*pc[i]
  print(table(aa.name[,i])*pc[i])
}


weights_vector <- c(
  "ExterQual" = 0.46541,
  "GarageCars" = 0.46541,
  "GarageFinish" = 0.46541,
  "OverallQual" = 0.46541,
  "SalePrice" = 0.46541,
  "BedroomAbvGr" = 0.11605,
  "BsmtCond" = 0.11605,
  "GrLivArea" = 0.11605,
  "KitchenAbvGr" = 0.11605,
  "TotRmsAbvGrd" = 0.11605,
  "BsmtFinSF1" = 0.08592,
  "BsmtFinType2" = 0.08592,
  "LotArea" = 0.08592,
  "MSSubClass" = 0.08592,
  "X1stFlrSF" = 0.08592,
  "ExterCond" = 0.04529,
  "Fireplaces" = 0.04529,
  "LotFrontage" = 0.04529,
  "MSSubClass" = 0.04529,
  "TotalBsmtSF" = 0.04529,
  "BsmtCond" = 0.04171,
  "BsmtUnfSF" = 0.04171,
  "GarageCond" = 0.04171,
  "GarageQual" = 0.04171,
  "YrSold" = 0.04171,
  "BsmtExposure" = 0.03816,
  "BsmtFinSF1" = 0.03816,
  "GarageCond" = 0.03816,
  "GarageQual" = 0.03816,
  "MSSubClass" = 0.03816,
  "GarageCond" = 0.03326,
  "GarageQual" = 0.03326,
  "MoSold" = 0.03326,
  "OverallCond" = 0.03326,
  "YrSold" = 0.03326,
  "BsmtCond" = 0.02896,
  "BsmtFinType2" = 0.02896,
  "FireplaceQu" = 0.02896,
  "Fireplaces" = 0.02896,
  "Street" = 0.02896,
  "BsmtCond" = 0.02375,
  "BsmtFinType2" = 0.02375,
  "ExterCond" = 0.02375,
  "MoSold" = 0.02375,
  "Street" = 0.02375,
  "BsmtFinSF1" = 0.02301,
  "BsmtUnfSF" = 0.02301,
  "ExterCond" = 0.02301,
  "FireplaceQu" = 0.02301,
  "Fireplaces" = 0.02301
)
weight <- NULL
weight <- as.data.frame((weights_vector))
weight <-cbind(weight, names(weights_vector))
colnames(weight) <- c('값','변수')
weight$변수 <- as.factor(weight$변수)
sum(weight$값[weight$변수 == 'BsmtCond'])
table(weight$변수) # 28개

sum(weight$값[weight$변수 == 'BedroomAbvGr'])
sum(weight$값[weight$변수 == 'BsmtCond'])
sum(weight$값[weight$변수 == 'BsmtExposure'])
sum(weight$값[weight$변수 == 'BsmtFinSF1'])
sum(weight$값[weight$변수 == 'BsmtFinType2'])
sum(weight$값[weight$변수 == 'BsmtUnfSF'])
sum(weight$값[weight$변수 == 'ExterCond'])
sum(weight$값[weight$변수 == 'ExterQual'])
sum(weight$값[weight$변수 == 'FireplaceQu'])
sum(weight$값[weight$변수 == 'Fireplaces'])
sum(weight$값[weight$변수 == 'GarageCars'])
sum(weight$값[weight$변수 == 'GarageCond'])
sum(weight$값[weight$변수 == 'GarageFinish'])
sum(weight$값[weight$변수 == 'GarageQual'])
sum(weight$값[weight$변수 == 'GrLivArea'])
sum(weight$값[weight$변수 == 'KitchenAbvGr'])
sum(weight$값[weight$변수 == 'LotArea'])
sum(weight$값[weight$변수 == 'LotFrontage'])
sum(weight$값[weight$변수 == 'MoSold'])
sum(weight$값[weight$변수 == 'MSSubClass'])
sum(weight$값[weight$변수 == 'OverallCond'])
sum(weight$값[weight$변수 == 'OverallQual'])
sum(weight$값[weight$변수 == 'SalePrice'])
sum(weight$값[weight$변수 == 'Street'])
sum(weight$값[weight$변수 == 'TotalBsmtSF'])
sum(weight$값[weight$변수 == 'TotRmsAbvGrd'])
sum(weight$값[weight$변수 == 'X1stFlrSF'])
sum(weight$값[weight$변수 == 'YrSold'])





# 변수 이름과 가중치를 데이터프레임으로 변환
weight_df <- data.frame(Variable = names(weights_vector), Weight = unlist(weights_vector))

# 특정 변수들에 대한 가중치 합계 계산
variables_to_sum <- c(
  'BedroomAbvGr', 'BsmtCond', 'BsmtExposure', 'BsmtFinSF1', 'BsmtFinType2',
  'BsmtUnfSF', 'ExterCond', 'ExterQual', 'FireplaceQu', 'Fireplaces',
  'GarageCars', 'GarageCond', 'GarageFinish', 'GarageQual',
  'GrLivArea', 'KitchenAbvGr', 'LotArea', 'LotFrontage', 'MoSold',
  'MSSubClass', 'OverallCond', 'OverallQual', 'SalePrice', 'Street',
  'TotalBsmtSF', 'TotRmsAbvGrd', 'X1stFlrSF', 'YrSold'
)

sums <- sapply(variables_to_sum, function(var) {
  sum(weight_df$Weight[weight_df$Variable == var])
})

# 결과를 데이터프레임으로 저장
result_df <- data.frame(Variable = variables_to_sum, SumWeight = sums)

# 결과 출력
print(result_df)
barplot(result_df$SumWeight)



library(ggplot2)

# 막대 그래프 생성
bar_plot <- ggplot(result_df, aes(x = Variable, y = SumWeight)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # x 축 레이블 회전
  labs(x = "Variable", y = "Sum of Weights", title = "Sum of Weights by Variable")

# 그래프 표시
print(bar_plot)


barplot(score$SumWeight, names.arg = score$Variable, 
        col = "royalblue", las = 2,border=NA,
        ylab = "Sum of Weights", cex.names = 0.7)





score <- result_df[order(-result_df$SumWeight),]
head(score[,1],10)

hist(ds$GarageCars)
hist(ds$GarageFinish)
hist(ds$OverallQual)
hist(ds$BsmtCond)
hist(ds$MSSubClass)
hist(ds$BsmtFinSF1)
hist(ds$BedroomAbvGr)
colnames(ds)
score # 28개

sort(abs(pp$rotation[,1]),T)
