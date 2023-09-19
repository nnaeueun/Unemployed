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
  str(ds.char) # 1460,27
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
  zero.name[1,]
  # 15개 변수 제거
  ds <- as.data.frame(ds)
  colnames(ds) <- ds.name
}
head(ds) # 38행
# #제거된 변수
# [1] "MasVnrArea"    "BsmtFinSF2"    "X2ndFlrSF"     "LowQualFinSF"  "BsmtFullBath" 
# [6] "BsmtHalfBath"  "HalfBath"      "WoodDeckSF"    "EnclosedPorch" "X3SsnPorch"   
# [11] "ScreenPorch"   "PoolArea"      "PoolQC"        "Fence"         "MiscVal"


# 중요 변수 뭔지 
# 1. 스피어만상관계수
library(corrplot)
cor.mat <- cor(ds, method = 'spearman')
corrplot(cor.mat, order = 'hclust', addrect = 9)

# 2. 상관계수로 PCA
pp <- prcomp(cor.mat, scale=T)
pc <- summary(pp) # PC10까지 전체 분산 0.90 이상 설명한다
pc <- pc$importance[2,c(1:10)] # PC별 분산

pr <- pp$rotation

weight <- NULL
score <- NULL
weight_Val <- NULL
# weight따지기 PC11까지
for(i in 1:10){
  weight <- abs(pr[,i]) * pc[i]
  score <- cbind(score, weight)
  weight_Val <- paste('pc',i,sep='')
  colnames(score)[i] <- weight_Val
}

barplot(sort(rowSums(score),T), las=2, cex.names = 0.8)


aa <- NULL
aa.name <-NULL
bb <- NULL
for(i in 1:10){ # PC11까지 보겠다는 것
  aa <- sort(abs(pr[,i]),T)
  aa.name <- cbind(aa.name, names(aa[1:10]))
  bb<- cbind(bb, aa[c(1:10)])
  print(head(aa,10))
}
head(bb)
head(aa.name)
barplot(sort(table(aa.name),T), las=2, cex.names = 0.8) # 35개변수 선택됨
aa.name 


## 가중치 부여해서 계산
result_df <- NULL
for(i in 1:10){ #PC10까지 weight
  result <- table(aa.name[,i])*pc[i]
  result_df <- rbind(result_df, data.frame(Variable = names(result), Value = as.numeric(result)))
  print(table(aa.name[,i])*pc[i])
}
result_df

# result_df를 변수 이름을 기준으로 그룹화하고 합계를 계산
library(dplyr)
result_summary <- result_df %>%  group_by(Variable) %>%  summarize(SumValue = sum(Value))
result_summary

# 내림차순정렬
result_summary_sorted <- result_summary %>%  arrange(desc(SumValue))

barplot(result_summary_sorted$SumValue, names.arg = result_summary_sorted$Variable,
        las=2, ylim=c(0,0.6),
        col = 'royalblue', border=NA, cex.names = 0.7)
mean(result_summary$SumValue)
abline(h = 0.2504222, col='red', lwd = 2)

head(result_summary_sorted, 13)
