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

# 0 개수 확인 및 0이 0.8넘는 변수 제거
{
ds <- NULL
ds.name <- NULL
zero.name <- NULL
for(i in 1:ncol(ds.int)){
  cat(names(ds.int)[i], sum(ds.int[,i]==0), sum(ds.int[,i]==0)/nrow(ds.int),'\n')
  if(sum(ds.int[,i]==0)/nrow(ds.int) < 0.8){
    ds.name <- cbind(ds.name, names(ds.int[i]))
    ds <- cbind(ds, ds.int[,i])
  }else{
    zero.name <- cbind(zero.name, names(ds.int[i]))
  }
}
zero.name #BsmtFinSF2, LoqQualFinSF, BsmtHalfBath, EnclosedPorch, X4SsnPorch, ScreenPorch, PoolArea, PoolQC, Fence, MiscVal
ds <- as.data.frame(ds)
colnames(ds) <- ds.name
}
head(ds)
#제거된 변수
# BsmtFinSF2, LoqQualFinSF, BsmtHalfBath,
# EnclosedPorch, X4SsnPorch, ScreenPorch,
# PoolArea, PoolQC, Fence, MiscVal

# 중요 변수 뭔지 
# 1. 스피어만상관계수
cor.mat <- cor(ds, method = 'spearman')
corrplot(cor.mat, order = 'hclust', addrect = 7)

# 2. 상관계수로 PCA
pp <- prcomp(cor.mat, scale=T)
summary(pp) # PC11까지 전체 분산 0.90225 설명한다

pr <- pp$rotation
aa <- NULL
aa.name <-NULL
bb <- NULL
for(i in 1:11){ # PC11까지 보겠다는 것
  aa <- sort(abs(pr[,i]),T)
  aa.name <- cbind(aa.name, names(aa[1:5]))
  bb<- cbind(bb, aa[c(1:5)])
  print(head(aa,5))
}
head(bb)
head(aa.name)

barplot(table(aa.name), las=2)
# 0이 차지하는 영향이 매우 큰거같음
plot(ds$LotFrontage)
