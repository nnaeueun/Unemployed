## 데이터파악
{
setwd('C:/Users/po020/Desktop/교육/데이터분석 실무자와 빅데이터 인프라부터 데이터 분석 경험하기')

files = c('1st_train_mdf.csv')
}
# 1. 원본 데이터 가져오기 및 확인
{
ds <- read.csv(files)
head(ds) # 1460 81
str(ds)
summary(ds)
}
# 2. 데이터 타입 나누기
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
}
# 3. Ordinal Encoding
{
oe <- ds
for(i in 1:ncol(oe)){
  if(oe[1,i] %in% c('Po','Fa','TA','Gd','Ex')){
    oe[,i] <- factor(oe[,i],
                     levels = c('Po','Fa','TA','Gd','Ex'),
                     labels = c(1:5))
    oe[,i] <- as.integer(oe[,i])
  }
}
head(oe,3)
class(oe$FireplaceQu)
tail(oe$FireplaceQu,10)
unique(oe$PoolQC)
class(oe$KitchenQual)
oe$LotShape <- factor(oe$LotShape,
                      levels = c('IR3','IR2','IR1','Reg'),
                      labels = c(1:4))
oe$LotShape <- as.integer(oe$LotShape)
unique(oe$LotShape)
oe$BsmtExposure <- factor(oe$BsmtExposure,
                          levels = c('No','Mn','Av','Gd'),
                          labels = c(1:4))
oe$BsmtExposure <- as.integer(oe$BsmtExposure)
oe$BsmtFinType2 <- factor(oe$BsmtFinType2,
                          levels = c('Unf','LwQ','Rec','BLQ','ALQ','GLQ'),
                          labels = c(1:6))
oe$BsmtFinType2 <- as.integer(oe$BsmtFinType2)
unique(oe$BsmtFinType2)
oe$GarageFinish <- factor(oe$GarageFinish,
                          levels = c('Unf','RFn','Fin'),
                          labels = c(1:3))
oe$GarageFinish <- as.integer(oe$GarageFinish)
unique(oe$GarageFinish)
oe$Fence <- factor(oe$Fence,
                   levels = c('MnWw','GdWo','MnPrv','GdPrv'),
                   labels = c(1:4))
oe$Fence <- as.integer(oe$Fence)
unique(oe$Fence)
str(oe)
oe$PoolQC <- factor(oe$PoolQC,
                   levels = c('Fa','Gd','Ex'),
                   labels = c(1:3))
oe$PoolQC <- as.integer(oe$PoolQC)
unique(oe$PoolQC)
oe$FireplaceQu <- factor(oe$FireplaceQu,
                 levels = c('Po','Fa','TA','Gd','Ex'),
                 labels = c(1:5))
oe$FireplaceQu <- as.integer(oe$FireplaceQu)
unique(oe$FireplaceQu)
write.csv(oe, file='C:/Users/po020/Desktop/교육/데이터분석 실무자와 빅데이터 인프라부터 데이터 분석 경험하기/1st_train_encoding.csv')


}
# 3-1. 이진변수 Encoding

# 3-2. 시계열 데이터 가져오기
{
  df <- read.csv('1st_train_encoding2.csv')
  SalePrice <- ds$SalePrice
  YMsold <- df$YMsold
  ds <- ds[,-81]
  ds <- cbind(ds, YMsold)
  ds <- cbind(ds, SalePrice)
  str(ds)
  write.csv(ds, file='C:/Users/po020/Desktop/교육/데이터분석 실무자와 빅데이터 인프라부터 데이터 분석 경험하기/HouseSales/data/1st_train_encoding.csv')
}

# 4. 전처리 한 데이터 불러오기
files = c('1st_train_encoding.csv')
ds <- read.csv('1st_train_encoding.csv')
#ds <- ds[,-1]
colnames(ds) # 1460 82

# 3. 변수 타입별로 나누어보기
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

YMsold <- ds.char$YMsold
YMsold <- as.Date(YMsold)
head(YMsold)
ds.int <- cbind(ds.int, YMsold)

# 4. 수치형데이터 시각화

par(mfrow=c(1,3))
for(i in 1:(ncol(ds.int)-1)){
  plot(ds.int$YMsold,ds.int[,i], main=names(ds.int[i]), ylab=names(ds.int[i]), col='royalblue')
  hist(ds.int[,i], main=names(ds.int[i]), xlab=names(ds.int[i]), col='royalblue')
  boxplot(ds.int[,i], main=names(ds.int[i]), col='royalblue')
}

# 이미지 저장
{
setwd('C:/Users/po020/Desktop/교육/데이터분석 실무자와 빅데이터 인프라부터 데이터 분석 경험하기/HouseSales/plot')
output_dir <- "01_histbox3/"
dir.create(output_dir, showWarnings = FALSE)  # 디렉토리가 없으면 생성합니다.
ds.int <- cbind(ds.int, YMsold)
par(mfrow=c(1,3))
for(i in 1:ncol(ds.int)){
  par(mfrow=c(1,3))
  plot(ds.int$YearBuilt,ds.int[,i], main=names(ds.int[i]), ylab=names(ds.int[i]), col='royalblue')
  hist(ds.int[,i], main=names(ds.int[i]), xlab=names(ds.int[i]), col='royalblue')
  boxplot(ds.int[,i], main=names(ds.int[i]), col='royalblue')
  
  # 파일 이름 설정 및 저장
  file_name <- paste(output_dir, "수치형plot_", i, ".png", sep = "")
  dev.print(png, file = file_name, width = 1000, height = 400)
  dev.off()
}
}

# 5. 범주형데이터 시각화
for(i in 1:ncol(ds.char)){
  barplot(table(ds.char[,i]), main=names(ds.char[i]), las=2, col='royalblue', border=NA)
}

# 6. 결측치 0으로 대치하고 저장
ds[is.na(ds)] <- 0
write.csv(ds, file='C:/Users/po020/Desktop/교육/데이터분석 실무자와 빅데이터 인프라부터 데이터 분석 경험하기/HouseSales/data/1st_train_encoding3.csv')