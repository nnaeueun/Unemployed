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

# 2. Ordinal Encoding****

oe <- ds
for(i in 1:ncol(oe)){
  if(oe[1,i] %in% c('Po','Fa','TA','Gd','Ex', NA)){
    oe[,i] <- factor(oe[,i],
                     levels = c('Po','Fa','TA','Gd','Ex'),
                     labels = c(1:5))
    oe[,i] <- as.integer(oe[,i])
  }
}
head(oe)
class(oe$FireplaceQu)
tail(oe$FireplaceQu,10)
unique(oe$PoolQC)
class(oe$KitchenQual)
oe$LotShape <- factor(oe$LotShape,
                      levels = c('IR3','IR2','IR1','Reg'),
                      labels = c(1:4))
oe$LotShape <- as.integer(oe$LotShape)
class(oe$LotShape)
unique(oe$LotShape)

oe$BsmtExposure <- factor(oe$BsmtExposure,
                          levels = c('No','Mn','Av','Gd'),
                          labels = c(1:4))
oe$BsmtExposure <- as.integer(oe$BsmtExposure)
head(oe$BsmtExposure)
unique(oe$BsmtExposure)

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
table(oe$Fence)
str(oe)
write.csv(oe, file='C:/Users/po020/Desktop/교육/데이터분석 실무자와 빅데이터 인프라부터 데이터 분석 경험하기/1st_train_encoding.csv')
