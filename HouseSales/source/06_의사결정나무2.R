setwd('C:/Users/po020/Desktop/교육/데이터분석 실무자와 빅데이터 인프라부터 데이터 분석 경험하기/HouseSales')

files = c('data/1st_train_encoding3.csv')
ds <- read.csv(files)
colnames(ds)
ds <- ds[,-c(1,2)]
colnames(ds)

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

for (i in 1:ncol(ds.char)){
  ds.char[,i] <- as.factor(ds.char[,i])
}
ds.char$YMsold <- as.Date(ds.char$YMsold)

ds.int$MSSubClass <- as.factor(ds.int$MSSubClass)

df <- cbind(ds.int, ds.char)

summary(ds$MoSold)
# col = c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightcyan")
plot(MoSold, df$SaleCondition, 
     col= c("red3", "yellow3", "lightgreen", "lightcyan",'lightblue','lightpink'),
     xlab='MoSold', ylab = 'SaleCondition')
legend("topright", legend = levels(df$SaleCondition), fill = c("red3", "yellow3", "lightgreen", "lightcyan",'lightblue','lightpink'), title = "SaleCondition")
levels(df$SaleCondition)

MoSold <- as.factor(ds$MoSold)
ds.int$OverallQual