# 분석주제#1 MoSold EDA

setwd('C:/Users/po020/Desktop/교육/데이터분석 실무자와 빅데이터 인프라부터 데이터 분석 경험하기/HouseSales')
  
files = c('data/1st_train_mdf.csv')
ds <- read.csv(files)
colnames(ds)

# 1월부터 12월까지 ds 나누기
{
ds1 <- ds[ds$MoSold == 1, ]
ds2 <- ds[ds$MoSold == 2, ]
ds3 <- ds[ds$MoSold == 3, ]
ds4 <- ds[ds$MoSold == 4, ]
ds5 <- ds[ds$MoSold == 5, ]
ds6 <- ds[ds$MoSold == 6, ]
ds7 <- ds[ds$MoSold == 7, ]
ds8 <- ds[ds$MoSold == 8, ]
ds9 <- ds[ds$MoSold == 9, ]
ds10 <- ds[ds$MoSold == 10, ]
ds11 <- ds[ds$MoSold == 11, ]
ds12 <- ds[ds$MoSold == 12, ]
}
par(mfrow=c(1,2))
barplot(table(ds2$MSSubClass))
barplot(table(ds6$MSSubClass))
# 수치형과 문자형 나누기
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
  str(ds.int) # 1460,38
  str(ds.char) # 1460, 43
}

for(i in 1:ncol(ds.char)){
  barplot(table(ds$MoSold, ds.char[,i]), main = names(ds[i]))
}

barplot(table(ds2$SaleCondition))
barplot(table(ds6$SaleCondition))

ds6 <- ds6[sample(nrow(ds6),50),]
ds2 <- ds2[sample(nrow(ds2),50),]

barplot(table(ds2$Neighborhood), las = 2)
barplot(table(ds6$Neighborhood), las = 2)

ds26 <- ds[ds$MoSold==2 | ds$MoSold==6,]


barplot(table(ds26$MoSold, ds26$OverallCond))


month_ratio <- nrow(ds[ds$MoSold==2,])/nrow(ds) # 6월 0.17328767   2월 0.03561644
# MoSold 변수의 월별 빈도수 계산
month_counts <- table(ds$MoSold)
# 월별 비율 계산
month_ratios <- month_counts / sum(month_counts)
# 바 차트 그리기
barplot(month_ratios, 
        names.arg = month.abb,  # 월 이름 (예: "Jan", "Feb", ...)
        main = "MoSold 월별 비율",  # 차트 제목
        xlab = "월",  # x축 레이블
        ylab = "비율",  # y축 레이블
        col = "blue",  # 바 색상
        ylim = c(0, 0.2)  # y축 범위 설정
)

colnames(ds26)
{
  ds.int <- NULL
  col.int <- NULL
  ds.char <- NULL
  col.char <- NULL
  for (i in 1:ncol(ds26)){
    if(class(ds26[,i])=='integer'){
      ds.int <- cbind(ds.int,ds26[,i])
      col.int <- cbind(col.int, names(ds26[i]))
    }
    else if(class(ds26[,i])=='character'){
      ds.char <- cbind(ds.char,ds26[,i])
      col.char <- cbind(col.char, names(ds26[i]))
    }
  }
  colnames(ds.int) <- col.int
  colnames(ds.char) <- col.char
  ds.int <- as.data.frame(ds.int)
  ds.char <- as.data.frame(ds.char)
  str(ds.int) # 1460,38
  str(ds.char) # 1460, 43
}


par(mfrow=c(1,2))
barplot(table(ds$Neighborhood[ds$MoSold==2]))
barplot(table(ds$Neighborhood[ds$MoSold==6]))

barplot(table(ds$HouseStyle[ds$MoSold==2]))
barplot(table(ds$HouseStyle[ds$MoSold==6]))

barplot(table(ds2$GarageQual))
barplot(table(ds6$GarageQual))


barplot(table(ds$MoSold, ds$MSZoning))

head(ds6)
head(ds2)

par(mfrow=c(1,2))
for(i in 1:ncol(ds2)){
  barplot(table(ds2[,i]), main=paste(names(ds2[i]),'2월'), col ='royalblue', border=NA, las=2)
  barplot(table(ds6[,i]), main=paste(names(ds6[i]),'6월'), col ='royalblue', border=NA, las=2)
}


barplot(table(ds$MoSold, ds))