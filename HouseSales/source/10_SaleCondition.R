setwd('C:/Users/po020/Desktop/교육/데이터분석 실무자와 빅데이터 인프라부터 데이터 분석 경험하기/HouseSales')

files = c('data/1st_train_mdf.csv')
ds <- read.csv(files)
colnames(ds)
ds$SaleCondition[which(ds$SaleCondition =='Abnorml')] <- 'Abnormal'
ds$SaleCondition[which(ds$SaleCondition !='Abnormal')] <- 'Not Abnormal'
sum(ds$SaleCondition =='Abnorml')
for (i in 1:ncol(ds)){
  barplot(table(ds$SaleCondition, ds[,i]), col=c('hotpink', 'royalblue'), legend = TRUE, border=NA,
          main = names(ds[i]))
}


files = c('data/1st_train_encoding.csv')
ds <- read.csv(files)
colnames(ds)

ds <- ds[order(ds$YMsold),]
ds$YMsold <- as.Date(ds$YMsold)

plot(ds$SalePrice,ds$YMsold, type = 'o')
plot(ds$YMsold, ds$SalePrice, type = "l", xlab = "날짜", ylab = "판매 가격",
     main = "SalePrice 시계열 그래프")

mean(ds$SalePrice[which(ds$YMsold=='2006-01-01')])
mean(ds$SalePrice[which(ds$YMsold=='2006-02-01')])
tail(ds$SalePrice)




# 필요한 패키지 불러오기
library(lubridate)

# 시작 날짜와 종료 날짜 설정
start_date <- as.Date("2006-01-01")
end_date <- as.Date("2010-07-01")

# 결과를 저장할 데이터프레임 생성
result_df <- data.frame(Date = as.Date(character(0)), AveragePrice = numeric(0))

# 날짜 범위 내에서 한 달씩 증가하며 평균 계산 및 데이터프레임에 추가
date <- start_date
while (date <= end_date) {
  next_date <- date %m+% months(1)  # 한 달 증가
  subset_data <- ds$SalePrice[ds$YMsold >= date & ds$YMsold < next_date]
  average_price <- mean(subset_data)
  result_df <- rbind(result_df, data.frame(Date = date, AveragePrice = average_price))
  date <- next_date  # 다음 달로 이동
}

# 결과 데이터프레임 출력
print(result_df)

# 그래프 그리기
plot(result_df$Date, result_df$AveragePrice, type = 'l',xlab = "", ylab = "Mean(SalePrice)", xaxt = "n", lwd=3, col='royalblue')

# x축의 눈금을 더 자세히 표현하기
axis.Date(1, at = seq(min(result_df$Date), max(result_df$Date), by = "3 month"), format = "%Y-%m", las = 1)






######
files = c('data/1st_train_encoding3.csv')
ds <- read.csv(files)
colnames(ds)
ds <- ds[,-c(1,2)]


# 변수 타입별로 나누어보기
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

# cor
cor(ds.int)
library(corrplot)
corrplot(cor(ds.int, method = 'spearman'))
ds.int <- ds.int[order(ds$YMsold),]

model1 <- lm(ds.int$SalePrice~., data = ds.int)
summary(model1)

plot(ds.int$SalePrice, type='l')
points(model1$fitted.values, col = 'red', type = 'l')


for(i in 1:ncol(ds.int)){
  plot(ds.int[,i], ds.int$SalePrice, xlab = names(ds.int[i]), ylab = 'SalePrice', pch = 16, col = 'royalblue')
}


cor(ds.int$SalePrice, ds.int$OverallQual) #0.7909816
cor(ds.int$SalePrice, ds.int$OverallCond) # -0.07785589
cor(ds.int$SalePrice, ds.int$BsmtQual) # 5852
cor(ds.int$SalePrice, ds.int$GarageQual) # 0.2738
cor(ds.int$SalePrice, ds.int$ExterQual) # 0.6826392
