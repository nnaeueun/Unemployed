setwd('C:/Users/po020/Desktop/교육/데이터분석 실무자와 빅데이터 인프라부터 데이터 분석 경험하기')

files = c('HouseSales/data/1st_train_encoding.csv')

# 1. 데이터 가져오기 및 확인
ds <- read.csv(files)
summary(ds)

# 2. NA를 0으로
# 결측치를 제거하기보다 NA인거는 아예 엾음을 뜻해서.
ds[is.na(ds)] <- 0

ds0 <- ds



# 날짜 합치기

# 한 자릿수 MONTH에 0 붙이기 for문 안넣어도 됐을듯
for(i in 1:nrow(ds)){
  if (ds$MoSold[i] < 10){
    ds0$MoSold[i] <- paste(0,ds$MoSold[i], sep='')
  }
}

# Year과 Month합치고 날짜형으로 지정
YMsold <- paste(ds0$YrSold, ds0$MoSold, sep = '')
YMsold <- paste(YMsold,"01",sep='')
head(YMsold)
YMsold <- as.Date(YMsold, format = '%Y%m%d')
class(YMsold)
table(YMsold)

getwd()
output_directory <- 'C:/Users/po020/Desktop/교육/데이터분석 실무자와 빅데이터 인프라부터 데이터 분석 경험하기/HouseSales/plot/02_timeserise'
for(i in 1:ncol(ds0)){
  if(class(ds0[1,i])=='integer'){
    a<-lm(ds0[,i]~YMsold, data=ds0)
    plot(YMsold, ds0[,i], col='royalblue', main = names(ds0[i]), ylab = names(ds0[i]),pch=20, xlab= 'Sold Date')
    abline(a$coefficients[1], a$coefficients[2], col = 'red', lwd=3)
    # 그래프 이미지 저장
    
    filename <- file.path(output_directory, paste("시계열",i,'.png', sep=""))
    png(filename, width=800, height = 300)
    plot(YMsold, ds0[,i], col='royalblue', main = names(ds0[i]), ylab = names(ds0[i]),pch=20, xlab= 'Sold Date')
    abline(a$coefficients[1], a$coefficients[2], col = 'red', lwd=3)
    dev.off()

    cat("그래프",i,"저장완료\n")
  }
}



plot(YMsold, ds0$LotArea, ylim=c(0,17000), pch=20, col = 'royalblue')
abline(14633.6937,-0.2946, col = 'red', lwd = 3)
lm(ds0$LotArea~YMsold)

head(ds0)
SalePrice <- ds0$SalePrice
ds0 <- ds0[-c(1,82,83)]
ds0 <- cbind(ds0, YMsold)
ds0 <- cbind(ds0, SalePrice)
names(ds0)

write.csv(ds0, file='C:/Users/po020/Desktop/교육/데이터분석 실무자와 빅데이터 인프라부터 데이터 분석 경험하기/1st_train_encoding.csv')
