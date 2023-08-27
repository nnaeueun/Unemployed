# 히스토그램은 수치형 데이터 분포

head(cars)
dist <- cars[,2]
hist(dist, main = '제동거리분포', xlab = '제동거리', ylab='빈도수', border='blue', col='lightgreen', las=2, breaks=5) #breaks막대개수


result <- hist(dist, main='제동거리분포', breaks=5, col='gray')

result
freq <- result$counts
names(freq) <- result$breaks[-1]
#히스토그램에서 빈도수 반환하는 코드

##

install.packages('Stat2Data')
library(Stat2Data)

data(Diamonds)
ds <- Diamonds$PricePerCt
hist(ds, main='캐럿당 가격분포', breaks=9, col='whitesmoke', las=2)
color
 <- rep('#a8dadc',9)
color[3] <- '#1d3557'
hist(ds, main='캐럿당 가격분포', breaks=9, col=color, las=2)
