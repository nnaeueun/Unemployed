setwd('C:/Users/po020/Desktop/교육/데이터분석 실무자와 빅데이터 인프라부터 데이터 분석 경험하기')

files = c('HouseSales/data/1st_ordinalVal.csv')

# 1. 데이터 가져오기 및 확인
ds <- read.csv(files)
summary(ds)

distance(colMeans(ds),method = 'euclidean')
getDistMethods()
str(mahalanobis(ds, colMeans(ds), cov(ds))) 1:1460
str


cov(colMeans(ds))

# 사분위 수??

cov(ds)
