# 상자그림
dist <- cars[,2]
boxplot(dist, main='자동차 제동거리')

boxplot.stats(dist)

boxplot(Petal.Length~Species, data= iris, main = '품종별 꽃잎 길이', col = c('green','yellow','blue'))
