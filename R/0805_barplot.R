favorite <- c('WINTER','SUMMER','SPRING','SUMMER','SUMMER','FALL','FALL','SUMMER','SPRING','SPRING')
favorite
table(favorite) # 도수분포계산

ds <- table(favorite)
ds
barplot(ds, main='favorite season', col='wheat')

colors() # 무슨 색 있는지 볼 수 있음

barplot(ds, main='favorite season', col=c('wheat','violetred2','tan2','sienna2'))

barplot(ds, main='favorite season', col=rainbow(4))

barplot(ds, main = '선호계절', col='sienna1', xlab='계절',ylab='빈도수')
barplot(ds, main = '선호계절', col='sienna1', xlab='계절',ylab='빈도수')

barplot(ds, main = '선호계절', col='tomato2', xlab='계절',ylab='빈도수', horiz=TRUE)

barplot(ds, main = '선호계절', col='tomato2', names = c('가을','봄','여름', '겨울'))

barplot(ds, main = '선호계절', col='tomato2', las=2) # las x축 수직방향으로

