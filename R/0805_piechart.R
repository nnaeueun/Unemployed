# 원그래프

favorite

ds <- table(favorite)
ds
pie(ds, main='선호계절',col=c('beige','yellow','green','pink'), radius=1)


# 3차원 원그래프
install.packages('plotrix')
library(plotrix)

pie3D(ds, main = '선호계절', labels=names(ds), labelcex=1.0, explode=0.1, radius=1.5, col = c('beige','yellow','green','pink'))
