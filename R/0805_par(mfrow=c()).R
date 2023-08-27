par(mfrow=c(2,2), mar=c(3,3,4,2))
# mar은 마진

hist(iris$Sepal.Length, main='쎼팔렝스', col='orange')

barplot(table(mtcars$cyl), main='mtcars', col=c('red','green','blue'))

barplot(table(mtcars$gear), main='mtcars', col=rainbow(3), horiz=TRUE)

pie(table(mtcars$cyl), main = 'mtcars', col= topo.colors(3), radius=2)

par(mfrow=c(1,1), mar=c(5,4,4,2)+.1)
