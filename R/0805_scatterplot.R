# 산점도 그래프
wt <- mtcars$wt #중량 데이터
mpg <- mtcars$mpg # 연비데이터
plot(wt, mpg, main= '중량과 연비', xlab = '중량', ylab='연비', col = 'red', pch=19)


iris.2 <- iris[,3:4] # 페탈렝스 윗스
head(iris.2)
levels(iris$Species)

group <- as.numeric(iris$Species) #종을 숫자형으로 변환

color <- c('red', 'green','blue')

plot(iris.2, main='iris', pch=c(group), col= color[group]) # 점의 모양을 그룹별로 하겠다 , 점 색상도 그룹별 1,2,3하겠다는 말
