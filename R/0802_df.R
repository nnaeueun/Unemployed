# 데이터프레임

head(iris)
head(iris[,c("Sepal.Length","Sepal.Width")])
head(iris$Sepal.Length)
iris[1:5,]
iris[1:5,c(1,3)]


city <- c("Seoul","Tokyo","NewYork")
rank <- c(1,3,2)
city.info <- data.frame(city, rank)
View(city.info)

burger <- as.data.frame(nut)
menu <- c('새우','불고기','치킨')
burger <- cbind.data.frame(burger, menu)
View(burger)

burger[c('B','L'),'menu']
burger[c('M','B'),'나트륨']
burger$열량


## 데이터셋 기본 정보 아는 함수
dim(iris) # 디멘전 행열갯수 차원원
nrow(iris)
ncol(iris)
colnames(iris)
head(iris)
tail(iris, n=10)
?tail


##
str(iris) #데이터셋 요약 정보보기
iris[,5]
levels(iris[,5]) #중복제거한 품종 종류
table(iris[,"Species"]) # 품종 종류별 갯수

colSums(iris[,-5])
colMeans(iris[,-5])
rowSums(iris[,-5])


z <- matrix(1:20, nrow = 4, ncol = 5)
z
t(z) # 행렬 변환 TRANSPOSE..

# 조건에 맞는 행과 열의 값 추출하기
IR.1 <- subset(iris, Species=='setosa')
IR.1

IR.2 <- subset(iris, Sepal.Length>5.0 & Sepal.Width>4.0)
IR.2
IR.2[,c(2,4)]


a <- matrix(1:20, 4,5)
b <- matrix(21:40, 4,5)
2*a
a+b
a <- 3*a
b <- b-4


class(iris)
class(state.x77)
as.numeric(is.matrix(iris)) # FALSE는 0
as.numeric(is.data.frame(iris)) # TRUE는 1

is.matrix(state.x77)
st <- data.frame(state.x77)
head(st)
class(st)
iris.m <- as.matrix(iris[,1:4])
head(iris.m)
class(iris.m)


class(iris[,"Species"])
class(iris[,4])
iris["Species"]
iris[5]
class(as.matrix(iris$Species))


## 벚나무 판매하기
class(trees)
str(trees)

girth.mean <- mean(trees$Girth)
candidate <- subset(trees, Girth > girth.mean & Height > 80 & Volume > 50)
nrow(candidate)
