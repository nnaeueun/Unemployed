# 매트릭스 활용

z <- matrix(1:20, nrow=4, ncol=5)
z # 세로로 숫자 나열뎀

z2 <- matrix(1:20, nrow=4, ncol=5, byrow = T) # byrow인자 있으니까 가로로 나열뎀
z2

z[2,]
z[,4]
z[1,4]

z[2, 1:3]
z[1, c(1,2,4)]
z[1:2, ]
z[,c(1,4)]


# 매트릭스 행과 열에 이름 붙이기
score <- matrix(c(90,85,69,78,85,96,49,95,90,80,70,60), nrow=4)
score
rownames(score) <- c('John','Tom','Mark','Jane')
colnames(score) <- c('English','Math','Science')
score

score['John',]
score[,'English']
score['Tom',c('Math','Science')]
rownames(score)
colnames(score)
colnames(score)[2]


# 퀴즈 햄버거 영양성분정보 제공
nut <- matrix(c(514,533,566,917,853,888,11,13,10), nrow=3)
nut
rownames(nut) <- c('M','L','B')
colnames(nut) <- c('열량','나트륨','포화지방')

nut['M','나트륨']
nut['M',]
nut[,'열량']
