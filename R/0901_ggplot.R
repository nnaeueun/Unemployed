# ggplot2
# 하나의 ggplot()함수에 여러 geom_xx()로 연결

library(ggplot2)

# 월과 강우량 데이터로 막대그래프 작성

month <- c(1,2,3,4,5,6)
rain <- c(55,50,45,50,60,70)

df <- data.frame(month, rain)
df

# 막대그래프 geom_bar
ggplot(df, aes(x=month, y=rain)) + # 데이터프레임 지정
  geom_bar(stat='identity', width = 0.7, fill='steelblue') # 그래프 형태


# ggplot로 히스토그램 geom_histogram()
# 품종에 따라 꽃잎 길이 히스토그램 작성
ggplot(iris, aes(x=Sepal.Width, fill = Species, color = Species))+
  geom_histogram(binwidth = 0.5, position='dodge') +
  theme(legend.position = 'top')


# 산점도 geom_point()
ggplot(data=iris, aes(x=Petal.Length, y=Petal.Width)) +
  geom_point()

# 이쁘게
ggplot(data=iris, aes(x=Petal.Length, y=Petal.Width, color=Species)) +
  geom_point(size=3) +
  ggtitle('꽃잎의 길이와 폭') +
  theme(legend.position = 'top', plot.title = element_text(size=25, face='bold', colour = 'steelblue'))


# ggplot 상자그림 geom_boxplot()
ggplot(iris, aes(y=Petal.Length))+
  geom_boxplot(fill='yellow')

# 품종별 boxplot()
ggplot(iris, aes(x=Species,y=Petal.Length,fill=Species))+
  geom_boxplot() +
  ggtitle('품종별 꽃잎 길이') +
  theme(legend.position = 'top', plot.title = element_text(size=25, face='bold', colour = 'steelblue'))

# 선그래프 geom_line()
year <- 1937:1960
cnt <- as.vector(airmiles)
df <- data.frame(year, cnt)
head(df)

ggplot(df, aes(x=year, y=cnt)) +
  geom_line(col='red')+
  ggtitle('연도별 승객들 이동 거리')
