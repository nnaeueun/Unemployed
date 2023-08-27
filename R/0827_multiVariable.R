# 다중변수 데이터 분석


# pressure데이터셋을 통한 온도와 기압의 관련성 분석
# 1. 데이터 확인 str, class, head
pressure
str(pressure)

# 2. 산점도 작성 plot(x축, y축)
plot(pressure$temperature, pressure$pressure,
     main = '온도와 기압',
     xlab = '화씨',
     ylab = '기압',
     type = 'o',
     col = 'green')

# 3. 상관계수 분석 cor()
cor(pressure$temperature, pressure$pressure)
# 0.7577923 양의 상관관계


# car 데이터셋을 이용한 산점도와 상관계수 계산
# 1. 데이터확인
head(cars)
# 2. 산점도 작성
plot(cars$speed, cars$dist,
     main = '속도와 제동거리',
     xlab='속도',
     ylab='제동거리',
     col='blue')

# 3. 상관계수
cor(cars$speed, cars$dist)
# 0.8068949 속도와 제동거리는 양의상관관계


# 다중변수 사이의 상관관계

# 미국의 어떤 주 살인율
# 1. 데이터 확인 str, class, head
str(state.x77)
class(state.x77) # 매트릭스 구조임
st <- data.frame(state.x77) # 데이터프레임으로 형변환
View(st)
class(st)
head(st)

# 2. 다중산점도작성
plot(st)

# 3. 다중상관계수
cor(st)
# 기대수명과 살인율은 -0.7808458 로 강한 음의 상관관계
# 인과관계 여부는 심층적인 연구를 해야 알 수 있다
