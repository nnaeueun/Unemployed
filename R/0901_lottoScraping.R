# 로또 번호 생성 프로그램

1:45 # 1부터 45까지 생성

lotto <- sample(1:45, 6) # 6개 뽑기
lotto

sort(lotto) # 정렬
sum(lotto) # 합계

# <프로그램 요구 사항>
# 1. 1~45 까지 6개 뽑기
# 2. 정렬
# 3. 번호 합이 100부터 170인것만 사용 정규분포 상 이렇게 되어야 확률 높아져서 ㅋ
# 4. 5게임

i <- 1
myres <- matrix(0, nrow=5, ncol=6) # 내가 고른 답 나오게게
while(i <= 5){
  mylotto = sort(sample(1:45,6))
  if(sum(mylotto) >= 100 & sum(mylotto) <= 170){
    cat(mylotto, '합=', sum(mylotto),'\n')
    myres[i,] <- mylotto
    i = i+1
  }
}
myres



# 로또 사이트 스크래핑

# rvest가 R에서 유명한 스크래핑 패키지임
if(!require(rvest)){
  install.packages("rvest")
} # 라이브러리가 설치 안되어있을 때만 설치된다.

library(rvest)

url0 <- "https://dhlottery.co.kr/gameResult.do?method=byWin"
html0 <- read_html(url0, encoding="EUC-KR") # 현재 사이트 엔코딩이 이거라서
html0

# 1. 회차 출력
elem1 <- html_node(html0, ".win_result > h4 > strong")
# <div class='win_result'> -> <h4> -> <strong> 에 있는 회차 정보
elem1
nth <- html_text(elem1) # html태그에 있는 text를 가져오는 함수
cat(nth,'\n') # 그 글자를 가져와서 로또 회차를 출력함

# 2. 당첨번호 출력
spans <- html_nodes(html0, '.num.win > p > span')
# <div class='num win"> -> <p> -> <span> 에 있는 당첨 번호
lotto <- html_text(spans)
cat('당첨 번호 = ',lotto,'\n')

# 3. 보너스번호 출력
elem2 <- html_node(html0,'.num.bonus > p > span')
# <div class='num bonus'> -> <p> -> <span>
bonus <- html_text(elem2)
cat('보너스 번호 = ', bonus,'\n')




## 이제 당첨 번호와 랜덤 샘플링 비교해보기
res <- c(lotto, bonus)
ans <- matrix(0, nrow=5, ncol=6)
for(i in 1:5){
  ans[i,] <- myres[i,]%in%res
  cat(nth, '맞힌 개수 ', sum(ans[i,]),'\n')
}
ans