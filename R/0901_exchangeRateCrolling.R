# 네이버 환율 정보 크롤링
if(!require('dplyr')){
  install.packages("dplyr")
}

if(!require('stringr')){
  install.packages('stringr')
}

library(stringr) # 문자열 처리 라이브러리
library(dplyr) # %>% 문법 사용 라이브러리
library(rvest) # 크롤링 라이브러리


url0 = "https://search.naver.com/search.naver?sm=tab_hty.top&where=nexearch&query="
query = '환율'
# 검색을 위해 퍼센트 인코딩(percent-encoding)함
# 한글이 자꼬 깨져서 영어로 바꿨다가
param = URLencode(iconv(query, to ='UTF-8')) # 환율이라는 한글을 바꿔

url1 = str_c(url0, param) # paste(url0,param,sep='')으로 해도됨
html1 = read_html(url1, encoding='UTF-8') # 한글 꺠지지 않게 해당 사이트 엔코딩 확인하기
html1

Sys.getlocale() # 엔코딩 확인 korean은 에러나기도 함
Sys.setlocale("LC_ALL","English") #그래서 영어로 바깠다


df_table <- html1 %>%
  html_node(".rate_table_info") %>%
  html_table() # table형태로 리턴하겠다.
# <table class = rate_table_info>


df_table # 한글이 깨져있다

# Sys.setlocale() 한글로 되돌리는 작업
Sys.setlocale("LC_ALL","Korean")
df_table