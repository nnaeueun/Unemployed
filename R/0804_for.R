a <- 1:5
for(i in a){
  print(i)
}

for(i in 1:9){
  cat('2*',i,'=',i*2, '\n', sep='')
}

# 짝수 출력
for(i in 1:20){
  if(i %% 2==0){
    print(i)
  }
}


# 1부터 100까지 합
sum <- 0
for(i in 1:100){
  sum <- sum + i
}
sum
