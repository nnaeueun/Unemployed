a <- 10
b <- 20

if(a>5 & b>5){
  print(a+b)
}

if(a>5 || b>30){
  print(a*b)
}


if(a>b){
  c <- a
}else{
  c <- b
}
print(c)

c <- ifelse(a>b, a, b)
print(c)
