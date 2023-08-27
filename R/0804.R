install.packages('svDialogs')
library(svDialogs)

user.input <- dlgInput('Input income')$res
# 신기하다 인풋을 받는다 ㅋㅋ
user.input
income <- as.numeric(user.input)
tax<-income * 0.05
cat('세금 : ', tax)

getwd()
setwd('C:/Users/kimna/Desktop/교육/R통계')

air <- read.csv('airquality.csv', header=T)
head(air)
class(air)
str(air)

my.iris <- subset(iris, Species == 'setosa')
write.csv(my.iris,'my_iris.csv', row.names = F) #인덱스 없이 쓰겠다.

install.packages('xlsx')
library(xlsx)

air <- read.xlsx('airquality.xlsx', header=T, sheetIndex = 1)
head(air)

my.iris <- subset(iris, Species=='setosa')
write.xlsx(my.iris,'my_iris.xlsx', row.names = F)


air <- read.table('airquality.txt',header=T, sep = ' ')
tail(air[1])

