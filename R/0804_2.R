carprice <- read.csv('carprice.csv', header=T)
head(carprice)
tail(carprice)
str(carprice)

input.type <- dlgInput('Input type')$res
input.city <- dlgInput('Input MPG.city')$res

input.city <- as.numeric(input.city)


result <- subset(carprice, Type == input.type & MPG.city >= input.city)
print(result)
sink('search.txt', append = T)
print(result)
sink()

linear = lm(carprice$Price~carprice$MPG.city, data = carprice)
summary(linear)
plot(linear)
plot(carprice$Price, carprice$MPG.city)
abline(linear, col='red')

predict <- linear$coefficients[[2]]*15+linear$coefficients[[1]]
points(predict,15,col = 'red')
