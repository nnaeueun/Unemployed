setwd('C:/Users/po020/Desktop/교육/데이터분석 실무자와 빅데이터 인프라부터 데이터 분석 경험하기')

files = c('HouseSales/data/1st_ordinalVal.csv')

# 1. 데이터 가져오기 및 확인
ds <- read.csv(files)
summary(ds)



cov(colMeans(ds))

# 사분위 수??

cov(ds)

cor(ds, method='spearman')

library(corrplot)
library(heatmaply)
library(Hmisc)
corrplot(cor(ds,method = 'spearman'), order='hclust', addrect = 11)

d.rcorr <- rcorr(as.matrix(ds),type='spearman')
heatmaply_cor(cor(ds,method='spearman'), node_type='scatter', point_size_mat = -log10(d.rcorr$P))
# pca
heatmaply_cor(cor(ds,method='spearman'))
ds.pca <- prcomp(ds, scale=F)
summary(ds.pca)
biplot(ds.pca, main = 'PCA')
ds.pca$center
ds.cor <- cor(ds, method = 'spearman')
hclust(ds.cor, method = 'complete')
ds.cor.pca <- prcomp(ds.cor, scale=T)
biplot(ds.pca, main = 'cor PCA', xlabs = rep("*", nrow(ds.pca)))

biplot(ds.pca,
       col = c('darkblue', 'red'),
       scale = 0, xlabs = rep("*", 1460))
plot(ds.pca$center)
ds.pca$center


for(i in 1:ncol(ds)){
  barplot(table(ds[,i]), main=names(ds[i]))
}
