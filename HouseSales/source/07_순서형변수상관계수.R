setwd('C:/Users/po020/Desktop/교육/데이터분석 실무자와 빅데이터 인프라부터 데이터 분석 경험하기')

files = c('HouseSales/data/1st_ordinalVal.csv')

ds <- read.csv(files)
colnames(ds)


cor(ds$GarageQual[ds$GarageQual>0], ds$GarageCond[ds$GarageCond>0], method = 'spearman')
# 0.547505
cor(ds$GarageQual, ds$GarageCond, method = 'spearman')
# 0.8171323

corrplot(cor(ds, method = 'spearman'), order = 'hclust', addrect=9)


cor(ds$GarageCars[ds$GarageCars>0], ds$GarageCond[ds$GarageCond>0], method = 'spearman')
# 0.1067
cor(ds$GarageCars, ds$GarageCond, method = 'spearman')
# 0.4090627

cor(ds$BsmtCond, ds$BsmtQual, method = 'spearman')
# 0.316948
cor(ds$BsmtCond[ds$BsmtCond>0], ds$BsmtQual[ds$BsmtQual>0], method = 'spearman')
# 0.199183

cor(ds$GarageCond, ds$GarageQual, method = 'spearman')
# 0.8171323
cor(ds$GarageCond[ds$GarageCond>0], ds$GarageQual[ds$GarageQual>0], method = 'spearman')
# 0.547505
corrplot(cor(ds.com, method = 'spearman'), order='hclust', addrect = 7)

ds <- replace(ds, ds==0, NA)


for(i in 1:ncol(ds)){
  cat(names(ds)[i], sum(is.na(ds[,i])), sum(is.na(ds[,i]))/nrow(ds),'\n')
}

# 결측치 없는 데이터 셋 만듦
ds1 <- subset(ds, select=-c(PoolQC, BsmtHalfBath, Fence, KitchenAbvGr)) # 결측치가 80%넘는 컬럼 삭제
head(ds1)
ds.com <- na.omit(ds1)
head(ds.com)
cor(ds.com, method = 'spearman')

library(corrplot)
#corrplot(cor(ds.com ,method = 'spearman'), order='hclust', addrect = 11)
corrplot(cor(ds.com, method = 'spearman'), order='hclust', addrect=4)
par(mfrow=c(1,2))
corrplot(cor(ds, method = 'spearman'), order='hclust', addrect=11)



library(heatmaply)
library(Hmisc)
d.rcorr <- rcorr(as.matrix(ds),type='spearman')
com.rcorr <- rcorr(as.matrix(ds.com), type = 'spearman')
heatmaply_cor(cor(ds,method='spearman'), node_type='scatter', point_size_mat = -log10(d.rcorr$P))
heatmaply_cor(cor(ds.com,method='spearman'), node_type='scatter', point_size_mat = -log10(com.rcorr$P))
