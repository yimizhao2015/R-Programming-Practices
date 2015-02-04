###load the dataset and prepare data for analysis
test<- read.csv("~/Desktop/Study/Hunter study/multivariate data analysis/Final papers/test/test.csv", header=T)
test.hs<-test[1:3]
test.gpa<-test[-c(1:3)]
###explore the data
round(cor(test),2)
pairs(scale(test))
library(psych)
pairs.panels(scale(test),pch=21)
### pca
test.pca<- princomp(test,cor=T)
summary(test.pca)
round(test.pca$rotation,3)
screeplot(test.pca,type="lines",col="blue",main="variance of pca")
library(FactoMineR)
test.pca2<-PCA(test)
### factor analysis
library(psych)
corMat<-cor(test)
test.fa<-fa(r=corMat,nfactors=3,rotate="varimax",fm="pa")
#proportion of variation of each variable explained by the three factors
round(test.fa$communality,3)
# overall model fit(percentage of variation explained in our model)
sum(test.fa$communality)/8 # 8 is the number of parameters in our model

