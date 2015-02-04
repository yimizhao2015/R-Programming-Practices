#load the data
library(xlsx)
longley<-read.xlsx("longley.xls",1)
YEAR<-as.factor(longley$YEAR)
longley2<-cbind(longley[,-7],YEAR)

##data explore##
#scatter plots
par(mfrow=c(2,3))
plot(longley$YEAR,longley$TOTEMPL,type="b",col=1)
plot(longley$YEAR,longley$INFLAT,type="b",col=2)
plot(longley$YEAR,longley$GNP,type="b",col=3)
plot(longley$YEAR,longley$UNEMPL,type="b",col=4)
plot(longley$YEAR,longley$ARMYEMPL,type="b",col=5)
plot(longley$YEAR,longley$POPGT14,type="b",col=6)

# correlation plot
library(GGally)
ggpairs(longley[,-7],cex=0.8)
pairs(TOTEMPL~.,data=longley)

## PCA##
#conduct pca
longley.pca<- prcomp(longley[,-7],scale=T)
summary(longley.pca)
round(longley.pca$rotation,3)
#plot of variance of each PCA
par(mfrow=c(1,2))
screeplot(longley.pca, type="lines",col="blue",main="variance of pca")
screeplot(longley.pca, type="barplot",col="blue",main="variance of pca")

### model fitting###
#multiple linear regression#
md1<-lm(TOTEMPL~.,data=longley)
summary(md1)
# check outlier and influential points
par(mfrow=c(2,2))
plot(md1)
summary(update(md1,.~.-INFLAT))
summary(update(md1,.~.-INFLAT-POPGT14))
longley.lm<-lm(TOTEMPL~ GNP + UNEMPL + ARMYEMPL + YEAR, data = longley)

library(leaps)
summary(regsubsets(y=longley$TOTEMPL, x = longley[, -1], method="forward"))
summary(regsubsets(y=longley$TOTEMPL, x = longley[, -1], method="backward"))

all<- regsubsets(TOTEMPL~., data=longley)
rs<-summary(all)
par(mfrow=c(1,2))
plot(2:7,rs$adjr2, xlab="No.of Parameters", ylab="Adjusted R-square")
plot(2:7,rs$cp, xlab="No. of Parameters", ylab="Cp Statistics")
abline(0,1)

#outliers and influential points
h<-lm.influence(md1)$hat
names(h)<-longley$YEAR
rev(sort(h))
# exclude 1962, 1951 and refit the model
all2<-regsubsets(TOTEMPL~., data=longley,subset=(longley$YEAR!="1962" & longley$YEAR!="1951"))
rs2<-summary(all2)
rs2$which[which.max(rs2$adjr2),]
## best model fit
longley.best<-lm(TOTEMPL~GNP+UNEMPL+ARMYEMPL+YEAR,data=longley,subset=(longley$YEAR!="1962" & longley$YEAR!="1951"))
summary(longley.best)
# remove POPGT14
summary(update(longley.best,.~.-POPGT14-GNP))
