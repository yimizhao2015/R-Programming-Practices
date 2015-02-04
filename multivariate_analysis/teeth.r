#load the data
teeth <- read.csv("teeth.csv")
teeth_top<-teeth[,c(1,2,4,6,8)]
teeth_bottom<-teeth[,c(1,3,5,7,9)]
#explore the data
summary(teeth[,-1])
library(GGally)
plot(teeth[,-1],main="teeth data",pch=18,cex=0.7)
ggpairs(teeth[,-1],cex=0.8)

### PCA###
#conduct pca
teeth.pca<- prcomp(teeth[,-1],scale=T)
summary(teeth.pca)
round(teeth.pca$rotation,3)
#plot of variance of each PCA
par(mfrow=c(1,2))
screeplot(teeth.pca, type="lines",col="blue",main="variance of pca")
screeplot(teeth.pca, type="barplot",col="blue",main="variance of pca")

###cluster analysis###
## hierarchical cluster analysis
teeth2<-teeth[-1]
dis<-dist(teeth2,method='euclidean')
dis.matrix=as.matrix(dis)
teeth_ward<-hclust(dis,method="ward")
##plot the dendrogram cluster with city names at the bottom
plot(teeth_ward, labels=teeth$ANIMAL, ylab='distance',cex=0.7)
#cut the tree into 4 clusters and draw dendrogram with red borders around the clusters
groups<-cutree(teeth_ward,k=4)
rect.hclust(teeth_ward, k=4, border="red")
##analyze the result
round(aggregate(teeth[,2:9], by=list(groups),FUN=mean),3)
GROUP<-as.factor(groups)
clusters<-cbind(teeth,GROUP)
table(clusters$GROUP)

##perform kmeans cluster##
teeth.k<-kmeans(teeth2,centers=4)
teeth.k$size
round(aggregate(teeth2,by=list(teeth.k$cluster),FUN=mean),2)
clusplot(teeth2, teeth.k$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)
