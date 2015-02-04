#load dataset
citytemp <- read.csv("citytemp.csv")
head(citytemp)
# reshape the data
library(reshape2)
citytemp.long<-melt(citytemp,id.vars="CITY",measure.vars=c("JAN","JULY"),variable.name="MONTH",value.name="TEMP")

#explore the data
#temp mean and sd 
mean<-sapply(citytemp[,-1],mean)
sd<-sapply(citytemp[,-1],sd)
rbind(mean,sd)
#temp in JAN
plot(citytemp$JAN,main="Temprature in JAN",ylab="temp",las=1)
abline(h=mean(citytemp$JAN),col="blue")
identify(citytemp$JAN,labels=citytemp$CITY,cex=0.6,col="blue")
#temp in JULY
plot(citytemp$JULY,main="Temprature in JULY",ylab="temp",las=1)
abline(h=mean(citytemp$JULY),col="blue")
identify(citytemp$JULY,labels=citytemp$CITY,cex=0.6,col="red")

# Jan vs July
plot(citytemp$JAN,citytemp$JULY,main="Temprature JAN vs JULY",las=1)
abline(lm(JULY~JAN,citytemp))
cor(citytemp$JAN,citytemp$JULY)

###cluster analysis###
## hierarchical cluster analysis
mycitytemp<-citytemp[-1]
dis<-dist(mycitytemp,method='euclidean')
dis.matrix=as.matrix(dis)
fit_ward<-hclust(dis,method="ward")
##plot the dendrogram cluster with city names at the bottom
plot(fit_ward, labels=citytemp$CITY, ylab='distance',cex=0.7)
#cut the tree into 4 clusters and draw dendrogram with red borders around the clusters
groups<-cutree(fit_ward,k=4)
rect.hclust(fit_ward, k=4, border="red")

##analyze the result
round(aggregate(citytemp[,2:3], by=list(groups),FUN=mean),3)
GROUP<-as.factor(groups)
clusters<-cbind(citytemp,GROUP)
table(clusters$GROUP)

## k-means cluster analysis
### k-means cluster 
##Determine number of clusters
library(cluster)
wss <- (nrow(mycitytemp)-1)*sum(apply(mycitytemp,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mycitytemp,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
##perform kmeans cluster
fit.k<-kmeans(mycitytemp,centers=4)
fit.k$size
clusplot(mycitytemp, fit.k$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)
round(aggregate(mycitytemp,by=list(fit.k$cluster),FUN=mean),3)
