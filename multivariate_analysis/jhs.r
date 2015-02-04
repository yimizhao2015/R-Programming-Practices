# load the data
jhs <- read.csv("jhs.csv")
jhs$AGE<-as.factor(jhs$AGE)
jhs$SEX<-as.factor(jhs$SEX)


# explore the data
# plot WEIGHT vs HEIGHT
plot(jhs$WEIGHT~jhs$HEIGHT,main="WEIGHT vs HEIGHT",col=jhs$SEX)
cor(jhs$WEIGHT,jhs$HEIGHT)
abline(lm(WEIGHT~HEIGHT,jhs),col="blue")
text(locator(1),"R=0.88")


# mean values over AGE & SEX
round(tapply(jhs$HEIGHT,jhs$SEX,mean),2)
round(tapply(jhs$WEIGHT,jhs$SEX,mean),2)
round(tapply(jhs$HEIGHT,jhs$AGE,mean),2)
round(tapply(jhs$WEIGHT,jhs$AGE,mean),2)
library(gplots)
par(mfrow=c(2,2))
plotmeans(HEIGHT~SEX,jhs,las=1)
title("HEIGHT Mean over SEX")
plotmeans(WEIGHT~SEX,jhs,las=1)
title("WEIGHT Mean over SEX")
plotmeans(HEIGHT~AGE,jhs,las=1)
title("HEIGHT Mean over AGE")
plotmeans(WEIGHT~AGE,jhs,las=1)
title("WEIGHT Mean over AGE")

# or this way
interaction.plot(jhs$AGE,jhs$SEX,jhs$HEIGHT,col=c(1,2))
interaction.plot(jhs$AGE,jhs$SEX,jhs$WEIGHT,col=c(3,4))
#MANOVA
jhs.manova<-manova(cbind(WEIGHT,HEIGHT)~AGE*SEX,jhs)
#model summary using test “Pillai”, “Wilks”, “Hotelling-Lawley”, “Roy”
summary(jhs.manova,test="Pillai")
summary(jhs.manova,test="Wilks")
summary(jhs.manova,test="Hotelling-Lawley")
summary(jhs.manova,test="Roy")

# adjusted manova model
age.manova<-manova(cbind(WEIGHT,HEIGHT)~AGE,jhs)
summary(age.manova,test="Wilks")
#check which dimensions differ
summary.aov(age.manova)

#pairwise comparison 
for(i in 11:15){
    for(j in i:16){
        if(i!=j){
            if(!(i==11&j==16)){
             model.s<-summary(manova(cbind(WEIGHT,HEIGHT)~AGE,jhs,subset=jhs$AGE %in%c(i,j)))
             p<-model.s$stats[[11]]
             if(p<0.05){
                 print(cbind(i,j,p))
             }
            }       
        }
    }
}
# age group 12 vs 14
age.manova.dif<-manova(cbind(WEIGHT,HEIGHT)~AGE,jhs,subset=jhs$AGE %in%c("12","14"))
summary(age.manova.dif)
age.manova.dif
