#"Factors Affecting Wage:A General Linear Model" by Yimi Zhao, Dec.2014
#load the dataset
library(AER)
data(CPS1985)

# take a look at the data
head(CPS1985)
str(CPS1985)

##data exploration##
#data summary 
summary(CPS1985)
# plot wage vs each other variable
par(mfrow=c(2,5))
plot(wage~., CPS1985)
# correlation plot of numeric variables
library(psych)
pairs.panels(scale(CPS1985[,1:4]),pch=21)
#or use default pair plot
pairs(CPS1985[,1:4],panel=panel.smooth)

### model fit###
##(1)full model##
full0<-lm(wage~.,data=CPS1985)
summary(full0)
# remove age(collinearity)
full<-lm(wage~.-age,CPS1985)
summary(full)
# regression diagnostics
par(mfrow=c(2,2))
plot(full)
# reslut(1) of diagnostics
summary(lm(abs(residuals(full))~fitted(full)))#check constant variance
# reslut(2) of diagnostics
CPS1985[171,]#outlier case 171
# reslut(3) of diagnostics: log(wage)
hist(CPS1985$wage)# wage is high right skewed or
plot(density(CPS1985$wage),main="wage plot")
# result(4)cooks distance(influential observation)
libary(faraway)
halfnorm(cooks.distance(full),main="cooks distance")# case 171,410

## refit model-md1##
md1<-lm(log(wage)~.-age,CPS1985,subset=(rownames(CPS1985)!="170"))
summary(md1)
# new diagnostics
plot(md1)
summary(lm(abs(residuals(md1))~fitted(md1)))#check constant variance
shapiro.test(residuals(md1))
halfnorm(cooks.distance(md1),main="cooks distance")

## refit model-md2##
data<- CPS1985[-c(171,200),]
md2<-lm(log(wage)~.-age,data)
summary(md2)

### variable selection
##(1)backward elimination
summary(update(md2,.~.-ethnicity))
##(2)AIC Criterion
step(md2)
###final model###
md.final<-lm(formula = log(wage) ~ education + experience + region + gender + 
    occupation + sector + union + married, data = data)
summary(md.final)

summary(lm(formula = log(wage) ~ education + I(experience/10) + region + gender + 
    occupation + sector + union + married, data = data)) # rescale experience coefficient

## md.final diagnostics## 
plot(md.final)
# constant variance
plot(residuals(md.final)~fitted(md.final),main="final model")
abline(h=0,col="blue")
#normality
qqnorm(residuals(md.final))
qqline(residuals(md.final),col="blue")
shapiro.test(residuals(md.final))
#outlier and influential obsevation
library(faraway)
halfnorm(cooks.distance(md.final),main="cooks distance")
anova(md.final)
drop1(md.final,test="F")

# study the model for parsimony
anova(md.final)
drop1(md.final,test="F")
# refit a final model
md.final2<-lm(formula = log(wage) ~ education + experience + region + gender + 
                 occupation + union + married, data = data)
summary(md.final2)

anova(md.final2)
drop1(md.final2,test="F")

# coef for interpretatiion 
round(coef(md.final2),3)
round(exp(coef(md.final2)),3)

