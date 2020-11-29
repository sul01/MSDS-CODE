library(ALSM)
library(ggplot2)

#7.3
brand=read.table("~/Desktop/brand.txt",quote="\"",comment.char="")
names(brand)=c("pref","moisture","sweetness")
attach(brand)
anova(update(lm(pref~moisture+sweetness), . ~ . - moisture),lm(pref~moisture+sweetness))
anova(update(lm(pref~moisture+sweetness), . ~ . - sweetness),lm(pref~moisture+sweetness))
summary(lm(pref~moisture+sweetness))

#7.12
t(as.matrix(cbind(rep(1,16),moisture,sweetness)))%*%as.matrix(cbind(rep(1,16),moisture,sweetness))
summary(lm(pref~moisture))
summary(lm(pref~sweetness))
summary(lm(pref~moisture+sweetness))
anova(lm(pref~moisture))$"Sum Sq"[1]
anova(lm(pref~sweetness))$"Sum Sq"[2]
cor(moisture,sweetness)
anova(update(lm(pref~moisture+sweetness),.~.-moisture),lm(pref~moisture+sweetness))$"Sum of Sq"[2]/anova(lm(pref~sweetness))$"Sum Sq"[2]
anova(update(lm(pref~moisture+sweetness),.~.-sweetness),lm(pref~moisture+sweetness))$"Sum of Sq"[2]/anova(lm(pref~moisture))$"Sum Sq"[2]
cor(moisture,sweetness)

#7.16
brandT=as.data.frame(scale(brand))
names(brandT)=c("prefT","moistureT","sweetnessT")
attach(brandT)
summary(lm(prefT~moistureT+sweetnessT,data=brandT))
sy=sqrt(sum((pref-mean(pref))^2)/15)
sm=sqrt(sum((moisture-mean(moisture))^2)/15)
sw=sqrt(sum((sweetness-mean(sweetness))^2)/15)
(sy/sm)*0.8924
(sy/sw)*0.3946
summary(lm(pref~moisture+sweetness))

#7.24
summary(lm(pref~moisture))
summary(lm(pref~moisture+sweetness))
anova(lm(pref~moisture))
anova(lm(pref~sweetness+moisture))
cor(moisture,sweetness)

#8.6
steroid=read.table("~/Desktop/steroid.txt",quote="\"",comment.char="")
names(steroid)=c("level","age")
steroid$age=age-mean(age)
steroid$age2=age^2
attach(steroid)
J=lm(level~age+age2,data=steroid)
summary(J)
anova(J)
x=age-mean(age)
y=level
yh=summary(lm(level~age+age2))$coefficients[1,1]+
  summary(lm(level~age+age2))$coefficients[2,1]*x+
  summary(lm(level~age+age2))$coefficients[3,1]*x^2
y2=summary(lm(level~age+age2))$coefficients[1,1]+
  summary(lm(level~age+age2))$coefficients[2,1]*x
Z=as.data.frame(cbind(x,y,yh,y2))
ggplot(Z)+
  geom_point(aes(x,y))+
  geom_line(aes(x,yh))+
  geom_line(aes(x,y2,color="red"))+
  labs(x="",y="")
J0=lm(level~1)
JJ=lm(level~age)
anova(J0,J)
anova(JJ,J)

#B
job=read.table("~/Desktop/job.txt", quote="\"", comment.char="")
names(job)=c("X1","X2","X3","X4","Y")
attach(job)
step(lm(Y~X1+X2+X3+X4),direction="backward")
step(lm(Y~X1+X2+X3+X4),direction="both")
step(lm(Y~1,data=job),direction="forward",scope=formula(lm(Y~X1+X2+X3+X4)))
step(lm(Y~1,data=job),direction="both",scope=formula(lm(Y~X1+X2+X3+X4)))


