#libraries
library(readr)
library(plyr)
library(dplyr)
library(ggplot2)
library(heuristica)

#Import and cleaning
auto=read_csv("Documents/R/cleanDataAuto.csv")
auto=Auto[complete.cases(cleanDataAuto),]
cat(nrow(auto), "cases")
attach(auto)
AUTO=as.matrix(auto)

#functions
  #Scoreing function
  scoreCalc=function(M){
    f_temp=numeric(5)
    scoreF=matrix(NA,nrow(M),5)
    for(n in 1:nrow(M)){
      for(f in 1:4){
        if(M[n,f] >= thrF[f]){
          f_temp[f] <- -1
        }
        else{
          f_temp[f] <- 1
        }
      }
      if(M[n,5] <= thrF[5]){
        f_temp[5] <- -1
      }
      else{
        f_temp[5] <- 1
      }
      scoreF[n,] <- f_temp
    }
    return (scoreF)
  }
  #Classifier function
  classifier=function(A, s){
    c=numeric(length(s))
    for(i in 1:length(s)){
      if(s[i]>=A) c[i]=1
      else if(s[i]<A) c[i]=-1
    }
    return (c)
  }
  #True class function
  trueclass=function(A, s){
    c=numeric(length(s))
    for(i in 1:length(s)){
      if(s[i]>A) c[i]=1
      else if(s[i]<A) c[i]=-1
    }
    return (c)
  }
  
  
#mean and standard deviation
MSD=data.frame(mean=apply(auto, 2, mean),sd=apply(auto, 2, sd))


#Feature Histogram
for(i in 1:6){
  x=ggplot(auto,aes(AUTO[,i]))+
    geom_histogram()+
    labs(x=names(auto)[i])+
    theme(text=element_text(size=20),plot.title = element_text(hjust = 0.5))
  print(x)
}

#Related pdf with same mean/sd
n=seq(-10000,10000,0.1)
norms=data.frame(n=n,
                 mpg=dnorm(n,MSD[1,1],MSD[1,2]),
                 cylinders=dnorm(n,MSD[2,1],MSD[2,2]),
                 displacement=dnorm(n,MSD[3,1],MSD[3,2]),
                 horsepower=dnorm(n,MSD[4,1],MSD[4,2]),
                 weight=dnorm(n,MSD[5,1],MSD[5,2]),
                 acceleration=dnorm(n,MSD[6,1],MSD[6,2]))
ggplot(norms,aes(n,cylinders))+
  geom_line()+
  xlim(-5,15)+
  labs(title="cylinders",x="",y="")+
  theme(text=element_text(size=20),plot.title=element_text(hjust=0.5))
ggplot(norms,aes(n,displacement))+
  geom_line()+
  xlim(-150,550)+
  labs(title="displacement",x="",y="")+
  theme(text=element_text(size=20),plot.title=element_text(hjust=0.5))
ggplot(norms,aes(n,horsepower))+
  geom_line()+
  xlim(-50,250)+
  labs(title="horsepower",x="",y="")+
  theme(text=element_text(size=20),plot.title=element_text(hjust=0.5))
ggplot(norms,aes(n,weight))+
  geom_line()+
  xlim(0,6000)+
  labs(title="weight",x="",y="")+
  theme(text=element_text(size=20),plot.title=element_text(hjust=0.5))
ggplot(norms,aes(n,acceleration))+
  geom_line()+
  xlim(5,25)+
  labs(title="acceleration",x="",y="")+
  theme(text=element_text(size=20),plot.title=element_text(hjust=0.5))

#Scatter of features
ggplot(auto,aes(AUTO[,2],mpg))+
  geom_jitter()+
  labs(x="cylinders")+
  theme(text=element_text(size=20))
for(i in 3:6){
  x=ggplot(auto,aes(AUTO[,i],mpg))+
    geom_point()+labs(x=names(auto)[i])+
    theme(text=element_text(size=20))
  print(x)
}

#Correlations
corF=numeric(6)
for(i in 1:6){
  corF[i]=cor(mpg,auto[,i])
}
Wcor=cor(auto[,-1])

#Qunatile curve
qF=quantile(mpg,seq(0, 1, 0.01))
seq1=seq(0,100,1)
quant=data.frame(seq1,qF)
names(quant)=c("Q","mpg")
ggplot(quant,aes(Q,mpg))+geom_step(direction="hv")+
  labs(x="Q%")+
  theme(text=element_text(size=20))

#Sorting by qunatile
LOWmpg=auto[mpg<=max(qF[0:34]),]
HIGHmpg=auto[mpg>min(qF[67:101]),]

#Histograms of features for post-sort
for(i in 2:6){
  x=ggplot(LOWmpg,aes(as.matrix(LOWmpg)[,i]))+
    geom_histogram()+labs(title="LOW",x=names(auto)[i])+
    theme(text=element_text(size=20))
  y=ggplot(HIGHmpg,aes(as.matrix(HIGHmpg)[,i]))+
    geom_histogram()+labs(title="HIGH",x=names(auto)[i])+
    theme(text=element_text(size=20))
  print(x)
  print(y)
}

#Calcultion of threshold
mL=apply(LOWmpg[,-1], 2, mean)
mH=apply(HIGHmpg[,-1], 2, mean)
stdL=apply(LOWmpg[,-1], 2, sd)
stdH=apply(HIGHmpg[,-1], 2, sd)
s=sqrt((stdL^2 + stdH^2)/131)
discr=abs(mH-mL)/s
thrF=(mL*stdH+mH*stdL)/(stdH+stdL)

#Full score 
fscore=apply(scoreCalc(AUTO[,-1]),1,sum)

#train/test split
train=filter(auto,!between(mpg,max(qF[0:34]),min(qF[67:101])))
test=filter(auto,between(mpg,max(qF[0:34]),min(qF[67:101])))

#True classification 
Rclass_true=trueclass(median(mpg),as.matrix(train[,1]))
Eclass_true=trueclass(median(mpg),as.matrix(test[,1]))

#Predicted classification train(R)/test(E)
Rclass_pred1=classifier(1,apply(scoreCalc(train[,-1]),1,sum))
Rclass_pred2=classifier(2,apply(scoreCalc(train[,-1]),1,sum))
Rclass_pred3=classifier(3,apply(scoreCalc(train[,-1]),1,sum))

Eclass_pred1=classifier(1,apply(scoreCalc(test[,-1]),1,sum))
Eclass_pred2=classifier(2,apply(scoreCalc(test[,-1]),1,sum))
Eclass_pred3=classifier(3,apply(scoreCalc(test[,-1]),1,sum))

#Confusion martix train(R)/test(E)
Ctrain1=table(Rclass_pred1,Rclass_true)
Ctrain2=table(Rclass_pred2,Rclass_true)
Ctrain3=table(Rclass_pred3,Rclass_true)

Ctest1=table(Eclass_pred1,Eclass_true)
Ctest2=table(Eclass_pred2,Eclass_true)
Ctest3=table(Eclass_pred3,Eclass_true)

#Results
Fbar
sdF
corF
Wcor
mL
mH
stdL
stdH
s
discr
thrF
Ctrain1
Ctest1
Ctrain2
Ctest2
Ctrain3
Ctest3