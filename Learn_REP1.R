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
      else if(s[i]<=-A) c[i]=-1
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
Fbar=apply(auto, 2, mean)
sdF=apply(auto, 2, sd)

#Feature Histogram and related pdf with same mean/sd
for(i in 1:6){
hist(AUTO[,i],main=paste("histogram of", names(auto)[i]),xlab=names(auto)[i])
seq=seq(-100,100,by=0.1)
pdf=dnorm(seq,Fbar[i],sdF[i])
plot(seq,pdf,main=paste("normal w/ mean & sd of",names(auto)[i]),xlab="",ylab="")
}

#Scatter of features
for(i in 2:6){
  print(ggplot(auto,aes(AUTO[,i],mpg))+geom_point())
}

#Correlations
corF=numeric(6)
for(i in 1:6){
  corF[i]=cor(mpg,auto[,i])
}
Wcor=cor(auto[,-1])

#Qunatile curve
qF=quantile(mpg,seq(0, 1, 0.01))
plot(qF,type="l",xlab="Q%",ylab="mpg")

#Sorting by qunatile
LOWmpg=as.matrix(auto[mpg<=max(qF[0:34]),])
HIGHmpg=as.matrix(auto[mpg>min(qF[67:101]),])

#Histograms of features for post-sort
for(i in 2:6){
  hist(LOWmpg[,i],main=paste("Histogram of",names(auto)[i],"(lowmpg)"),xlab=names(auto)[i])
  hist(HIGHmpg[,i],main=paste("Histogram of",names(auto)[i],"(highmpg)"),xlab=names(auto)[i])
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
Ctrain2
Ctrain3
Ctest1
Ctest2
Ctest3
