#Libraryies and Data sets
library(readr)
library(class)
library(ggplot2)
CONSOLAS=read_csv("Documents/R/fonts/CONSOLAS.csv")
EBRIMA=read_csv("Documents/R/fonts/EBRIMA.csv")
BITSTREAMVERA=read_csv("Documents/R/fonts/BITSTREAMVERA.csv")

#Cleaning and sorting
drop_names=c("fontVariant","m_label","orientation","m_top","m_left","originalH","originalW","h","w")
CL1=subset(BITSTREAMVERA[complete.cases(BITSTREAMVERA),-which(names(BITSTREAMVERA) %in% drop_names)],strength==0.4&italic==0)
CL2=subset(CONSOLAS[complete.cases(CONSOLAS),-which(names(CONSOLAS) %in% drop_names)],strength==0.4&italic==0)
CL3=subset(EBRIMA[complete.cases(EBRIMA),-which(names(EBRIMA) %in% drop_names)],strength==0.4&italic==0)
DATA=rbind(CL1,CL2,CL3)

#mean and standard deviation
m=apply(DATA[,-(1:3)],2,mean)
s=apply(DATA[,-(1:3)],2,sd)

#standardize
DATA=rbind(cbind(CL1[,1:3],t(t(sweep(CL1[,-(1:3)],2,m))/s)),
           cbind(CL2[,1:3],t(t(sweep(CL2[,-(1:3)],2,m))/s)),
           cbind(CL3[,1:3],t(t(sweep(CL3[,-(1:3)],2,m))/s)))

set.seed(1) #make random selection reproducible
#assign selections ~20% of each class
r1=sort(sample(nrow(CL1),nrow(CL1)*0.2))
r2=sort(sample(nrow(CL2),nrow(CL2)*0.2))
r3=sort(sample(nrow(CL3),nrow(CL3)*0.2))

#define test&training sets
testCL1=CL1[r1,];trainCL1=CL1[-r1,]
testCL2=CL2[r2,];trainCL2=CL2[-r2,]
testCL3=CL3[r3,];trainCL3=CL3[-r3,]
TRAINSET=rbind(trainCL1,trainCL2,trainCL3)
TESTSET=rbind(testCL1,testCL2,testCL3)

#KNN
Kbest=1

#train/test
train1=knn(TRAINSET[,-(1:3)],TRAINSET[,-(1:3)],TRAINSET$font,Kbest)
test1=knn(TRAINSET[,-(1:3)],TESTSET[,-(1:3)],TRAINSET$font,Kbest)

trainperf1=mean(train1==TRAINSET$font)
testperf1=mean(test1==TESTSET$font)

#confusion matrices 
trainconf1=table(TRAINSET$font,train1);trainconf1=trainconf1/apply(trainconf1,1,sum)
testconf1=table(TESTSET$font,test1);testconf1=testconf1/apply(testconf1,1,sum)

#PCA
#correlation matrix
CORR=cor(DATA[,-(1:3)])

#eigenvalues/vectors
lambda = eigen(CORR)$values
W = eigen(CORR)$vectors

PVEs = NULL
for(r in 1:400) PVEs=c(PVEs, sum(lambda[1:r])/400)
R=c(1:400)
EIG=as.data.frame(cbind(R,lambda,PVEs,W))

#eigenvalues vs r
plot(lambda, ylab='eigenvalue', xlab='r')
ggplot(EIG,aes(R,lambda))+
  geom_point()+
  labs(y="Eigen Values")

#PVE vs r
ggplot(EIG,aes(R,PVEs))+
  geom_point()+
  labs(y="% Variacne")


r = which(PVEs[]>0.95)[1]

#create new dataset
W.t = t(W) #transpose W
Y = as.matrix(DATA[,-(1:3)])%*%W.t #principal components
Y = Y[,1:r]

newDATA = cbind(DATA[,1:3], as.data.frame(Y))

#re-classifying the new data
#classes
CL1n = subset(newDATA, newDATA[,1]=='BITSTREAMVERA') 
CL2n = subset(newDATA, newDATA[,1]=='CONSOLAS') 
CL3n = subset(newDATA, newDATA[,1]=='EBRIMA') 

#assign selections ~20% of each class
r1n = sort(sample(nrow(CL1n),nrow(CL1n)*0.2))
r2n = sort(sample(nrow(CL2n),nrow(CL2n)*0.2))
r3n = sort(sample(nrow(CL3n),nrow(CL3n)*0.2))
#define test&training sets
testCL1n = CL1n[r1n,]; trainCL1n=CL1n[-r1n,]
testCL2n = CL2n[r2n,]; trainCL2n=CL2n[-r2n,]
testCL3n = CL3n[r3n,]; trainCL3n=CL3n[-r3n,]

TRAINSETn=rbind(trainCL1n,trainCL2n,trainCL3n)
TESTSETn=rbind(testCL1n,testCL2n,testCL3n)

#redo KNN with newDATA
Kbest=1

#train/test
train1n=knn(TRAINSETn[,-(1:3)],TRAINSETn[,-(1:3)],TRAINSETn$font,Kbest)
test1n=knn(TRAINSETn[,-(1:3)],TESTSETn[,-(1:3)],TRAINSETn$font,Kbest)

trainperf1n=mean(train1n==TRAINSETn$font)
testperf1n=mean(test1n==TESTSETn$font)

#confusion matrices 
trainconf1n=table(TRAINSETn$font,train1n)
testconf1n=table(TESTSETn$font,test1n)

#conf in %'s
trainconf1n=trainconf1n/apply(trainconf1n,1,sum)
testconf1n=testconf1n/apply(testconf1n,1,sum)

#scatter plots
Cln=as.data.frame(rbind(CL1n,CL2n,CL3n))
names(Cln)[c(1,4:7)]=c("Font","Y1","Y2","Y3","Y4")
h=c(7,13,19,14,20,21)
for(i in 1:6){
  plot=ggplot(Cln,aes_string(x=names(Cln)[expand.grid(1:6,1:6)[h[i],1]+3],
                             y=names(Cln)[expand.grid(1:6,1:6)[h[i],2]+3]))+
    geom_point(aes(color=Font))+
    theme(legend.position="bottom")
  print(plot)
}



