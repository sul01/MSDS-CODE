#Libraryies and Data sets
library(readr)
library(class)
library(ggplot2)
VLADIMIR=read_csv("Documents/R/fonts/VLADIMIR.csv")
EBRIMA=read_csv("Documents/R/fonts/EBRIMA.csv")
BITSTREAMVERA=read_csv("Documents/R/fonts/BITSTREAMVERA.csv")

#functions
normalize=function(x) (x- min(x))/(max(x) - min(x))


#Cleaning and sorting
drop_names=c("fontVariant","m_label","orientation","m_top","m_left","originalH","originalW","h","w")
CL1=subset(VLADIMIR[complete.cases(VLADIMIR),-which(names(VLADIMIR) %in% drop_names)],strength==0.4&italic==0)
CL2=subset(EBRIMA[complete.cases(EBRIMA),-which(names(EBRIMA) %in% drop_names)],strength==0.4&italic==0)
CL3=subset(BITSTREAMVERA[complete.cases(BITSTREAMVERA),-which(names(BITSTREAMVERA) %in% drop_names)],strength==0.4&italic==0)
DATA=rbind(CL1,CL2,CL3)

#(0)
#mean and standard deviation
m=apply(DATA[,-(1:3)],2,mean)
s=apply(DATA[,-(1:3)],2,sd)

#standardize
CL1=cbind(CL1[,1:3],t(t(sweep(CL1[,-(1:3)],2,m))/s))
CL2=cbind(CL2[,1:3],t(t(sweep(CL2[,-(1:3)],2,m))/s))
CL3=cbind(CL3[,1:3],t(t(sweep(CL3[,-(1:3)],2,m))/s))
DATA=rbind(CL1,CL2,CL3)

#correlation matrix
CORR=cor(DATA[,-(1:3)])

#finding highest correlation values
up=upper.tri(CORR)
out=data.frame(which(up, arr.ind=TRUE), cor=CORR[up])
out=out[!is.na(out$cor),]
out=out[order(abs(out$cor), decreasing=TRUE),]
out$row=rownames(CORR)[out$row];out$col=colnames(CORR)[out$col]
top10Cor=out[1:10,]

#(1.0)
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

#(1.1)
K=12

#train/test
train12=knn(TRAINSET[,-(1:3)],TRAINSET[,-(1:3)],TRAINSET$font,K)
test12=knn(TRAINSET[,-(1:3)],TESTSET[,-(1:3)],TRAINSET$font, K)

trainperf12=mean(train12==TRAINSET$font)
testperf12=mean(test12==TESTSET$font)

#(1.2) Determine best K by elbow method
#takes a while to run
K=c(5,10,15,20,30,40,50,100)
testperfK=NULL
for (i in K){ 
  testknn=knn(TRAINSET[,-(1:3)],TESTSET[,-(1:3)],TRAINSET$font, i)
  testperfK=c(testperfK,mean(testknn==TESTSET$font))
}

Kval=as.data.frame(cbind(K,testperfK))

#plot 0<=K<=100
ggplot(Kval,aes(K,testperfK))+
  geom_line()+
  labs(x="K",y="% Correct classification on TESTSET",title="0<=K<=100")+
  scale_x_continuous(breaks=round(seq(0,100,by=10),1))

#(1.3) Closer inspection around K=100
K_knee=c(95,96,97,98,99,100,101,102,103,104,105)
testperfK_knee=NULL
for (i in K_knee){ 
  testknn_knee=knn(TRAINSET[,-(1:3)],TESTSET[,-(1:3)],TRAINSET$font,i)
  testperfK_knee=c(testperfK_knee,mean(testknn_knee==TESTSET$font))
}

Kval_knee=as.data.frame(cbind(K_knee,testperfK_knee))

#plot around K=101
ggplot(Kval_knee,aes(K_knee,testperfK_knee))+
  geom_line()+
  labs(x="K",y="% Correct classification on TESTSET",title="Values around K=101")+
  scale_x_continuous(breaks=round(seq(95,105,by=1),1))

#(1.4)
Kbest=101

#train/test
train101=knn(TRAINSET[,-(1:3)],TRAINSET[,-(1:3)],TRAINSET$font,Kbest)
test101=knn(TRAINSET[,-(1:3)],TESTSET[,-(1:3)],TRAINSET$font,Kbest)

trainperf101=mean(train101==TRAINSET$font)
testperf101=mean(test101==TESTSET$font)

#confusion matrices 
trainconf101=table(TRAINSET$font,train101)
testconf101=table(TESTSET$font,test101)

#conf in %'s
trainconf101=trainconf101/apply(trainconf101,1,sum)
testconf101=testconf101/apply(testconf101,1,sum)

#(1.5)
p=as.numeric(diag(testconf101))
#N*p(N)>4 for all 3 cases, assuming error has approximate normal distribution, 
sigmaCL1=sqrt(p[1]*(1-p[1])/nrow(testCL1))
sigmaCL2=sqrt(p[2]*(1-p[2])/nrow(testCL2))
sigmaCL3=sqrt(p[3]*(1-p[3])/nrow(testCL3))
#95% confidence interval
intervalCL1=c(p[1]-sigmaCL1*qnorm(1-0.05/2), p[1]+sigmaCL1*qnorm(1-0.05/2))
intervalCL2=c(p[2]-sigmaCL1*qnorm(1-0.05/2), p[2]+sigmaCL2*qnorm(1-0.05/2))
intervalCL3=c(p[3]-sigmaCL1*qnorm(1-0.05/2), p[3]+sigmaCL3*qnorm(1-0.05/2))

#(1.6,1.7) Binning by groups of pixels
PACK1=NULL;PACK2=NULL;PACK3=NULL;PACK4=NULL;

for(L in 0:9)  for(M in 0:9)  PACK1 = c(PACK1, sprintf('r%ic%i',L,M))
for(L in 0:9)  for(M in 10:19)  PACK2=c(PACK2,sprintf('r%ic%i',L,M))
for(L in 10:19)  for(M in 10:19)  PACK3=c(PACK3,sprintf('r%ic%i',L,M))
for(L in 10:19)  for(M in 0:9)  PACK4=c(PACK4,sprintf('r%ic%i',L,M))

P1KN=knn(TRAINSET[,PACK1],TESTSET[,PACK1],TRAINSET$font,101)
P2KN=knn(TRAINSET[,PACK2],TESTSET[,PACK2],TRAINSET$font,101)
P3KN=knn(TRAINSET[,PACK3],TESTSET[,PACK3],TRAINSET$font,101)
P4KN=knn(TRAINSET[,PACK4],TESTSET[,PACK4],TRAINSET$font,101)

w1=mean(P1KN==TESTSET$font)
w2=mean(P2KN==TESTSET$font)
w3=mean(P3KN==TESTSET$font)
w4=mean(P4KN==TESTSET$font)

#(1.8)Weigthing and normalization
COM=w1*TESTSET[,PACK1]+w2*TESTSET[,PACK2]+w3*TESTSET[,PACK3]+w4*TESTSET[,PACK4]
W_TRAINSET=cbind(w1*TRAINSET[,PACK1]/sum(COM),
                 w2*TRAINSET[,PACK2]/sum(COM),
                 w3*TRAINSET[,PACK3]/sum(COM),
                 w4*TRAINSET[,PACK4]/sum(COM))
W_TESTSET=cbind(w1*TESTSET[,PACK1]/sum(COM),
                w2*TESTSET[,PACK2]/sum(COM),
                w3*TESTSET[,PACK3]/sum(COM),
                w4*TESTSET[,PACK4]/sum(COM))

#KNN on normalized sets
W_train101=knn(W_TRAINSET,W_TRAINSET,TRAINSET$font,Kbest)
W_test101=knn(W_TRAINSET,W_TESTSET,TRAINSET$font,Kbest)

W_trainperf101=mean(W_train101==TRAINSET$font)
W_testperf101=mean(W_test101==TESTSET$font)

#confusion matrix
W_trainconf101=table(TRAINSET$font,W_train101)
W_testconf101=table(TESTSET$font,W_test101)

#conf in %'s
W_trainconf101/apply(trainconf101,1,sum) 
W_testconf101/apply(testconf101,1,sum)


