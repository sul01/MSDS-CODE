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


#PCA
#correlation matrix
CORR=cor(DATA[,-(1:3)])

#eigenvalues/vectors
lambda=eigen(CORR)$values
W=eigen(CORR)$vectors
PVEs=NULL
for(r in 1:400) PVEs=c(PVEs,sum(lambda[1:r])/400)
R=c(1:400)
EIG=as.data.frame(cbind(R,lambda,PVEs,W))
r=which(PVEs[]>0.95)[1]

#create new dataset
W.t=t(W) #transpose W
Y=as.matrix(DATA[,-(1:3)])%*%W.t #principal components
Y=Y[,1:r]
newDATA=cbind(DATA[,1:3],as.data.frame(Y))

#re-classifying the new data
#classes
CL1n=subset(newDATA,newDATA[,1]=='BITSTREAMVERA') 
CL2n=subset(newDATA,newDATA[,1]=='CONSOLAS') 
CL3n=subset(newDATA,newDATA[,1]=='EBRIMA') 

#assign selections ~20% of each class
r1n=sort(sample(nrow(CL1n),nrow(CL1n)*0.2))
r2n=sort(sample(nrow(CL2n),nrow(CL2n)*0.2))
r3n=sort(sample(nrow(CL3n),nrow(CL3n)*0.2))
#define test&training sets
testCL1n=CL1n[r1n,];trainCL1n=CL1n[-r1n,]
testCL2n=CL2n[r2n,];trainCL2n=CL2n[-r2n,]
testCL3n=CL3n[r3n,];trainCL3n=CL3n[-r3n,]
TRAINSET=rbind(trainCL1n,trainCL2n,trainCL3n)
TESTSET=rbind(testCL1n,testCL2n,testCL3n)