#Libraryies and Data sets
library(readr)
library(ggplot2)
library(randomForest)
CONSOLAS=read_csv("Documents/R/fonts/CONSOLAS.csv")
EBRIMA=read_csv("Documents/R/fonts/EBRIMA.csv")
BITSTREAMVERA=read_csv("Documents/R/fonts/BITSTREAMVERA.csv")
set.seed(1)

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

#3.1
rfTrain = randomForest(font~., data=TRAINSET, ntree=100, mtry=sqrt(r))
rfPred = predict(rfTrain, TESTSET)

#conf. matrix in %
rfTrain$confusion[,-4]/apply(rfTrain$confusion[,-4],1,sum)
rfTestconf = table(TESTSET$font, rfPred); rfTestconf/apply(rfTestconf,1,sum)

#3.2
ntrees = c(10,50,100,200,300,400)
rfn = vector(mode = "list", length = length(ntrees)) #test predictions
for(n in 1:length(ntrees)){
  rfn[[n]] = predict(randomForest(font~., data=TRAINSET, ntree=ntrees[n], mtry=sqrt(r)), TESTSET)
}

accn = NULL #global accuracies
bitstreamACCn = NULL; consolasACCn = NULL; ebrimaACCn = NULL #diagonals of the conf. matrix

for(i in 1:6){
  conf = table(TESTSET$font, rfn[[i]])
  accn = c(accn, sum(diag(conf))/sum(conf))
  percent = (conf/apply(conf,1,sum))
  bitstreamACCn = c(bitstreamACCn, diag(percent)[1])
  consolasACCn = c(consolasACCn, diag(percent)[2])
  ebrimaACCn = c(ebrimaACCn, diag(percent)[3])
  print(sprintf("conf%i (n=%i):", i, ntrees[i]))
  print(percent)#display all 6 matrices
}

#accuracy vs ntrees
plot(ntrees,accn, type='l')

#diagonals(font acc) vs ntrees
plot(ntrees, bitstreamACCn, type='l')
plot(ntrees, consolasACCn, type='l')
plot(ntrees, ebrimaACCn, type='l')

bntr = 100 #best ntree 
#(Explore around the 100 range to get even better bntr)

#4.1
bestRF = randomForest(font~., data=TRAINSET, ntree=bntr, mtry=sqrt(r), importance=T)
plot(lambda[1:r], bestRF$importanceSD[-c(1,2),4])

#performance of bestRF on testset
bestPred = predict(bestRF, TESTSET) 
bestTestconf = table(TESTSET$font, bestPred)
bestTestconf/apply(bestTestconf,1,sum)

#4.2
#assign new selections ~20% of each class
r1n=sort(sample(nrow(CL1n),nrow(CL1n)*0.2))
r2n=sort(sample(nrow(CL2n),nrow(CL2n)*0.2))
r3n=sort(sample(nrow(CL3n),nrow(CL3n)*0.2))
#define new test&training sets
testCL1n=CL1n[r1n,];trainCL1n=CL1n[-r1n,]
testCL2n=CL2n[r2n,];trainCL2n=CL2n[-r2n,]
testCL3n=CL3n[r3n,];trainCL3n=CL3n[-r3n,]
newTRAINSET=rbind(trainCL1n,trainCL2n,trainCL3n)
newTESTSET=rbind(testCL1n,testCL2n,testCL3n)

newRF = randomForest(font~., data=newTRAINSET, ntree=bntr, mtry=sqrt(r))
newPred = predict(newRF, newTESTSET)
#test conf. matrix
newTestconf = table(newTESTSET$font, newPred)
newTestconf/apply(newTestconf,1,sum)

#confidence intervals

#5

