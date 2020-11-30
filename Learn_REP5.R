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
n=1
set.seed(n);r1n=sort(sample(nrow(CL1n),nrow(CL1n)*0.2))
set.seed(n);r2n=sort(sample(nrow(CL2n),nrow(CL2n)*0.2))
set.seed(n);r3n=sort(sample(nrow(CL3n),nrow(CL3n)*0.2))
#define test&training sets
testCL1n=CL1n[r1n,];trainCL1n=CL1n[-r1n,]
testCL2n=CL2n[r2n,];trainCL2n=CL2n[-r2n,]
testCL3n=CL3n[r3n,];trainCL3n=CL3n[-r3n,]
TRAINSET=rbind(trainCL1n,trainCL2n,trainCL3n)
TESTSET=rbind(testCL1n,testCL2n,testCL3n)
TRAINSET$font=as.factor(TRAINSET$font)
TESTSET$font=as.factor(TESTSET$font)

#3.1
rfTrain = randomForest(font~., data=TRAINSET, ntree=100, mtry=sqrt(r))
rfPred = predict(rfTrain, TESTSET)

#conf. matrix in %
rfTrain$confusion[,-4]/apply(rfTrain$confusion[,-4],1,sum)
rfTestconf = table(TESTSET$font, rfPred); rfTestconf/apply(rfTestconf,1,sum)

#3.2
ntrees = c(1:400) 
rfn = vector(mode = "list", length = length(ntrees)) #test predictions
accn = NULL #global accuracies
bitstreamACCn = NULL; consolasACCn = NULL; ebrimaACCn = NULL #diagonals of the conf. matrix

for(i in 1:length(ntrees)){
  rfn[[i]] = predict(randomForest(font~., data=TRAINSET, ntree=ntrees[i], mtry=sqrt(r)), TESTSET)
  confn = table(TESTSET$font, rfn[[i]])
  accn = c(accn, sum(diag(confn))/sum(confn))
  percent = (confn/apply(confn,1,sum))
  bitstreamACCn = c(bitstreamACCn, diag(percent)[1])
  consolasACCn = c(consolasACCn, diag(percent)[2])
  ebrimaACCn = c(ebrimaACCn, diag(percent)[3])
}

  #print(sprintf("conf%i (n=%i):", i, ntrees[i]))
  #print(percent)#display all matrices


#accuracy  vs ntrees
ACTREE=as.data.frame(cbind(ntrees,accn,bitstreamACCn,consolasACCn,ebrimaACCn))

ggplot(ACTREE)+
  geom_line(aes(ntrees,accn))+
  labs(x="Number of Trees",y="Accuracy")

#diagonals(font acc) vs ntrees
ggplot(ACTREE)+
  geom_line(aes(ntrees,bitstreamACCn,color="red"))+
  geom_line(aes(ntrees,consolasACCn,color="blue"))+
  geom_line(aes(ntrees,ebrimaACCn,color="green"))+
  labs(x="Number of Trees",y="Accuracy")+
  scale_color_manual(labels=names(ACTREE[,-c(1:2)]),values=c("red","blue","green"))

bntr = which.max(accn) #best ntree 

#4.1
bestRF = randomForest(font~., data=TRAINSET, ntree=bntr, mtry=sqrt(r), importance=T)

#eigenvalues vs importance
IMK=as.data.frame(cbind("LK"=lambda[1:r],"IMK"=bestRF$importanceSD[-c(1,2),4]))

ggplot(IMK,aes(LK,IMK))+
  geom_point()+
  labs(x="Eignvalue",y="Importances")

#performance of bestRF on testset
bestPred = predict(bestRF, TESTSET) 
bestTestconf = table(TESTSET$font, bestPred)
bestTestconf = bestTestconf/apply(bestTestconf,1,sum)
bestacc = sum(diag(bestTestconf))/sum(bestTestconf)#global accuracy


#4.2
#assign new selections ~20% of each class
set.seed(2);r1n=sort(sample(nrow(CL1n),nrow(CL1n)*0.2))
set.seed(2);r2n=sort(sample(nrow(CL2n),nrow(CL2n)*0.2))
set.seed(2);r3n=sort(sample(nrow(CL3n),nrow(CL3n)*0.2))
#define new test&training sets
testCL1n_n=CL1n[r1n,];trainCL1n_n=CL1n[-r1n,]
testCL2n_n=CL2n[r2n,];trainCL2n_n=CL2n[-r2n,]
testCL3n_n=CL3n[r3n,];trainCL3n_n=CL3n[-r3n,]
newTRAINSET=rbind(trainCL1n_n,trainCL2n_n,trainCL3n_n)
newTESTSET=rbind(testCL1n_n,testCL2n_n,testCL3n_n)
newTRAINSET$font=as.factor(newTRAINSET$font)
newTESTSET$font=as.factor(newTESTSET$font)

newRF = randomForest(font~., data=newTRAINSET, ntree=bntr, mtry=sqrt(r))
newPred = predict(newRF, newTESTSET)

#Accuracy & test conf. matrix
newTestconf = table(newTESTSET$font, newPred)
accn_n = sum(diag(newTestconf))/sum(newTestconf)
percent_n = newTestconf/apply(newTestconf,1,sum)
bitstreamACCn_n = diag(percent_n)[1]
consolasACCn_n = diag(percent_n)[2]
ebrimaACCn_n = diag(percent_n)[3]
newTestconf = newTestconf/apply(newTestconf,1,sum)

#confidence intervals
#comparing overall accuracies:
sigmanew = sqrt(accn_n*(1-accn_n)/ nrow(newTESTSET))
sigmabest = sqrt(bestacc*(1-bestacc)/ nrow(TESTSET))
#90% conf. intervals:
intervalnew = c(accn_n-sigmanew*qnorm(1-0.1/2), accn_n+sigmanew*qnorm(1-0.1/2))
intervalbest = c(bestacc-sigmabest*qnorm(1-0.1/2), bestacc+sigmabest*qnorm(1-0.1/2))
#there is significant overlap between these 2 intervals -> evidence that accuracy for both 
#bestRF and newRF should be similar, which they are (0.824 vs 0.831)

#comparing confusion matrices (accuracy for each class):
#newRF:
p = diag(newTestconf)
sigmaCL1new=sqrt(p[1]*(1-p[1])/nrow(testCL1n_n))
sigmaCL2new=sqrt(p[2]*(1-p[2])/nrow(testCL2n_n))
sigmaCL3new=sqrt(p[3]*(1-p[3])/nrow(testCL3n_n))
#90% confidence interval
intervalCL1new=c(p[1]-sigmaCL1new*qnorm(1-0.1/2), p[1]+sigmaCL1new*qnorm(1-0.1/2))
intervalCL2new=c(p[2]-sigmaCL2new*qnorm(1-0.1/2), p[2]+sigmaCL2new*qnorm(1-0.1/2))
intervalCL3new=c(p[3]-sigmaCL3new*qnorm(1-0.1/2), p[3]+sigmaCL3new*qnorm(1-0.1/2))

#bestRF:
p2 = diag(bestTestconf)
sigmaCL1best=sqrt(p2[1]*(1-p2[1])/nrow(testCL1n))
sigmaCL2best=sqrt(p2[2]*(1-p2[2])/nrow(testCL2n))
sigmaCL3best=sqrt(p2[3]*(1-p2[3])/nrow(testCL3n))
#90% confidence interval
intervalCL1best=c(p2[1]-sigmaCL1best*qnorm(1-0.1/2), p2[1]+sigmaCL1best*qnorm(1-0.1/2))
intervalCL2best=c(p2[2]-sigmaCL2best*qnorm(1-0.1/2), p2[2]+sigmaCL2best*qnorm(1-0.1/2))
intervalCL3best=c(p2[3]-sigmaCL3best*qnorm(1-0.1/2), p2[3]+sigmaCL3best*qnorm(1-0.1/2))
#similar conclusion when comparing by each font acc 

#5.1
#clone the single class, rename the combined classes' classifiers
#C1 vs C2+C3:
trainC23 = rbind(trainCL2n, trainCL3n); trainC23$font = 'CONSOLAS/EBRIMA'
testC23 = rbind(testCL2n, testCL3n); testC23$font = 'CONSOLAS/EBRIMA'
trainC1vs23 = droplevels(rbind(rbind(trainCL1n, trainCL1n), trainC23))
testC1vs23 = droplevels(rbind(rbind(testCL1n, testCL1n), testC23))
trainC1vs23$font=as.factor(trainC1vs23$font)
testC1vs23$font=as.factor(testC1vs23$font)
RF1 = randomForest(font~., data=trainC1vs23, ntree=bntr, mtry=sqrt(r))

#C2 vs C1+C3:
trainC13 = rbind(trainCL1n, trainCL3n); trainC13$font = 'BITSTREAM/EBRIMA'
testC13 = rbind(testCL1n, testCL3n); testC13$font = 'BITSTREAM/EBRIMA'
trainC2vs13 = droplevels(rbind(rbind(trainCL2n, trainCL2n), trainC13))
testC2vs13 = droplevels(rbind(rbind(testCL2n, testCL2n), testC13))
trainC2vs13$font=as.factor(trainC2vs13$font)
testC2vs13$font=as.factor(testC2vs13$font)
RF2 = randomForest(font~., data=trainC2vs13, ntree=bntr, mtry=sqrt(r))

#C3 vs C1+C2:
trainC12 = rbind(trainCL1n, trainCL2n); trainC12$font = 'BITSTREAM/CONSOLAS'
testC12 = rbind(testCL1n, testCL2n); testC12$font = 'BITSTREAM/CONSOLAS'
trainC3vs12 = droplevels(rbind(rbind(trainCL3n, trainCL3n), trainC12))
testC3vs12 = droplevels(rbind(rbind(testCL3n, testCL3n), testC12))
trainC3vs12$font=as.factor(trainC3vs12$font)
testC3vs12$font=as.factor(testC3vs12$font)
RF3 = randomForest(font~., data=trainC3vs12, ntree=bntr, mtry=sqrt(r))

#confusion matrix of test results
pred1 = table(testC1vs23$font, predict(RF1, testC1vs23))
pred2 = table(testC2vs13$font, predict(RF2, testC2vs13))
pred3 = table(testC3vs12$font, predict(RF3, testC3vs12))

#global accuracies on test sets
A1 = sum(diag(pred1))/sum(pred1)
A2 = sum(diag(pred2))/sum(pred2)
A3 = sum(diag(pred3))/sum(pred3)

#conf. matrix in percentage
M1 = pred1/apply(pred1,1,sum)
M2 = pred2/apply(pred2,1,sum)
M3 = pred3/apply(pred3,1,sum)

#5.2
#C1 vs C2+C3
pred1best = table(testC1vs23$font, predict(bestRF, testC1vs23))
pred1best = cbind(pred1best[,1], pred1best[,2]+pred1best[,3])
colnames(pred1best) = rownames(pred1best)

#C2 vs C1+C3
pred2best = table(testC2vs13$font, predict(bestRF, testC2vs13))
pred2best = cbind(pred2best[,1]+pred2best[,3],pred2best[,2])
colnames(pred2best) = rownames(pred2best)

#C3 vs C1+C2
pred3best = table(testC3vs12$font, predict(bestRF, testC3vs12))
pred3best = cbind(pred3best[,1]+pred3best[,2],pred3best[,3])
colnames(pred3best) = rownames(pred3best)

#global accuracies on test sets
B1 = sum(diag(pred1best))/sum(pred1best)
B2 = sum(diag(pred2best))/sum(pred2best)
B3 = sum(diag(pred3best))/sum(pred3best)

#conf. matrix in percentage
BM1 = pred1best/apply(pred1best,1,sum)
BM2 = pred2best/apply(pred2best,1,sum)
BM3 = pred3best/apply(pred3best,1,sum)
