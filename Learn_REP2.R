#Libraryies and Data sets
library(readr)
library(class)
VLADIMIR=read_csv("Documents/R/fonts/VLADIMIR.csv")
EBRIMA=read_csv("Documents/R/fonts/EBRIMA.csv")
BITSTREAMVERA=read_csv("Documents/R/fonts/BITSTREAMVERA.csv")

#Cleaning and sorting
drop_names=c("fontVariant","m_label","orientation","m_top","m_left","originalH","originalW","h","w")
CL1=subset(VLADIMIR[complete.cases(VLADIMIR),-which(names(VLADIMIR) %in% drop_names)],strength==0.4&italic==0)
CL2=subset(EBRIMA[complete.cases(EBRIMA),-which(names(EBRIMA) %in% drop_names)],strength==0.4&italic==0)
CL3=subset(BITSTREAMVERA[complete.cases(BITSTREAMVERA),-which(names(BITSTREAMVERA) %in% drop_names)],strength==0.4&italic==0)
DATA=rbind(CL1,CL2,CL3)

#(0)
#mean and standard deviation
m = apply(DATA[,-(1:3)],2,mean)
s = apply(DATA[,-(1:3)],2,sd)

#standardize
CL1 = cbind(CL1[,1:3], t(t(sweep(CL1[,-(1:3)],2, m))/s))
CL2 = cbind(CL2[,1:3], t(t(sweep(CL2[,-(1:3)],2, m))/s))
CL3 = cbind(CL3[,1:3], t(t(sweep(CL3[,-(1:3)],2, m))/s))
SDATA = rbind(CL1,CL2,CL3)

#correlation matrix
CORR = cor(DATA[,-(1:3)])

#finding highest correlation values
up = upper.tri(CORR)
out = data.frame(which(up, arr.ind=TRUE), cor=CORR[up])
out = out[!is.na(out$cor),]
out = out[order(abs(out$cor), decreasing=TRUE),]
out$row = rownames(CORR)[out$row]
out$col = colnames(CORR)[out$col]
top10Cor = out[1:10,]

#(1.0)
set.seed(1) #make random selection reproducible
#assign selections ~20% of each class
r1 = sort(sample(nrow(CL1), nrow(CL1)*0.2))
r2 = sort(sample(nrow(CL2), nrow(CL2)*0.2))
r3 = sort(sample(nrow(CL3), nrow(CL3)*0.2))
#define test&training sets
testCL1 = CL1[r1,]; trainCL1 = CL1[-r1,]
testCL2 = CL2[r2,]; trainCL2 = CL2[-r2,]
testCL3 = CL3[r3,]; trainCL3 = CL3[-r3,]

TRAINSET = rbind(trainCL1, trainCL2, trainCL3)
TESTSET = rbind(testCL1, testCL2, testCL3)

#(1.1)
K=12
#training set:
train12 = knn(TRAINSET[,-(1:3)], TRAINSET[,-(1:3)], TRAINSET$font, K)
trainperf12 = mean(train12 == TRAINSET$font)
#test set:
test12 = knn(TRAINSET[,-(1:3)], TESTSET[,-(1:3)], TRAINSET$font, K)
testperf12 = mean(test12 == TESTSET$font) 

#confusion matrices
trainconf12 = table(TRAINSET$font, train12) #confusion matrix
trainconf12/apply(trainconf12, 1, sum) #in percentages
testconf12 = table(TESTSET$font, test12)
testconf12/apply(testconf12, 1, sum)

#(1.2) 
K = c(5,10,15,20,30,40,50,100)
testperfK = NULL
for (i in K){ 
  testknn = knn(TRAINSET[,-(1:3)], TESTSET[,-(1:3)], TRAINSET$font, i)
  testperfK = c(testperfK, mean(testknn == TESTSET$font))
}
plot(K,testperfK, type="l")

#(1.3)
#lower values of K give better performance, try [a,b] = [1,5]
K = c(1,2,3,4,5)
testperfK = NULL
for (i in K){ 
  testknn = knn(TRAINSET[,-(1:3)], TESTSET[,-(1:3)], TRAINSET$font, i)
  testperfK = c(testperfK, mean(testknn == TESTSET$font))
}
plot(K,testperfK, type="l") #K=1 gives best performance

#(1.4)
#training set:
train1 = knn(TRAINSET[,-(1:3)], TRAINSET[,-(1:3)], TRAINSET$font, 1)
trainperf1 = mean(train1 == TRAINSET$font)
#test set:
test1 = knn(TRAINSET[,-(1:3)], TESTSET[,-(1:3)], TRAINSET$font, 1)
testperf1 = mean(test1 == TESTSET$font) 

#confusion matrices
trainconf1 = table(TRAINSET$font, train1) #confusion matrix
trainconf1/apply(trainconf1, 1, sum) #in percentages
testconf1 = table(TESTSET$font, test1)
testconf1/apply(testconf1, 1, sum)

#(1.6)
PACK1 = NULL
for(L in 0:9)  for(M in 0:9)  PACK1 = c(PACK1, sprintf('r%ic%i',L,M))

testknnP1 = knn(TRAINSET[,PACK1], TESTSET[,PACK1], TRAINSET$font, 1)
w1 = mean(testknnP1 == TESTSET$font)

#(1.7)
PACK2=NULL; PACK3=NULL; PACK4=NULL;
for(L in 0:9)  for(M in 10:19)  PACK2 = c(PACK2, sprintf('r%ic%i',L,M))
for(L in 10:19)  for(M in 10:19)  PACK3 = c(PACK3, sprintf('r%ic%i',L,M))
for(L in 10:19)  for(M in 0:9)  PACK4 = c(PACK4, sprintf('r%ic%i',L,M))

testknnP2 = knn(TRAINSET[,PACK2], TESTSET[,PACK2], TRAINSET$font, 1)
w2 = mean(testknnP2 == TESTSET$font)

testknnP3 = knn(TRAINSET[,PACK3], TESTSET[,PACK3], TRAINSET$font, 1)
w3 = mean(testknnP3 == TESTSET$font)

testknnP4 = knn(TRAINSET[,PACK4], TESTSET[,PACK4], TRAINSET$font, 1)
w4 = mean(testknnP4 == TESTSET$font)
