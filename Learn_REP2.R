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

#confusion matrices (used in later parts)
trainconf12 = table(TRAINSET$font, train12) #confusion matrix
trainconf12/apply(trainconf, 1, sum) #in percentages
testconf12 = table(TESTSET$font, test12)
testconf12/apply(testconf12, 1, sum)

#(1.2) 
#takes a while to run
K = c(5,10,15,20,30,40,50,100)
testperfK = NULL
for (i in K){ 
  testknn = knn(TRAINSET[,-(1:3)], TESTSET[,-(1:3)], TRAINSET$font, i)
  testperfK = c(testperfK, mean(testknn == TESTSET$font))
}
plot(K,testperfK, type="l")