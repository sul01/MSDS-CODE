#Libraryies and Data sets
library(readr)
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

#1
kclusters = vector(mode = "list", length = 10)
for(k in 1:10){
  kclusters[[k]] = kmeans(DATA[,-(1:3)], k, nstart=20)#(Try nstart=50 if possible)
}

#reduction of variance
perfk = NULL
for(i in 1:10){
  perfk = c(perfk, 1-sum(kclusters[[i]]$withinss)/kclusters[[1]]$withinss)
}
plot(perfk, type='l')

bestk = 7#(Might change)

#2
clusterk = kclusters[[bestk]]
CENT = cluster$center

#PCA
pcaCENT = prcomp(CENT, scale=T)$x[,(1:3)]
#(DISPLAY pcaCENT ON 3D GRAPH)

bigCLU = DATA[as.numeric(names(clusterk$cluster[clusterk$cluster == which(clusterk$size==max(clusterk$size))])),]
pcaBig = prcomp(bigCLU[,-(1:3)])$x[,(1:3)]
#(DISPLAY pcaBIG vectors)

#3
gini = NULL #gini index for [kth] cluster
fBitstream = NULL; fConsolas = NULL; fEbrima = NULL #count of font cases in cluster[k]

for(k in 1:bestk){
  CLU = DATA[as.numeric(names(clusterk$cluster[clusterk$cluster == k])),]
  f1 = nrow(subset(CLU, CLU[,1]=='BITSTREAMVERA'))/nrow(CLU)
  f2 = nrow(subset(CLU, CLU[,1]=='CONSOLAS'))/nrow(CLU)
  f3 = nrow(subset(CLU, CLU[,1]=='EBRIMA'))/nrow(CLU)
  gini = c(gini, f1*(1-f1)+f2*(1-f2)+f3*(1-f3))
  fBitstream = c(fBitstream, f1); fConsolas = c(fConsolas, f2); fEbrima = c(fEbrima, f3)
}

IMP = sum(gini)
FREQ = rbind(fBitstream, fConsolas, fEbrima)
