#Libraryies and Data sets
library(readr)
library(ggplot2)
library(factoextra)
library(plot3D)
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

#1
kclusters = vector(mode = "list", length = 10)
for(k in 1:10){
  kclusters[[k]] = kmeans(DATA[,-(1:3)], k, nstart=50)#(Try nstart=50 if possible)
}

#reduction of variance
perfk = NULL
for(i in 1:10){
  perfk = c(perfk, 1-sum(kclusters[[i]]$withinss)/kclusters[[1]]$withinss)
}
fviz_nbclust(DATA[,-c(1:3)],kmeans,"silhouette")
bestk=3

#2
clusterk=kclusters[[bestk]]
CENT=clusterk$center
fviz_cluster(clusterk,data=DATA[,-c(1:3)])

#PCA
pcaCENT=prcomp(CENT, scale=T)$x[,(1:3)]
points3D(pcaCENT[,1],pcaCENT[,2],pcaCENT[,3],pch=19)

DATA=cbind(DATA,cluster=clusterk$cluster)
bigCLU=DATA[DATA$cluster==which(clusterk$size==max(clusterk$size)),]
pcaBig=prcomp(bigCLU[,-c(1:3,405)])$x[,(1:3)]
points3D(pcaBig[,1],pcaBig[,2],pcaBig[,3])

#3
fonts = c('BITSTREAMVERA', 'CONSOLAS', 'EBRIMA')
gini=NULL #gini index for [kth] cluster
fBitstream=NULL;fConsolas=NULL;fEbrima=NULL #frequency of font cases in cluster[k]
TOP=NULL 

for(k in 1:bestk){
  CLU=DATA[DATA$cluster==k,]
  f1=nrow(subset(CLU, CLU[,1]=='BITSTREAMVERA'))/nrow(CLU)
  f2=nrow(subset(CLU, CLU[,1]=='CONSOLAS'))/nrow(CLU)
  f3=nrow(subset(CLU, CLU[,1]=='EBRIMA'))/nrow(CLU)
  gini=c(gini,f1*(1-f1)+f2*(1-f2)+f3*(1-f3))
  fBitstream=c(fBitstream,f1);fConsolas=c(fConsolas,f2);fEbrima=c(fEbrima, f3)
  TOP=c(TOP,fonts[which(c(f1,f2,f3) == max(f1,f2,f3))])
  print(TOP)
}

IMP=sum(gini)
FREQ=rbind(fBitstream,fConsolas,fEbrima)

#4
PRED = DATA
PRED$font_pred = vector(mode = "list", length = nrow(DATA))
for(k in 1:bestk){
  PRED[PRED$cluster==k,]$font_pred=TOP[k]
}

#confusion matrices 
conf = table(PRED$font, unlist(PRED$font_pred))
#conf in %'s
conf/apply(conf,1,sum)
