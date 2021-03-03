#libraries
library(class)
library(lubridate)
library(tidyverse)

#Data sets
Solar=read_csv("Documents/R/SolarPrediction.csv")

#Formatting
Solar$Duration=difftime(Solar$TimeSunSet,Solar$TimeSunRise,units="mins")
Solar$Duration=as.numeric(Solar$Duration)
Solar$Time=as.numeric(as.period(Solar$Time,units="mins"))
Solar$Data=month(as.Date(Solar$Data,format="%m/%d/%Y %H:%M:%S"))
Solar=Solar[,-c(1,10,11)]
names(Solar)[c(1,7)]=c("Month","Wind_Direction")
names=names(Solar)
features=names(Solar)[]

#Cleaning
Solar=Solar[complete.cases(Solar),]

#Scaling
Solar=as.data.frame(scale(as.matrix(Solar)))

#Sampling
set.seed(1)
Solar=Solar[sample(1:dim(Solar)[1],10000,replace=FALSE),]
attach(Solar)

#DiscrimPower
qf=quantile(Radiation,seq(0,1,0.01))

Low=filter(Solar,Radiation<=qf[24])
Low_mid=filter(Solar,(Radiation>qf[24]&Radiation<=qf[51]))
High_mid=filter(Solar,(Radiation>qf[51]&Radiation<=qf[76]))
High=filter(Solar,Radiation>qf[76])

#histograms
for( i in 1:9){
  print(ggplot(Solar,aes_string(names(Solar)[i]))+
          geom_histogram()+
          facet_wrap(~Class))
}

Low$Class="Low"
Low_mid$Class="Low_mid"
High_mid$Class="High_mid"
High$Class="High"
Class=c(Low$Class,Low_mid$Class,High_mid$Class,High$Class)
Solar$Class=Class
attach(Solar)

#t-test
T1=c();T2=c();T3=c();T4=c();T5=c();T6=c();T7=c();T8=c();T9=c()

LIST=list(rbind(Low,Low_mid),
          rbind(Low,High_mid),
          rbind(Low,High),
          rbind(Low_mid,High_mid),
          rbind(Low_mid,High),
          rbind(High_mid,High))

for(i in 1:6){
  T1[i]=as.numeric(t.test(Month~Class,data=as.data.frame(LIST[i]))[3])
  T2[i]=as.numeric(t.test(Time~Class,data=as.data.frame(LIST[i]))[3])
  T3[i]=as.numeric(t.test(Radiation~Class,data=as.data.frame(LIST[i]))[3])
  T4[i]=as.numeric(t.test(Temperature~Class,data=as.data.frame(LIST[i]))[3])
  T5[i]=as.numeric(t.test(Pressure~Class,data=as.data.frame(LIST[i]))[3])
  T6[i]=as.numeric(t.test(Humidity~Class,data=as.data.frame(LIST[i]))[3])
  T7[i]=as.numeric(t.test(Wind_Direction~Class,data=as.data.frame(LIST[i]))[3])
  T8[i]=as.numeric(t.test(Speed~Class,data=as.data.frame(LIST[i]))[3])
  T9[i]=as.numeric(t.test(Duration~Class,data=as.data.frame(LIST[i]))[3])
}

Pval=as.data.frame(cbind(T1,T2,T4,T5,T6,T7,T8,T8))
names(Pval)=names(Solar)[-c(3,10)]
row.names(Pval)=c("Low V Low_mid",
                  "Low V High_mid",
                  "Low V High",
                  "Low_mid V High_mid",
                  "Low_mid V High",
                  "High_mid V High")
Qval=1-Pval

#train/test
i=1
set.seed(i);Low_train=Low[sort(sample(nrow(Low),nrow(Low)*0.8)),]
set.seed(i);Low_mid_train=Low_mid[sort(sample(nrow(Low),nrow(Low)*0.8)),]
set.seed(i);High_mid_train=High_mid[sort(sample(nrow(Low),nrow(Low)*0.8)),]
set.seed(i);High_train=High[sort(sample(nrow(Low),nrow(Low)*0.8)),]

set.seed(i);Low_test=Low[-sort(sample(nrow(Low),nrow(Low)*0.8)),]
set.seed(i);Low_mid_test=Low_mid[-sort(sample(nrow(Low),nrow(Low)*0.8)),]
set.seed(i);High_mid_test=High_mid[-sort(sample(nrow(Low),nrow(Low)*0.8)),]
set.seed(i);High_test=High[-sort(sample(nrow(Low),nrow(Low)*0.8)),]

TRAIN=rbind(Low_train[,-3],Low_mid_train[,-3],High_mid_train[,-3],High_train[,-3])
TEST=rbind(Low_test[,-3],Low_mid_test[,-3],High_mid_test[,-3],High_test[,-3])
  
#KNN for multi K
K=seq(1,100,1)
TRAIN_perfK=NULL
TEST_perfK=NULL
for (i in K){
  set.seed(1)
  TRAIN_knn=knn(TRAIN[,-9],TRAIN[,-9],TRAIN[,9],i)
  TEST_knn=knn(TRAIN[,-9],TEST[,-9],TRAIN[,9],i)
  TRAIN_perfK=c(TRAIN_perfK,mean(TRAIN[,9]==TRAIN_knn))
  TEST_perfK=c(TEST_perfK,mean(TEST[,9]==TEST_knn))
}

Kval=as.data.frame(cbind(K,TEST_perfK,TRAIN_perfK))

X_lim=seq(0,100,10)
ggplot(Kval)+
  geom_line(aes(K,TEST_perfK),color="red")+
  geom_line(aes(K,TRAIN_perfK),color="blue")+
  scale_x_continuous(breaks=round(X_lim,1))+
  labs(x="K",y="% Correct Classification")

#conf matrix
Kbest=7

set.seed(1);TRAIN_B=knn(TRAIN[,-9],TRAIN[,-9],TRAIN[,9],Kbest)
set.seed(1);TEST_B=knn(TRAIN[,-9],TEST[,-9],TRAIN[,9],Kbest)

TRAIN_Conf=table(TRAIN$Class,TRAIN_B)
TEST_Conf=table(TEST$Class,TEST_B)

TRAIN_Conf=TRAIN_Conf/apply(TRAIN_Conf,1,sum)
TEST_Conf=TEST_Conf/apply(TEST_Conf,1,sum)

#Weight KNN
weights=c()
W_TRAIN=matrix(data=NA,nrow=7476,ncol=8)
W_TEST=matrix(data=NA,nrow=2524,ncol=8)

for(i in 1:8){
  weights[i]=sum(Qval[,i])/8
}
weights=weights/sum(weights)

for(i in 1:8){
  W_TRAIN[,i]=TRAIN[,i]*weights[i]
  W_TEST[,i]=TEST[,i]*weights[i]
}

W_TRAIN=as.data.frame(W_TRAIN)
names(W_TRAIN)=names(TRAIN)[-9]
W_TEST=as.data.frame(W_TEST)
names(W_TEST)=names(TEST)[-9]

#Weighted Conf Matrix
set.seed(1);W_TRAIN_B=knn(W_TRAIN,W_TRAIN,TRAIN[,9],W_Kbest)
set.seed(1);W_TEST_B=knn(W_TRAIN,W_TEST,TRAIN[,9],W_Kbest)

W_TRAIN_Conf=table(TRAIN$Class,W_TRAIN_B)
W_TEST_Conf=table(TEST$Class,W_TEST_B)

W_TRAIN_Conf=W_TRAIN_Conf/apply(W_TRAIN_Conf,1,sum)
W_TEST_Conf=W_TEST_Conf/apply(W_TEST_Conf,1,sum)

#disp
Qval

TRAIN_Conf/4
W_TRAIN_Conf/4
W_TRAIN_Conf/4-TRAIN_Conf/4
sum(W_TRAIN_Conf/4-TRAIN_Conf/4)

TEST_Conf/4
W_TEST_Conf/4
W_TEST_Conf/4-TEST_Conf/4
sum(W_TEST_Conf/4-TEST_Conf/4)
