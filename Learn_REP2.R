#Libraryies and Data sets
library(readr)
VLADIMIR=read_csv("Documents/R/fonts/VLADIMIR.csv")
EBRIMA=read_csv("Documents/R/fonts/EBRIMA.csv")
BITSTREAMVERA=read_csv("Documents/R/fonts/BITSTREAMVERA.csv")

#Cleaning and sorting
drop_names=c("fontVariant","m_label","orientation","m_top","m_left","originalH","originalW","h","w")
CL1=subset(VLADIMIR[complete.cases(VLADIMIR),-which(names(VLADIMIR) %in% drop_names)],strength==0.4&italic==0)
CL2=subset(EBRIMA[complete.cases(EBRIMA),-which(names(EBRIMA) %in% drop_names)],strength==0.4&italic==0)
CL3=subset(BITSTREAMVERA[complete.cases(BITSTREAMVERA),-which(names(BITSTREAMVERA) %in% drop_names)],strength==0.4&italic==0)
DATA=rbind(CL1,CL2,CL3)
