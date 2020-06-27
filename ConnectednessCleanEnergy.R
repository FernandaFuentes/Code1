
library(frequencyConnectedness)

exampleSim <- read.csv("data.csv",sep=",",header = T);head(exampleSim)


#DESCRIPTIVE STATISTIC

e.des <- function(dato){
  
  ed<-as.numeric(c(mean(dato),sd(dato),min(dato),max(dato),skewness(dato),kurtosis(dato),
        jarque.bera.test(dato)[3], kpss.test(dato)[3],Box.test(dato,lag=5)[3],
        pp.test(dato)[4]))
  ed<-rbind(ed)
  colnames(ed)=colnames=c("mean", "sd", "min", "max","skewness", "kurtosis","Kpss","Jarque-bera","BoxPierce","PP-Unit")
  ed
}


for(i in 1:20){
  ed=e.des(exampleSim[,i])
  print(ed)
  
}


#adf(H0, tiene raiz unitaria)

VARselect(exampleSim)

est <- VAR(exampleSim, p = 1, type = "const") 
sp  <- spilloverDY12(est, n.ahead = 10, no.corr = F);sp

