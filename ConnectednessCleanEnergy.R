
library(e1071)  
library(frequencyConnectedness)
library(vars)
library(tseries)  

exampleSim <- read.csv("data.csv",sep=",",header = T);head(exampleSim)

# DESCRIPTIVE STATISTIC
descriptive <- function(dato){
         ed <- as.numeric(c(mean(dato),sd(dato),min(dato),max(dato),skewness(dato),kurtosis(dato),jarque.bera.test(dato)[3], kpss.test(dato)[3],Box.test(dato,lag=10)[3], pp.test(dato)[4]))
         ed <- rbind(ed)
  colnames(ed)=colnames=c("mean", "sd", "min", "max","skewness", "kurtosis","Kpss","Jarque-bera","BoxPierce","PP-Unit");ed
}

for(i in 1:20){ed<-descriptive(exampleSim[,i])
                   cat("stock",i,ed);cat(sep="\n")}


# CONNECTEDNESS
VARselect(exampleSim)
est <- VAR(exampleSim, p = 1, type = "const") 
sp  <- spilloverDY12(est, n.ahead = 10, no.corr = F);sp


# ROLLING 
params_est  = list(p = 1, type = "const")
spR         <- spilloverRollingDY12(exampleSim, params_est  =params_est,func_est="VAR"   , n.ahead = 10, no.corr = F, cluster = NULL, window = 500)

plotFrom(spR)
plotTo(spR)


#IMPULSE-RESPONSE
irf.OVX    <- irf(est,impulse=colnames(exampleSim)[1], response= c(colnames(exampleSim))[c(14,15,16,8,9,10,5,6,7)],n.ahead= 250, ortho= T, cumulative= F,boot= T, seed= 12345)
irf.GZL    <- irf(est,impulse=colnames(exampleSim)[2], response= c(colnames(exampleSim))[c(14,15,16,8,9,10,5,6,7)],n.ahead= 250, ortho= T, cumulative= F,boot= T, seed= 12345)
irf.VIX    <- irf(est,impulse=colnames(exampleSim)[3], response= c(colnames(exampleSim))[c(14,15,16,8,9,10,5,6,7)],n.ahead= 250, ortho= T, cumulative= F,boot= T, seed= 12345)
irf.VSTOXX <- irf(est,impulse=colnames(exampleSim)[4], response= c(colnames(exampleSim))[c(14,15,16,8,9,10,5,6,7)],n.ahead= 250, ortho= T, cumulative= F,boot= T, seed= 12345)

plot(irf.OVX,    ylim=c(-0.1,0.1))
plot(irf.GZL,    ylim=c(-0.1,0.1))
plot(irf.VIX,    ylim=c(-0.1,0.1))
plot(irf.VSTOXX, ylim=c(-0.1,0.1))

