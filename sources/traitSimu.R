####################################################################################
##                                                                                ##
##   extrct informations of Simulations   results                                 ##
##   Authors: Marie-Pierre Etienne marie.etienne@agroparistech.fr                 ##
##   Date: Aug. 17,  2013                                                         ##
##   Date: Aug,  19 2013                                                         ##
##                                                                                ##
##                                                                                ##
####################################################################################

rm(list=ls())

main.dir <- file.path(Sys.getenv("HOME"),"ICCAT/ICCAT-BFT")
res.dir <- file.path(main.dir,"bfte/2012/vpa/inflated/high")
src.dir <- file.path(main.dir,"sources")
report.dir <- file.path(main.dir,"Report")


load(file.path(main.dir, "allSims.Rdata"))
load(file.path(report.dir, "RData", "Info.RData"))
## 

simPar <- readLines(file.path(res.dir,"ICCAT.psc"))
sel.sim <- read.table(file.path(res.dir,"ICCAT.psc"), skip=1, nrows=as.numeric(simPar[1]))

syr <- Info$syr
nyr <- Info$nyr
yr <- syr:nyr

sage <- Info$sage
nage<- Info$nage
age <- sage:nage
ngear <- Info$ngear

nSim <- length(sims)
nClasses <- 20


lro_simro <- unlist(lapply(sims, function(d) log(d$ro/d$simro) ))
p <- quantile(lro_simro, probs=seq(0,1, 1/nClasses) )
hist(lro_simro, main="R0", xlim=range(p), breaks=p)
abline(v=0, col=2)
ind.90 <- which( lro_simro<p[length(p)-1] & lro_simro>p[2])
hist(lro_simro[ind.90], main="R0", xlim=range(lro_simro[ind.90]))
abline(v=0, col=2)

library(ggplot2)
plotRes <- function(nameP, sims, logScale=F, excl =0.1)
{
  prov <- readSimRes(nameP=nameP, namePsim=paste("sim", nameP,sep=""), sims=sims, logScale=logScale)
  qu <- quantile(prov, probs=c(excl/2, 1 - excl/2))
  ind <- which( prov<qu[2] & prov>qu[1])
  prov.df <- data.frame(x=prov[ind])
  p <- ggplot(prov.df)+geom_histogram( aes(x=x))+ geom_vline(aes(xintercept=0, col="red")) + xlab(nameP)
  print(p)
  ggsave(filename=file.path(report.dir, "figure", paste("sim",nameP,".pdf",sep="")), width=10, height=10)
}




plotRes('rinit', sims=sims, logScale=T)
plotRes(nameP='ro', sims=sims, logScale=T)
plotRes('h', sims=sims, logScale=F)




plotRes(nameP='ro', sims, logScale=T)
p <- quantile(lrbar_simrbar, probs=seq(0,1, 1/nClasses) )
hist(lrbar_simrbar, main="R0", xlim=range(p), breaks=p)
abline(v=0, col=2)
ind.90 <- which( lrbar_simrbar<p[length(p)-1] & lrbar_simrbar>p[2])
hist(lrbar_simrbar[ind.90], main="R0", xlim=range(lrbar_simrbar[ind.90]))
abline(v=0, col=2)


hist(log(unlist(lapply(sims, function(d) d$ro))), main="R0")
abline(v=log_R0[1], col=2)

hist(log(unlist(lapply(sims, function(d) d$rbar))), main="Rbar")
abline(v=log_avgrec[1], col=2)

hist((unlist(lapply(sims, function(d) d$sigma))), main="sigma")
abline(v=sigma, col=2)
hist((unlist(lapply(sims, function(d) d$tau))), main="tau")
abline(v=tau, col=2)

hist((unlist(lapply(sims, function(d) d$varphi))), main="varphi")
abline(v=sim_varphi, col=2)

hist((unlist(lapply(sims, function(d) d$rho))), main="rho")
abline(v=sim_rho, col=2)

