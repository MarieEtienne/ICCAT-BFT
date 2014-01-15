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

source(file.path(src.dir,'Utils.R'))
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


library(ggplot2)
library(xtable)
plotRes <- function(nameP, sims, logScale=F, excl =0.1, x.lim=NULL)
{
  prov <- readSimRes(nameP=nameP, sims=sims, logScale=logScale)
  provsim <- readSimRes(nameP=paste('sim',nameP, sep=''), sims=sims, logScale=logScale)
  qu <- quantile(prov, probs=c(excl/2, 1 - excl/2))
  ind <- which( prov<qu[2] & prov>qu[1])
  prov.df <- data.frame(x=prov[ind], xsim=provsim[ind])
  p <- ggplot(prov.df)+geom_histogram( aes(x=x))+ geom_vline(aes(xintercept=xsim, col="red")) + xlab(nameP)
  if(!is.null(x.lim))
    p <- ggplot(prov.df)+geom_histogram( aes(x=x))+ geom_vline(aes(xintercept=xsim, col="red")) + xlab(nameP) + xlim(x.lim)
  
  print(p)
  ggsave(filename=file.path(report.dir, "figure", paste("ICCAT-sim",nameP,".pdf",sep="")), width=10, height=10)
}

sim.res <- data.frame(ro=readSimRes(nameP="ro",  sims=sims))
parToRead <- c("ro", "simro", "rinit", "simrinit", "varphi", "tau_I", "tau_R", "MSY", "Fmsy", "Bmsy" )
lapply(parToRead, function(d) {sim.res[[d]] <<- readSimRes(nameP=d,  sims=sims)})






plotRes('rinit', sims=sims, logScale=T, )

plotRes(nameP='ro', sims=sims, logScale=T)
plotRes('h', sims=sims, logScale=F)
plotRes('tau_R', sims=sims, logScale=F)

sim.res$ro <- log(sim.res$ro)
sim.res$rinit <- log(sim.res$rinit)
sim.res <- sim.res[,-c(2,4)]
sim.res$MSY <- log(sim.res$MSY)
sim.res$Bmsy <- log(sim.res$Bmsy)

t(apply(sim.res, 2, summary ))
xtable((apply(sim.res, 2, quantile, probs=c(0,0.01, 0.025, 0.05,  0.5,  0.95, 0.975, 0.99, 1) )))

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

