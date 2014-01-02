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

res.dir <- file.path(Sys.getenv("HOME"),"ICCAT/ICCAT-BFTE","bfte/2012/vpa/reported/low/")
src.dir <- file.path(Sys.getenv("HOME"),"ICCAT/ICCAT-BFTE","sources")
report.dir <- file.path(Sys.getenv("HOME"),"ICCAT/ICCAT-BFTE","Report")


load(file.path(res.dir, "allSims.Rdata"))
load(file.path(report.dir, "sim.RData"))
## 

simPar <- readLines(file.path(res.dir,"ICCAT.psc"))
sel.sim <- read.table(file.path(res.dir,"ICCAT.psc"), skip=1, nrows=as.numeric(simPar[1]))
yr <- sims[[1]]$yr
syr <- yr[1]
nyr <- yr[length(yr)]
age <- sims[[1]]$age
sage <- age[1]
nage<- age[length(age)]

ngear <- sims[[1]]$ngear

#### Selectivity estimation

par(mfcol=c(1,3))
plot(x=sage:nage,y=exp(sel.sim[1,]),xlim=c(0,10), ylim=c(0,3), xlab="Age", ylab="Selectivity", col=1, lwd=2, type="b" )
sel<- lapply(sims,
             function(d) { tmp<- as.matrix(d$log_sel[seq(1,ngear*(nyr-syr+1), (nyr-syr+1)), 2:(nage+1)], 
                                           byrow=T, ncol=nage-sage+1);
                           lines(sage:nage, exp(d$log_sel[1,2:11]), col=2)
                           tmp
             }
             )
plot(x=sage:nage,y=exp(sel.sim[2,]),xlim=c(0,10), ylim=c(0,3), xlab="Age", ylab="Selectivity", col=1, lwd=2, type="b" )
for( i in 1:length(sel))
  lines(x=sage:nage,y=exp(sel[[i]][2,]), col=2)

plot(x=sage:nage,y=exp(sel.sim[3,]),xlim=c(0,10), ylim=c(0,3), xlab="Age", ylab="Selectivity", col=1, lwd=2, type="b" )
for( i in 1:length(sel))
  lines(x=sage:nage,y=exp(sel[[i]][3,]), col=2)

sigma=sqrt(sim_rho)*sim_varphi
tau=sqrt(1-sim_rho)*sim_varphi

hist(log(unlist(lapply(sims, function(d) d$ro))), main="R0")
abline(v=log_R0[1], col=2)

hist(log(unlist(lapply(sims, function(d) d$rbar))), main="Rbar")
abline(v=log_avgrec[1], col=2)

hist((unlist(lapply(sims, function(d) d$sigma))), main="sigma")
abline(v=sigma, col=2)

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

