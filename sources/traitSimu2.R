main.dir= setwd(Sys.getenv("HOME"), "ICCAT/ICCAT-BFT")
source(file.path(main.dir,"sources","read.admb.R')
dn<-dir("simulation",pattern="^[[:digit:]]")
sims <- lapply(dn,function(d){setwd(file.path(Sys.getenv(main.dir,"simulation",d)))
                              print(d)
                              if(file.exists('iscam.cor'))
                              {
                                A<-read.rep("iscam.rep")
                                load("simulatedData.Rd")
                                setwd("..")
                                list(Fmsy=A$fmsy,MSY=A$msy,Bmsy=A$bmsy,
                                    Bo = A$bo, 
                                     varphi=A$varphi, simvarphi=simulatedData$varphi,
                                     rho=A$rho, simvrho=simulatedData$varrho,
                                     tau_I=sqrt(A$rho)*A$varphi,  simtau_I=simulatedData$tau_I,
                                     tau_A=A$age_tau2, simtau_A=simulatedData$tau_A,
                                     tau_R=sqrt(1-A$rho)*A$varphi,  simtau_R=simulatedData$tau_R,
                                     q=A$q, simq=simulatedData$q,
                                     h=A$steepness, simh = simulatedData$h,
                                    ro=A$ro, simro = simulatedData$R0)
                                }else
                                {
                                  list(Fmsy=NA,MSY=NA,Bmsy=NA,
                                       Bo = NA, 
                                       ro=NA, simro = NA,
                                       varphi=NA, simvarphi=NA,
                                       rho=NA, simvrho=NA,
                                       tau_I=NA,  simtau_I=NA,
                                       tau_A=rep(NA, length(A$age_tau2)), simtau_A=NA,
                                       tau_R=NA,  simtau_R=NA,
                                       q=rep(NA, length(A$q)), simq=NA,
                                       h=NA, simh = NA,
                                       ro=NA, simro = NA)
                                  
                                }  
}
)
       save(sims,file=file.path("allSims.Rdata"))


pdf(file="R0estimation.pdf")
hist(unlist(lapply(sims, function(d) log(d$ro/d$simro))), xlab="log(R0/simR0)", main="Reestimation")
quantile(unlist(lapply(sims, function(d) (d$ro/d$simro))), na.rm=T, probs=c(0,0.01, 0.05, 0.1, 0.9, 0.95, 0.99,1))

abline(v=0, col=2, lwd=2)
dev.off()

pdf(file="varPhiestimation.pdf")
hist(unlist(lapply(sims, function(d) (d$varphi))), xlab="varphi", main="Reestimation")
abline(v=simulatedData$varphi, col=2, lwd=2)
dev.off()

pdf(file="tau_Iestimation.pdf")
hist(unlist(lapply(sims, function(d) (d$tau_I))), xlab="tau_I", main="Reestimation")
abline(v=simulatedData$tau_I, col=2, lwd=2)
dev.off()

pdf(file="tau_Restimation.pdf")
hist(unlist(lapply(sims, function(d) (d$tau_R))), xlab="tau_R", main="Reestimation")
abline(v=simulatedData$tau_R, col=2, lwd=2)
dev.off()

pdf(file="tau_Aestimation.pdf")
hist(unlist(lapply(sims, function(d) (d$tau_A))), xlab="tau_A", main="Reestimation")
abline(v=simulatedData$tau_A, col=2, lwd=2)
dev.off()

pdf(file="SteepnessEstimation.pdf")
hist(unlist(lapply(sims, function(d) (d$h))), xlab="Steepness", main="Reestimation")
abline(v=simulatedData$h, col=2, lwd=2)
dev.off()


