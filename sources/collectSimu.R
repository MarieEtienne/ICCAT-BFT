####################################################################################
##                                                                                ##
##  Code to collect simulation results                                            ##
##                                                                                ##
##   Authors: Marie-Pierre Etienne marie.etienne@agroparistech.fr                 ##
##   Date: Jan. 1st,  2014                                                        ##
##   Date: Nov. 2nd,  2014                                                        ##
##                                                                                ##
##                                                                                ##
##  The purpose of this code is to go in each simulation subdirectories           ##                                                                              ##
##              to coellect simutaion results and store them in allSims.Rdata     ##
####################################################################################
main.dir= file.path(Sys.getenv("HOME"), "ICCAT/ICCAT-BFT")

setwd(main.dir)
source(file.path(main.dir,'sources','read.admb.R'))

dn<-dir("simulation",pattern="^[[:digit:]]")
sims <- lapply(1:length(dn),function(d){
			      print(d)
                              setwd(file.path(main.dir,"simulation",dn[d]))
                              if(file.exists('iscam.cor'))
                              {
                                A<-read.rep("iscam.rep")
                                load("simulatedData.Rd")
                                p<-list(Fmsy=A$fmsy,MSY=A$msy,Bmsy=A$bmsy,
                                    Bo = A$bo, 
                                     varphi=A$varphi, simvarphi=simulatedData$varphi,
                                     rho=A$rho, simvrho=simulatedData$varrho,
                                     tau_I=sqrt(A$rho)*A$varphi,  simtau_I=simulatedData$tau_I,
                                     tau_A=A$age_tau2, simtau_A=simulatedData$tau_A,
                                     tau_R=sqrt(1-A$rho)*A$varphi,  simtau_R=simulatedData$tau_R,
                                     q=A$q, simq=simulatedData$q,
                                     h=A$steepness, simh = simulatedData$h,
                                    ro=A$ro, simro = simulatedData$R0,
                                     rinit=A$rinit, 
                                     simrinit=simulatedData$Rinit)
                                }else
                                {
                                  p<- list(Fmsy=NA,MSY=NA,Bmsy=NA,
                                       Bo = NA, 
                                       ro=NA, simro = NA,
                                       varphi=NA, simvarphi=NA,
                                       rho=NA, simvrho=NA,
                                       tau_I=NA,  simtau_I=NA,
                                       tau_A=rep(NA, length(A$age_tau2)), simtau_A=NA,
                                       tau_R=NA,  simtau_R=NA,
                                       q=rep(NA, length(A$q)), simq=NA,
                                       h=NA, simh = NA,
                                       ro=NA, simro = NA,
				       rinit=NA, simrinit=NA) 
                                }  
                              setwd("..")

	return(p)

			      
})
       save(sims,file=file.path(main.dir,"allSims.Rdata"))

