####################################################################################
##                                                                                ##
##   file called by setiSCAMFiles.R                                               ##
##   write simulation files                                                       ##
##   Authors: Marie-Pierre Etienne marie.etienne@agroparistech.fr                 ##
##   Date: Aug 17th,  2013                                                         ##
##   Date: Aug. 19th, 2013                                                         ##
##                                                                                ##
##                                                                                ##
####################################################################################


cat("**writing Simulation File\n")

f.out<- file.path(main.dir,out, 'ICCAT.psc' )
print(f.out)
outs <- character(0)
count <- 0
selectivity.table <- readLines(selectivityFile)
selectivity.table<- selectivity.table[c(T,c('SM_TP', 'LL_JP1','NW_PS', 'JP_LL2','SP_BB1', 'SP_BB2', 'SP_BB3')%in%selected.indices)]
selectivity.table <- selectivity.table[selectivity.table !=""]
outs[count<- count+1] <- length(selectivity.table)
outs[(count+1):(count<-count+length(selectivity.table))]<- selectivity.table
outs[count<- count+1] <- "#variance for age composition data"
outs[count<- count+1] <- paste(rep(sim_age_tau^2, length(selectivity.table)), collapse="\t")
outs[count<- count+1] <- "#number of catchability coef nit_sim"
outs[count<- count+1] <- nit
outs[count<- count+1] <-paste(rep(sim_q, nit), collapse="\t")
outs[count<- count+1] <- "##################################################"
outs[count<- count+1] <- "#Value for rho and varphi                       ##"
outs[count<- count+1] <- "#sig=sqrt(rho) * varphi; tau=sqrt(1-rho)*varphi ##"
outs[count<- count+1] <- "##################################################"
outs[count<- count+1] <- paste0(sim_rho, "\t #rho for simulation ")
outs[count<- count+1] <- paste0(sim_varphi, "\t #varphi for simulation ")
outs[count<- count+1] <- "#Value for average mortalite "
outs[count<- count+1] <- mean(natMortality)
outs[count<- count+1] <- "#############################################"
outs[count<- count+1] <- "#Values for  recruitment                    #"
outs[count<- count+1] <- "#############################################"
outs[count<- count+1] <- paste0(log_R0[1],"\t #log recruitment at unfished condition ")
print(paste(log_avgrec[1],'\t#log_Rbar, average recruitment'))
outs[count<- count+1] <- paste(log_avgrec[1],'\t#log_Rbar, average recruitment')
print(paste(log_recinit[1],'\t#log_Rinit, initial recruitment'))
print(outs)
outs[count<- count+1] <- paste(log_recinit[1],'\t#log_Rinit, initial recruitment')
outs[count<- count+1] <- paste(sim_so,'\t#sim_so, initial recruitment')
outs[count<- count+1] <- paste(sim_beta,'\t#sim_beta, initial recruitment')


outs[(count+1):(cout<-count+4)]<- c(
  "## ------------------------------------------------------------------------- ##",
  "## MARKER FOR END OF DATA FILE (eof)                                         ##",
  "## ------------------------------------------------------------------------- ##",
  "999")


con.out = file(description=f.out, open="w")
writeLines(outs, con=con.out,  sep='\n' ) 
close.connection(con.out)


save(list=c("sim_q","std.agecomp","sim_rho", "sim_varphi", "log_R0", "log_avgrec"),
     file=file.path(main.dir,"Report","sim.RData"))