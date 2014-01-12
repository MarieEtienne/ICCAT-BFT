####################################################################################
##                                                                                ##
##   file called by setiSCAMFiles.R                                               ##
##   convert vpa data *.d1 into iSCAM CPL                                         ##
##   Authors: Marie-Pierre Etienne marie.etienne@agroparistech.fr                 ##
##   Date: Aug 1st,  2013                                                         ##
##   Date: Aug. 1st, 2013                                                         ##
##                                                                                ##
##                                                                                ##
####################################################################################


cat("**writing Projection File\n")
f.out<- file.path(main.dir,out, 'ICCAT.pfc' )

outf <- character(0)

count <- 1
#############################################################
##  Model dimension section                                ##
#############################################################
outf[count:(count<-(count+4))] <- c(
  '## _____________________________ ##',
  '## Projection file control (pfc) ##',
  '## _____________________________ ##',
  '##',
  '## n_tac  length of catch vector.')
TAC <- seq(0,30,5)*1e6
outf[count<- count+1] <- length(TAC)
outf[count<- count+1]<- '## tac vector (mt)'
outf[count<- count+1] <- paste(TAC, collapse="\t")
outf[(count+1):(count<- count+17)] <-c(
  '##',
  '## _____________________________ ##',
  '## Control options               ##',
  '## _____________________________ ##',
  '6    ## Length of control options vector',
  '',
  '1970 ## - 1) Start year for mean natural mortality rate',
  '2011 ## - 2)  Last year for mean natural mortality rate',
  '',
  '1970 ## - 3) Start year for average fecundity/weight-at-age in projections',
  '2011 ## - 4)  Last year for average fecundity/weight-at-age in projections',
  '',
  '1970 ## - 5) Start year for average recruitment period in projections.',
  '2011 ## - 6)   End year for average recruitment period in projections.',
  '',
  '## eof',
  '-999')

con.out = file(description=f.out, open="w")
writeLines(outf, con=con.out,  sep='\n' ) 
close.connection(con.out)
