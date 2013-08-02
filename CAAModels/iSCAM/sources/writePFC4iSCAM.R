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


f.out<- file.path(main.dir,out, 'ICCAT.pfc' )

outf <- character(0)

count <- 1
#############################################################
##  Model dimension section                                ##
#############################################################
outf[count:(count<-(count+24))] <- c(
  '## _____________________________ ##',
  '## Projection file control (pfc) ##',
  '## _____________________________ ##',
  '##',
  '## n_tac  length of catch vector.',
  '6',
  '## tac vector (mt)',
  '0.00 0.01 0.02 0.03 0.04 0.05',
  '##',
  '## _____________________________ ##',
  '## Control options               ##',
  '## _____________________________ ##',
  '6    ## Length of control options vector',
  '',
  '1966 ## - 1) Start year for mean natural mortality rate',
  '2010 ## - 2)  Last year for mean natural mortality rate',
  '',
  '1966 ## - 3) Start year for average fecundity/weight-at-age in projections',
  '2010 ## - 4)  Last year for average fecundity/weight-at-age in projections',
  '',
  '1966 ## - 5) Start year for average recruitment period in projections.',
  '2010 ## - 6)   End year for average recruitment period in projections.',
  '',
  '## eof',
  '-999')

con.out = file(description=f.out, open="w")
writeLines(outf, con=con.out,  sep='\n' ) 
close.connection(con.out)