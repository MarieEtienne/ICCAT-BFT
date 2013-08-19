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


cat("**writing Simulation File\n")
f.out<- file.path(main.dir,out, 'ICCAT.psc' )

outs <- character(0)
count <- 0
selectivity.table <- readLines(selectivityFile)
selectivity.table<- selectivity.table[c(T,c('SM_TP', 'LL_JP1','NW_PS', 'JP_LL2','SP_BB1', 'SP_BB2', 'SP_BB3')%in%selected.indices)]
selectivity.table <- selectivity.table[selectivity.table !=""]
outs[count<- count+1] <- length(selectivity.table)
outs[(count+1):(count<-count+length(selectivity.table))]<- selectivity.table
outs[count<- count+1] <- "#std deviation for age composition data"
outs[count<- count+1] <- paste(rep(0.01, length(selectivity.table), collapse="\t")
outs[(count+1):(cout<-count+4)]<- c(
  "## ------------------------------------------------------------------------- ##",
  "## MARKER FOR END OF DATA FILE (eof)                                         ##",
  "## ------------------------------------------------------------------------- ##",
  "999")


con.out = file(description=f.out, open="w")
writeLines(outs, con=con.out,  sep='\n' ) 
close.connection(con.out)
