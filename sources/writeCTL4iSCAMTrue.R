####################################################################################
##                                                                                ##
##   file called by setiSCAMFiles.R                                               ##
##   convert vpa data *.d1 into iSCAM control file                                ##
##   Authors: Marie-Pierre Etienne marie.etienne@agroparistech.fr                 ##
##   Date: Jul. 25,  2013                                                         ##
##   Date: Aug,  1st 2013                                                         ##
##                                                                                ##
##                                                                                ##
####################################################################################

cat("**writing Control File\n")

f.out<- file.path(main.dir,out,'ICCAT.ctl') 



outf <- character(0)
count <- 1
outf[count:(count<-(count+16))] <- c( 
  '## ------------------------------------------------------------------------- ##',
  '## CONTROL FILE TEMPLATE                                                     ##',
  '## ------------------------------------------------------------------------- ##',
  '##',
  '##',
  '## ------------------------------------------------------------------------- ##',
  '## CONTROLS FOR LEADING PARAMETERS                                           ##',
  '##  Prior descriptions:                                                      ##',
  '##                      -0 uniform      (0,0)                                ##',
  '##                      -1 normal       (p1=mu,p2=sig)                       ##',
  '##                      -2 lognormal    (p1=log(mu),p2=sig)                  ##',
  '##                      -3 beta         (p1=alpha,p2=beta)                   ##',
  '##                      -4 gamma        (p1=alpha,p2=beta)                   ##',
  '## ------------------------------------------------------------------------- ##',
  '## npar',
  '7',
  '## ival         lb      ub      phz     prior   p1      p2      #parameter   ##')

outf[count<- count+1] <- paste(paste(log_R0, collapse='\t'),'\t#log_ro/msy ')
outf[count<- count+1] <- paste(paste(h, collapse='\t'),'\t#steepness h ')
outf[count<- count+1] <- paste(paste(log_m, collapse='\t'),'\t#log_m, natural mortality')
outf[count<- count+1] <- paste(paste(log_avgrec, collapse='\t'),'\t#log_Rbar, average recruitment')
outf[count<- count+1] <- paste(paste(log_recinit, collapse='\t'),'\t#log_Rinit, initial recruitment')
outf[count<- count+1] <- paste(paste(rho, collapse='\t'),'\t#proportion of variance for observation process')
outf[count<- count+1] <- paste(paste(tau, collapse='\t'),'\t# total variance')

outf[(count+1):(count<-(count+15))] <- c( 
  '## SELECTIVITY PARAMETERS Columns for gear                                   ##',
  '## OPTIONS FOR SELECTIVITY (isel_type):                                      ##',
  '##      1) logistic selectivity parameters                                   ##',
  '##      2) selectivity coefficients                                          ##',
  '##      3) a constant cubic spline with age-nodes                            ##',
  '##      4) a time varying cubic spline with age-nodes                        ##',
  '##      5) a time varying bicubic spline with age & year nodes.              ##',
  '##      6) fixed logistic (set isel_type=6, and estimation phase to -1)      ##',
  '##      7) logistic function of body weight.                                 ##',
  '##      8) logistic with weight deviations (3 parameters)                    ##',
  '##      11) logistic selectivity with 2 parameters based on mean length      ##',
  '##      12) length-based selectivity coefficients with spline interpolation  ##',
  '##      sig=0.05 0.10 0.15 0.20 0.30 0.40 0.50                               ##',
  '##      wt =200. 50.0 22.2 12.5 5.56 3.12 2.00                               ##',
  '## ------------------------------------------------------------------------- ##')
ns <- grep("# NOW ENTER IN THE VULNERABILITIES", vpaData)+3
nf <-  eosIndices[eosIndices>ns][1]-1 #last line of abundance indices
caasurvey <- unlist(strsplit(vpaData[ns:nf],'\t'))
caasurvey <- as.numeric(caasurvey[seq(1,length(caasurvey), 1+nage-sage+2)])+1
caaData <- c(rep(1, nyr-syr), caasurvey)

outr[(countr+1):(countr<- countr+2)] <- c(  '## SELECTIVITY PARAMETERS Columns for gear                                   ##',
                                         '## OPTIONS FOR SELECTIVITY (isel_type):                                      ##'
                                         )
## Bspline if caa data available, logistic selectivity if not
## test selectivity logistic to try 
#outf[count<- count+1] <- paste(paste(c(3,1,1,1, 6,1,1,1), collapse='\t'), '\t# 1  -selectivity type ivector(isel_type) for gear')
outf[count<- count+1] <- paste(paste(3*(1:ngear %in% caaData)+6*( ! (1:ngear %in% caaData)), collapse='\t'), '\t# 1  -selectivity type ivector(isel_type) for gear')

outr[countr <- countr+1] <- paste(unlist(strsplit(outf[count],"\t")), collapse=" ")
outf[count<- count+1] <- paste(paste(9.9*(  (1:ngear)== 4) + 6*(  (1:ngear)!= 4) , collapse='\t'), 
                               '\t# 2  -Age/length at 50% selectivity (logistic)')
##### test pour selectivite
##outf[count<- count+1] <- paste(paste(c(3, 8, 7, 9.9, 8, 5, 3, 3), collapse="\t"),
#                               '\t# 2  -Age/length at 50% selectivity (logistic)')
outr[countr <- countr+1] <- paste(unlist(strsplit(outf[count],"\t")), collapse=" ")
outf[count<- count+1] <- paste(paste(2*( (1:ngear) != 4)+0.11*(  (1:ngear)==4 ), collapse='\t'),
                               '\t# 3  -STD at 50% selectivity (logistic)')
outr[countr <- countr+1] <- paste(unlist(strsplit(outf[count],"\t")), collapse=" ")
outf[count<- count+1] <- paste(paste(rep(round(nodesAge*nage), ngear)*( (1:ngear) != 4)  , collapse='\t'), '\t# 4  -No. of age nodes for each gear (0=ignore)')
outr[countr<- countr+1] <- paste(unlist(strsplit(outf[count],"\t")), collapse=" ")
outf[count<- count+1] <- paste(paste( rep(0, ngear), collapse='\t'), '\t# 5  -No. of year nodes for 2d spline(0=ignore)')
outf[count<- count+1] <- paste(paste( round( (1:ngear %in%caaData) * 2 + -1*(!nOccurrences(caaData, vect2=1:ngear))  ), collapse='\t'), '\t# 6  -Phase of estimation (-1 for fixed)')
outf[count<- count+1] <- paste(paste( rep(15, ngear), collapse='\t'), '\t# 7  -Penalty wt for 2nd differences w=1/(2*sig^2)')
outf[count<- count+1] <- paste(paste( rep(50, ngear), collapse='\t'), '\t# 8  -Penalty wt for dome-shaped w=1/(2*sig^2)')
outf[count<- count+1] <- paste(paste( rep(1, ngear), collapse='\t'), '\t# 9  -Penalty wt for time-varying selectivity')
outf[count<- count+1] <- paste(paste( rep(1, ngear), collapse='\t'), '\t# 10 -n_sel_blocks (number of selex blocks)')
outf[count<- count+1] <- c('## ------------------------------------------------------------------------- ##')
outf[(count+1):(count<- count+ngear)] <- rep(syr, ngear) 

outf[(count+1):(count<- count+10)] <- c('##',
  '##',
  '##',
  '## ------------------------------------------------------------------------- ##',
  '## PRIORS FOR SURVEY Q                                                       ##',
  '## Prior type:                                                               ##',
  '##			0 - uninformative prior                                         ##',
  '##			1 - normal prior density for log(q)                             ##',
  '##			2 - random walk in q                                            ##',
  '## ------------------------------------------------------------------------- ##')
outf[count<- count+1] <- paste(ngear-1,'\t# -number of surveys (nits)')
outf[count<- count+1] <- paste(paste(rep(0, ngear-1), collapse='\t'),'\t# -prior type (see legend above)')
outf[count<- count+1] <- paste(paste(rep(0, ngear-1), collapse='\t'),'\t# -prior log(mean)')
outf[count<- count+1] <- paste(paste(rep(0, ngear-1), collapse='\t'),'\t# -prior sd')
outf[(count+1):(count<- count+6)] <- c(
  '## ------------------------------------------------------------------------- ##',
  '##',
  '## ------------------------------------------------------------------------- ##',
  '## OTHER MISCELANEOUS CONTROLS                     ##',
  '##',
  '## ------------------------------------------------------------------------- ##')

outf[count<- count+1] <- paste('\t', VERB,'\t# 1  -verbose ADMB output (0=off, 1=on)')
outf[count<- count+1] <- paste('\t', REC,'\t# 2  -recruitment model (1=beverton-holt, 2=ricker)')

CvForCatch1 <- 0.0002
CvForCatch2 <- 0.0001
print(Catch)
outf[count<- count+1] <- paste0('\t',CvForCatch1*mean(Catch, na.rm=T),'\t# 3  -std in observed catches in first phase.')
outf[count<- count+1] <- paste0('\t',CvForCatch2*mean(Catch, na.rm=T),'\t# 4  -std in observed catches in last phase.')

outf[(count+1):(count<- count+11)] <- c(
  '\t0\t# 5  -Assume unfished in first year (0=FALSE, 1=TRUE)',
  '\t0.00\t# 6  -Minimum proportion to consider in age-proportions for dmvlogistic',
  '\t0.30\t# 7  -Mean fishing mortality for regularizing the estimates of Ft',
  '\t0.3\t# 8  -std in mean fishing mortality in first phase',
  '\t2.00\t# 9  -std in mean fishing mortality in last phase',
  '\t-1\t# 10 -phase for estimating m_deviations (use -1 to turn off mdevs)',
  '\t0.1\t# 11 -std in deviations for natural mortality',
  '\t12\t# 12 -number of estimated nodes for deviations in natural mortality',
  '\t0.50\t# 13 -fraction of total mortality that takes place prior to spawning',
  '\t1\t# 14 -switch for age-composition likelihood (1=dmvlogistic,2=dmultinom)',
  '\t0\t# 15 -switch for IFD distribution in selectivity simulations			 ##')
  #####################################################################################',
  ##     END OF FILE MARK                                                            ##',
  #####################################################################################',
  outf[(count+1):(count<- count+5)]<- 
    c('##',
      '## ------------------------------------------------------------------------- ##',
      '## MARKER FOR END OF CONTROL FILE (eofc)                                     ##',
      '## ------------------------------------------------------------------------- ##',
      '999')
  
con.out = file(description=f.out, open="w")
writeLines(outf, con=con.out,  sep='\n' ) 
close.connection(con.out)



