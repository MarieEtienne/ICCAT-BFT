####################################################################################
##                                                                                ##
##   file called by setiSCAMFiles.R                                               ##
##   convert vpa data *.d1 into iSCAM control file                                ##
##   Authors: Marie-Pierre Etienne marie.etienne@agroparistech.fr                 ##
##   Date: Jul. 25,  2013                                                         ##
##   Date: Aug. 28,  2012                                                         ##
##                                                                                ##
##                                                                                ##
####################################################################################


f.out<- file.path(main.dir,out,'ICCAT.ctl') 



outf <- character(0)
count <- 1
outf[count:(count<-(count+42))] <- c( 
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
  '## ival         lb      ub      phz     prior   p1      p2      #parameter   ##',
  '1.6         -5.0    15       4       0       -5.0    15.     #log_ro/msy ',
  '0.65        0.2     1.0      4       3       3       2       #steepness/fmsy',
  '-1.469      -5.0    0.0      2       1       -1.469  0.05    #log.m',
  '1.6         -5.0    15       1       0       -5.0    15      #log_avgrec',
  '1.60        -5.0    15       1     0         -5.0    15      #log_recinit',
  '0.2         0.001   0.999    3       3       12.0    52.8    #rho',
  '1.25        0.01    10.      3       4       39.0625 62.5    #kappa (precision)',
  '## ------------------------------------------------------------------------- ##',
  '##',
  '##',
  '## ------------------------------------------------------------------------- ##',
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

## Bspline if caa data available, logistic selectivity if not
outf[count<- count+1] <- paste(paste(5*(1:ngear %in% caaData)+6*( ! (1:ngear %in% caaData)), collapse='\t'), '\t# 1  -selectivity type ivector(isel_type) for gear')
outf[count<- count+1] <- paste(paste(rep(3.5, ngear), collapse='\t'), '\t# 2  -Age/length at 50% selectivity (logistic)')
outf[count<- count+1] <- paste(paste(rep(1, ngear), collapse='\t'), '\t# 3  -STD at 50% selectivity (logistic)')
outf[count<- count+1] <- paste(paste(rep(round(nodesAge*nage), ngear), collapse='\t'), '\t# 4  -No. of age nodes for each gear (0=ignore)')
outf[count<- count+1] <- paste(paste( round( nOccurrences(caaData, vect2=1:ngear) * nodesYear), collapse='\t'), '\t# 5  -No. of year nodes for 2d spline(0=ignore)')
outf[count<- count+1] <- paste(paste( round( (1:ngear %in%caaData) * 2 + -1*(!nOccurrences(caaData, vect2=1:ngear))  ), collapse='\t'), '\t# 6  -Phase of estimation (-1 for fixed)')
outf[count<- count+1] <- paste(paste( rep(15, ngear), collapse='\t'), '\t# 7  -Penalty wt for 2nd differences w=1/(2*sig^2)')
outf[count<- count+1] <- paste(paste( rep(50, ngear), collapse='\t'), '\t# 8  -Penalty wt for dome-shaped w=1/(2*sig^2)')
outf[count<- count+1] <- paste(paste( rep(1, ngear), collapse='\t'), '\t# 9  -Penalty wt for time-varying selectivity')
outf[count<- count+1] <- paste(paste( rep(1, ngear), collapse='\t'), '\t# 10 -n_sel_blocks (number of selex blocks)')
outf[count<- count+1] <- c('## ------------------------------------------------------------------------- ##')
outf[(count+1):(count<- count+ngear)] <- rep(1977, ngear) 

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
outf[(count+1):(count<- count+13)] <- c(
  '\t0.100\t# 3  -std in observed catches in first phase.',
  '\t0.0707\t# 4  -std in observed catches in last phase.',
  '\t0\t# 5  -Assume unfished in first year (0=FALSE, 1=TRUE)',
  '\t0.00\t# 6  -Minimum proportion to consider in age-proportions for dmvlogistic',
  '\t0.20\t# 7  -Mean fishing mortality for regularizing the estimates of Ft',
  '\t0.01\t# 8  -std in mean fishing mortality in first phase',
  '\t2.00\t# 9  -std in mean fishing mortality in last phase',
  '\t-3\t# 10 -phase for estimating m_deviations (use -1 to turn off mdevs)',
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

