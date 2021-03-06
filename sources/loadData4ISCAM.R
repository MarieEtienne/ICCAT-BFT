
####################################################################################
##                                                                                ##
##  Code to convert vpa data *.d1 in iSCAM data and control file                  ##
##                                                                                ##
##   Authors: Marie-Pierre Etienne marie.etienne@agroparistech.fr                 ##
##   Date: Jul. 25,  2013                                                         ##
##   Date: Aug. 19,  2013                                                          ##
##                                                                                ##
####################################################################################

  selectivityFile<- file.path(main.dir,'selectivityTable.txt')
##################################################################################
##                              DATA iSCAM Parameters
###################################################################################
  selected.indices=c('SM_TP', 'LL_JP1')#,'NW_PS', 'JP_LL2','SP_BB1', 'SP_BB2', 'SP_BB3')
#************************************************************************
#population parameters extracted from 2012 BFT report for eastern stock
#************************************************************************
  linf  <- 319
  k     <- 0.093
  t0    <- -0.97
  sclw  <- 1.95e-5  #scaler in length-weight allometry
  plw   <- 3.009     #power in length-weight allometry
  m50   <- 4     #50% maturity
  std50 <- 0.2*m50  #std at 50% maturity

  natMortality <- c(0.49, 0.24, 0.24, 0.24, .24, 0.20,0.175, 0.15, 0.125, 0.1)

##################################################################################
##                              CTL iSCAM Parameters
###################################################################################
  VERB  <- 0  #1  -verbose ADMB output (0=off, 1=on)')
  REC   <- 1    #2 -recruitment model (1=beverton-holt, 2=ricker)')
  ForgotWeight <- 1 #if 1 weight at age are derived from the given relationship not from the weight at age data
  ###########################################################################
  ### initial value for parameters, (ival), bound : low (lb), up (ub), phase (phz) prior des (prior, p1, p2), 
  ##  Prior descriptions:                                                      ##',
  ##                      -0 uniform      (0,0)                                ##',
  ##                      -1 normal       (p1=mu,p2=sig)                       ##',
  ##                      -2 lognormal    (p1=log(mu),p2=sig)                  ##',
  ##                      -3 beta         (p1=alpha,p2=beta)                   ##',
  ##                      -4 gamma        (p1=alpha,p2=beta)                   ##',
  ## ------------------------------------------------------------------------- ##',
  ## ival         lb      ub      phz     prior   p1      p2      #parameter   ##

  log_R0      <-  c(14.9,  -5.0,    30,    4,    0,    -5.0,   30.)#log_ro/msy 
  h           <-  c(0.92,   0.2,   1.0,    4,    3,       3,     2)       #steepness/fmsy',)
  log_m       <-  c(-1.47,   -5.0,   0.0,    -1,    1,  -1.469,  0.05)    #log.m',
  log_avgrec  <-  c( 14.2,   -5.0,    20,    1,    0,    -5.0,    20)      #log_avgrec',
  log_recinit <-  c( 15,   -5.0,    20,    1,    0,    -5.0,    20)      #log_recinit',
  rho         <-  c(0.14, 0.001, 0.999,    3,    3,    12.0,  52.8)    #rho',
  tau         <-  c(0.8, 0.001,    10,     3,    4,     1e-1, 1e-1)    #kappa (precision)',
#****************************************
# // parameters for bicubic spline
#****************************************
  nodesAge <- 0.5
  selectivityType <- c(3, 1, 1, 6, 1, 3, 13, 13)
  age50sel <- c(6,6,6,9.9,6,6,6,6)
  sd50sel <-  c(1,1,1,0.1,1,1,1,1)


#######################################################################
### READING INPUT VPA format file
#######################################################################
  listFiles <- list.files(file.path(main.dir,f.in))
  f.pot     <- listFiles[which(
    grepl(pattern=".d1", x=listFiles, fixed=TRUE) & 
      (!grepl(pattern=".d1~", x=listFiles, fixed=TRUE)))]
  if(length(f.pot)>1)
  {
    stop(paste('Several *.d1 files found : ', paste(f.pot, collapse='\ '), '\n', sep=''))
  }else
  {
    f.in <- file.path(f.in, f.pot)
  }
    source(file.path(src.dir, 'Utils.R'))
  source(file.path(src.dir, 'parseVPA.R'))
  vpa.dat <- parseVpaData(file.path(main.dir,f.in), selected.indices=selected.indices)
  attach(vpa.dat)

  file.copy(from=file.path(main.dir,'sources/Makefile'),to=file.path(main.dir,dir.out,'Makefile'),overwrite=T)
  
  outr <- character(0) ## for the report file
  countr <- 1
  outr[countr <- countr+1] <- c("## File used as entry                       ##")
  outr[countr <- countr+1] <- f.in
  

  
  source(file.path(src.dir,'writeData4ISCAM.R'))
  source(file.path(src.dir,'writeCTL4iSCAM.R'))
  source(file.path(src.dir,'writePFC4iSCAM.R'))
  source(file.path(src.dir,'writePSC4iSCAM.R'))
  
  cat('****************************************************\n')
  print(outr)
  cat('****************************************************\n')
  
  outf<- character(0)
  outf[1] <- paste0(file.path(main.dir, out,'ICCAT.dat'), '\t# Data File Name' )
  outf[2] <- paste0(file.path(main.dir, out,'ICCAT.ctl'), '\t# Control File Name' )
  outf[3] <- paste0(file.path(main.dir, out,'ICCAT.pfc'), '\t# Projection File Name' )
  outf[4] <- paste0(file.path(main.dir, out,'ICCAT.psc'), '\t# Simualtion File Name' )
  
  con.out = file(description=file.path(main.dir, wd,'RUN.dat'), open="w")
  writeLines(outf, con=con.out,  sep='\n' ) 
  close.connection(con.out)
  
  print(freport.out)
  con.out = file(description=freport.out, open="w")
  writeLines(outr, con=con.out,  sep='\n' ) 
  close.connection(con.out)
  
  Info  <- list(linf = linf, k =k, t0 = t0,
                sclw = sclw, plw = plw,
                m50 = m50,  std50 = std50, 
                syr=syr, nyr=nyr, sage=sage, nage=nage,
                Catch=Catch, ngear=ngear, CAA=CAA,
                survey_type=survey_type, surveyTime=surveyTime,surveySpecification=surveySpecification,
                nit=nit, nit_obs=nit_obs, iSCAMsurvey=iSCAMsurvey,
                compositionCatch=compositionCatch, na_gear=na_gear, na_obs=na_obs,
                waa=waa, natM=natMortality)
  save(Info, file=file.path(main.dir,"Report/RData/Info.RData"))
  detach(vpa.dat)

