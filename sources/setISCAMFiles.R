
####################################################################################
##                                                                                ##
##  Code to convert vpa data *.d1 in iSCAM data and control file                  ##
##                                                                                ##
##   Authors: Marie-Pierre Etienne marie.etienne@agroparistech.fr                 ##
##   Date: Jul. 25,  2013                                                         ##
##   Date: Aug. 2,  2013                                                          ##
##                                                                                ##
####################################################################################
#args <- commandArgs(trailingOnly = TRUE)
args=c("","Inputs/bfte/2012/vpa/simple/low/")
print(args)
main.dir <- '/home/metienne/ICCAT/ICCAT-BFT' 
setwd(main.dir)
#data file, in vpa format, with path relative to main directory

#directory where data and ctl files have to written, path relative to main directory
wd <- ''
#directory to find R code
src.dir <- file.path(main.dir,wd,'sources')
freport.out<- file.path(main.dir,'Report','RData','datafile.out') 



##################################################################################
##                              DATA iSCAM Parameters
###################################################################################

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




##################################################################################
##                              CTL iSCAM Parameters
###################################################################################
VERB  <- 0   #1  -verbose ADMB output (0=off, 1=on)')
REC   <- 1    #2 -recruitment model (1=beverton-holt, 2=ricker)')

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

log_R0      <-  c(  3,  -5.0,    15,    4,    0,    -5.0,   15.)#log_ro/msy 
h           <-  c(0.8,   0.2,   1.0,    4,    3,       3,     2)       #steepness/fmsy',)
log_m       <-  c(-2,   -5.0,   0.0,    -1,    1,  -1.469,  0.05)    #log.m',
log_avgrec  <-  c( 6,   -5.0,    30,    1,    0,    -5.0,    30)      #log_avgrec',
log_recinit <-  c( 8,   -5.0,    30,    1,    0,    -5.0,    30)      #log_recinit',
rho         <-  c(0.3, 0.001, 0.999,    3,    3,    12.0,  52.8)    #rho',
tau         <-  c(0.2, 0.001,    10,     3,    4,     1e-4, 1e-4)    #kappa (precision)',
#****************************************
# // parameters for bicubic spline
#****************************************
nodesAge <- 0.5 #one node every two years



##extract directory name
f.in = args[2]
f.in.split <- unlist(strsplit(f.in,'/'))
dir.out <- f.in.split
dir.out <-f.in.split[2:(length(dir.out))]

while(!is.null(dir.out)){
  if(! (dir.out[1] %in% dir(file.path(main.dir,wd), full.names=F, recursive=F))){
    dir.create(file.path(main.dir, wd, dir.out[1]))
  }
  wd <- file.path(wd, dir.out[1])
  if(length(dir.out)>1){
    dir.out <- dir.out[2:length(dir.out)]
  }else
    dir.out=NULL
}
file.copy(from=file.path(main.dir,'sources/Makefile'),to=file.path(main.dir,wd,'Makefile'),overwrite=T)
#######################################################################
### READING INPUT VPA format file
#######################################################################
f.in      <- file.path(main.dir,paste(f.in.split, collapse='/'))
listFiles <- list.files(f.in)
f.pot     <- listFiles[which(
          grepl(pattern=".d1", x=listFiles, fixed=TRUE) & 
          (!grepl(pattern=".d1~", x=listFiles, fixed=TRUE)))]
if(length(f.pot)>1)
{
  stop(paste('Several *.d1 files found : ', paste(f.pot, collapse='\ '), '\n', sep=''))
}else
{
  f.in <- file.path(f.in, f.pot)
  vpaData <- readLines(f.in)
  eosIndices <- grep('-1*[: :]*$',vpaData) ##end of section indices
  ns <- grep('# DATA FILE FOR Continuity', vpaData)
  substring <- unlist(strsplit(vpaData[ns+1]," "))
  substring <- substring[substring!='']
  syr <- strtoi(substring[1])  ### starting year for catch data
  ns <- grep('# NOW ENTER THE CATCH-AT-AGE DATA. ROW=YEAR, COLUMN=AGE', vpaData)+2
  
  nyr <- strtoi(substring[2])  ### last year
  
  
  ns <- grep("# NOW ENTER IN THE ABUNDANCE INDEX SPECIFICATIONS", vpaData)+7
  ##ns+7 = first index abundance
  nf <- which(vpaData=="-1 ")
  nf <-  nf[nf>ns][1]-1 #last line of abundance indices
  ngear <-  nf-ns+2  #ngear number of total gear fisheries + index
  
  ns <- grep('<--AGE', vpaData)
  substring <- unlist(strsplit(vpaData[ns], ' '))
  substring <- strtoi(substring[which(substring%in%paste(seq(1,100), sep=''))])
  sage <-  substring[1]
  nage <- substring[length(substring)]
  
  out<- wd
  
  outr <- character(0) ## for the report file
  countr <- 1
  outr[countr <- countr+1] <- c("## File used as entry                       ##")
  outr[countr <- countr+1] <- f.in
  

  
  source(file.path(src.dir, 'Utils.R'))
  source(file.path(src.dir,'writeData4ISCAM.R'))
  source(file.path(src.dir,'writeCTL4iSCAM.R'))
  source(file.path(src.dir,'writePFC4iSCAM.R'))
  
  cat('****************************************************\n')
  print(outr)
  cat('****************************************************\n')
  
  outf<- character(0)
  outf[1] <- paste0(file.path(main.dir, out,'ICCAT.dat'), '\t# Data File Name' )
  outf[2] <- paste0(file.path(main.dir, out,'ICCAT.ctl'), '\t# Control File Name' )
  outf[3] <- paste0(file.path(main.dir, out,'ICCAT.pfc'), '\t# Projection File Name' )
  
  con.out = file(description=file.path(main.dir, wd,'RUN.dat'), open="w")
  writeLines(outf, con=con.out,  sep='\n' ) 
  close.connection(con.out)
  
  print(freport.out)
  con.out = file(description=freport.out, open="w")
  writeLines(outr, con=con.out,  sep='\n' ) 
  close.connection(con.out)
  
  Info  <- list(linf = linf, k =k, t0 = t0,
                         sclw = sclw, plw = plw,
                         m50 = m50,  std50 = std50, CAA=CAA, compositionCatch=tot_catch,
                        nage=nage, sage=sage, syr=syr, nyr=nyr)
  save(Info, file=file.path(main.dir,"Report/RData/Info.RData"))
  
}  
