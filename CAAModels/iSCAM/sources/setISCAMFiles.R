
####################################################################################
##                                                                                ##
##  Main file to convert vpa data *.d1 in iSCAM data and control file             ##
##                                                                                ##
##   Authors: Marie-Pierre Etienne marie.etienne@agroparistech.fr                 ##
##   Date: Jul. 25,  2013                                                         ##
##   Date: Aug. 28,  2012                                                         ##
##                                                                                ##
####################################################################################
rm(list=ls())
setwd('/home/metienne/ICCAT/ICCAT-BFT/CAAModels/iSCAM/sources')
main.dir <- '/home/metienne/ICCAT/ICCAT-BFT' 
#data file, in vpa format, path relative to main directory
f.in <- 'Inputs/bfte/2012/vpa/inflated/low/bfte2012.d1'

#directory where data and ctl files have to written, path relative to main directory
wd <- 'CAAModels/iSCAM'



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
std50 <- 0.1*m50  #std at 50% maturity


##################################################################################
##                              CTL iSCAM Parameters
###################################################################################
VERB  <- 0    #1  -verbose ADMB output (0=off, 1=on)')
REC   <- 1    #2 -recruitment model (1=beverton-holt, 2=ricker)')

#****************************************
# // parameters for bicubic spline
#****************************************
nodesAge <- 0.5 #one node every two years
nodesYear <- 0.33 #one node every 3 years

##extract directory name
f.in.split <- unlist(strsplit(f.in,'/'))
dir.out <- f.in.split[(which(f.in.split=='Inputs')+1):(length(f.in.split)-1)]
dir.out <- dir.out[dir.out != 'vpa']
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

#######################################################################
### READING INPUT VPA format file
#######################################################################
f.in <- file.path(main.dir,'Inputs/bfte/2012/vpa/inflated/low/bfte2012.d1')
vpaData <- readLines(f.in)
eosIndices <- grep('-1*[: :]*$',vpaData) ##end of section indices
ns <- grep('# DATA FILE FOR Continuity', vpaData)
substring <- unlist(strsplit(vpaData[ns+1]," "))
substring <- substring[substring!='']
syr <- strtoi(substring[1])  ### starting year for catch data
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
source('Utils.R')
source('writeData4ISCAM.R')
source('writeCTL4iSCAM.R')

outf<- character(0)
outf[1] <- paste0(file.path(main.dir, out,'ICCAT.dat'), '\t# Data File Name' )
outf[2] <- paste0(file.path(main.dir, out,'ICCAT.ctl'), '\t# Control File Name' )
outf[3] <- paste0(file.path(main.dir, out,'ICCAT.pfc'), '\t# Projection File Name' )

con.out = file(description=file.path(main.dir, wd,'RUN.dat'), open="w")
writeLines(outf, con=con.out,  sep='\n' ) 
close.connection(con.out)