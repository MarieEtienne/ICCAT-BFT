####################################################################################
##                                                                                ##
##  Main R code to launche iSCAM analysis                                         ##
##                                                                                ##
##   Authors: Marie-Pierre Etienne marie.etienne@agroparistech.fr                 ##
##   Date: Aug. 1,  2013                                                          ##
##   Date: Aug. 1,  2013                                                          ##
##                                                                                ##
####################################################################################
rm(list=ls())
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

setwd('/home/metienne/ICCAT/ICCAT-BFT/CAAModels/iSCAM/sources')
source('setISCAMFiles.R') # create required file for iSCAM


