####################################################################################
##                                                                                ##
##  Main R code to launche iSCAM analysis                                         ##
##                                                                                ##
##   Authors: Marie-Pierre Etienne marie.etienne@agroparistech.fr                 ##
##   Date: Aug. 1,  2013                                                          ##
##   Date: Aug. 2,  2013                                                          ##
##                                                                                ##
####################################################################################

#test example Pacific Hake
main.dir <- "/home/metienne/ICCAT/iSCAM/examples/PacificHake/DATA/2010/"
wd <- ''
setwd(file.path(main.dir, wd))
source(file.path(main.dir,'sources','read.admb.R'))
rep <- read.admb(ifile=file.path(main.dir, wd,'PHake2010'))
rep$f


