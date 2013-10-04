####################################################################################
##                                                                                ##
##   exploits results from retrospectiv analysis                                 ##
##   Authors: Marie-Pierre Etienne marie.etienne@agroparistech.fr                 ##
##   Date: Aug. 2,  2013                                                         ##
##   Date: Aug,  23 2013                                                         ##
##                                                                                ##
##                                                                                ##
####################################################################################

rm(list=ls())
res.dir <- "/home/metienne/ICCAT/ICCAT-BFT/bfte/2012/vpa/reported/lowSave/"
src.dir <- "/home/metienne/ICCAT/ICCAT-BFT/sources"
report.dir <- "/home/metienne/ICCAT/ICCAT-BFT/Report"

source(file.path(src.dir, "read.admb.R"))
## list the retrospective file available and strores it in retroFilesList.txt
system(paste("ls", res.dir, "| grep ret > retroFilesList.txt")) 

fullYears <- read.rep(file.path(res.dir,"ICCAT.rep"))
             
retroFiles <- readLines("retroFilesList.txt")
nretro     <- length(retroFiles)

retroResults <- lapply(retroFiles, function(d){
                    tmp <- read.rep(file.path(res.dir,d))
                    list(yr=tmp$yr, sbt=tmp$sbt, bmsy=tmp$bmsy,
                         fmsy=tmp$fmsy, R0=tmp$ro)
                    })

######################################################################
##   Spawning biomass graph                                         ##
######################################################################

y.lim=range(sapply(retroResults, function(d) range(log(d$sbt)) ), log(fullYears$sbt))
par(mfcol=c(1,1))
plot(fullYears$yr, log(fullYears$sbt[1:length(fullYears$yr)]), main="Spawning Biomass",
     xlab="Years", ylab="Biomass in numbers", type="b", 
     ylim=y.lim) 
lapply(retroResults, function(d){
  lines(d$yr, log(d$sbt[1:length(d$yr)]), col=2)
})  

######################################################################
##   Bmsy variation                                               ##
######################################################################

y.lim=range(sapply(retroResults, function(d) range(log(d$bmsy)) ), log(fullYears$bmsy))
nmaxyr<- length(fullYears$yr)

plot(x=0, y=log(fullYears$bmsy),  xlim=c(0,nretro), pch=19, col=2,
     ylim=y.lim, xlab="Number of droped Years", ylab="Bmsy (log)", 
     main="Variation of Bmsy over retrospective analysis")
lapply(retroResults, function(d){
  points(x=nmaxyr-length(d$yr)+1, y=log(d$bmsy), col=2, pch=19)
})  

######################################################################
##   Bmsy variation                                               ##
######################################################################

y.lim=range(sapply(retroResults, function(d) range((d$fmsy)) ), (fullYears$fmsy))
plot(x=0, y=(fullYears$fmsy),  xlim=c(0,nretro), pch=19, col=2,
     ylim=y.lim, xlab="Number of droped Years", ylab="Fmsy ", 
     main="Variation of Fmsy over retrospective analysis")
lapply(retroResults, function(d){
  points(x=nmaxyr-length(d$yr)+1, y=(d$fmsy), col=2, pch=19)
})  



#########################################################################
##   MCMC analysis                                                     ##
#########################################################################

mcmc <- read.psv(file.path(res.dir,'iscam.psv'))
res <-  read.rep(file.path(res.dir,'ICCAT.rep'))
res.admb <-  read.admb(file.path(res.dir,'ICCAT'))
res.fit <- read.fit(file.path(res.dir,'ICCAT'))


#########################################################################
##  Table 1 replication                                                 ##
#########################################################################

proj <- read.table(file.path(res.dir, 'ICCAT.proj'), header=T)
year= unique(proj$Year)
nyear <- length(year)
tac = unique(proj$tac)
ntac <- length(tac)
table075 <- matrix(NA, ncol=nyear, nrow=ntac)

  for( j in 1:ntac)
  {
    for( y in 1:nyear)
    {
      #cat(j, " ", y, "\n")
      
    ind <- which(proj$tac==tac[j] & proj$Year==year[y])
    table075[j,y]<- mean(proj[ind,7]<1) #probability to be higher than 0.25 B0
   }
      print(table075[j,])
}


mean(proj[Tac0Year2011,4]>1)
