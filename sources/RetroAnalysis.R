####################################################################################
##                                                                                ##
##   exploits results from retrospectiv analysis                                 ##
##   Authors: Marie-Pierre Etienne marie.etienne@agroparistech.fr                 ##
##   Date: Aug. 2,  2013                                                         ##
##   Date: Jan,  4 2014                                                         ##
##                                                                                ##
##                                                                                ##
####################################################################################

library(ggplot2)
res.dir <- file.path(Sys.getenv("HOME"),"ICCAT/ICCAT-BFT", "bfte/2012/vpa/reported/high/")
src.dir <- file.path(Sys.getenv("HOME"),"ICCAT/ICCAT-BFT", "sources")
report.dir <- file.path(Sys.getenv("HOME"),"ICCAT/ICCAT-BFT", "Report")

source(file.path(src.dir, "read.admb.R"))
## list the retrospective file available and strores it in retroFilesList.txt
system(paste("ls", res.dir, "| grep ICCAT.ret > retroFilesList.txt")) 

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

lYear<- sort(tail(fullYears$yr,length(retroFiles)), decreasing=T)
ndf <- data.frame(x=fullYears$yr, y=log(fullYears$sbt[1:length(fullYears$yr)]))


palette(color <- c('#ece7f2', '#d0d1e6', '#a6bddb', '#74a9cf', '#3690c0', '#0570b0','#045a8d', '#023858'))
                  
a <- ggplot() + geom_line(ndf, mapping=(aes(x=x, y=y)), color=1)+ 
  xlab("Years") + ylab("Biomass  (log)") + ggtitle("Spawning Biomass") + ylim(c(18.5, 21))
ind <- 1
lapply(retroResults, function(d){
  #a data frame for predictors and response
  ndf <- data.frame(x=d$yr, y =log(d$sbt[1:length(d$yr)]) )
  ind <<- ind + 1
  a<<- a+geom_line(ndf, mapping=(aes(x=x, y=y)), color = ind)
} )
ggsave (filename=file.path(report.dir, "figure", "ICCAT-ReportedSBTRetro.pdf"), units="cm", width=14, height=8) 

######################################################################
##   Bmsy variation                                               ##
######################################################################

a <- ggplot() + geom_line(ndf, mapping=(aes(x=x, y=y)), color=1)+ 
  xlab("Years") + ylab("Biomass in numbers (log)") + ggtitle("Spawning Biomass")
ind <- 1
lapply(retroResults, function(d){
  #a data frame for predictors and response
  ndf <- data.frame(x=d$yr, y =log(d$sbt[1:length(d$yr)]) )
  ind <<- ind + 1
  a<<- a+geom_line(ndf, mapping=(aes(x=x, y=y)), color = ind)
} )
ggsave (filename=file.path(report.dir, "figure", "ICCAT-SBTRetro.pdf"), units="cm", width=14, height=8)
  
  
  
  ######################################################################
  ##  Bmsy  variation                                               ##
  ######################################################################
  
  nmaxyr<- tail(fullYears$yr,1)
  retro.bmsy <- data.frame(x=2011, y=log(fullYears$bmsy))
  lapply(retroResults, function(d){
  retro.bmsy <<- rbind(retro.bmsy, data.frame(x=tail(d$yr,1), y=log(d$bmsy)) )
})  
retro.bmsy$x <- as.integer(retro.bmsy$x)
ggplot(data=retro.bmsy)+geom_line(aes(x=x, y=y))+ylim(range(c(18.5, 19))) + 
  ylab("Bmsy ") + xlab("Final Year of retro analysis") + 
  scale_x_continuous(breaks = retro.bmsy$x[seq(1, length(retro.bmsy$x), 2)])   
ggsave (filename=file.path(report.dir, "figure", "ICCAT-BmsyEvolRetro.pdf"), units="cm", width=14, height=8)



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


### Probleme dans les sorties
proj <- read.table(file.path(res.dir, 'ICCAT.proj'), header=T)
year= unique(proj$Year)
nyear <- length(year)
tac = unique(proj$tac)
ntac <- length(tac)
table075 <- matrix(NA, ncol=nyear, nrow=ntac)

  for( j in 1:ntac)
  {
    cat(j,  "\n")
    for( y in 1:nyear)
    {
      
    ind <- which(proj$tac==tac[j] & proj$Year==year[y])
    table075[j,y]<- mean(proj[ind,7]<1) #probability to be higher than 0.25 B0
   }
      print(table075[j,])
}


mean(proj[Tac0Year2011,4]>1)
