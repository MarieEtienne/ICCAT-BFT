

library(knitr)
# set global chunk options
opts_chunk$set(fig.path='figure/ICCAT-', fig.align='center', fig.show='hold')
options(replace.assign=TRUE,width=80)
main.dir <- "/home/metienne/ICCAT/ICCAT-BFT"
wdsimple       <- "bfte/2012/vpa/reported/lowSave"
load(file.path(main.dir, 'Report','RDataSave','Info.RData'))
setwd(main.dir)
attach(Info)



RDataFiles<- readLines(file.path(main.dir,'Report', 'RDataSave', 'datafile.out'))



ns <- grep("## SELECTIVITY PARAMETERS Columns for gear", RDataFiles)
cat(RDataFiles[ns])
cat(RDataFiles[ns+1])
cat(RDataFiles[ns+2])



cat(RDataFiles[ns+5])



cat(RDataFiles[ns+3])
cat(RDataFiles[ns+4])



ns <- grep("## Survey timing ", RDataFiles)
cat(RDataFiles[ns])
cat(RDataFiles[ns+1])



survey=list()
gear.list <- unique(iSCAMsurvey$gear)
for( i in 1:nit)
{
    survey[[i]] <- iSCAMsurvey[iSCAMsurvey$gear==gear.list[i],]
    survey[[i]][,2] <- survey[[i]][,2]/ max(survey[[i]][,2])
    if(i == 1 ){
      plot(survey[[i]][,2]~survey[[i]][,1],type="l",xlim=c(1950,2011), ylim=c(0,1.4), lty=gear.list[i], col=gear.list[i], xlab="Year", ylab="Normalised abundance index")
    } else {
      lines(survey[[i]][,2]~survey[[i]][,1],  lty=gear.list[i], col=gear.list[i])
    }
}
legend("topleft", legend=paste("Gear", gear.list), col=gear.list, lty=gear.list)   



CAAReformat <- data.frame(NA, ncol=3, nrow=(nage-sage+1)*(nyr-syr+1))
for( a_ind in sage:nage){
  for(yr_ind in syr:nyr ){
    CAAReformat[(yr_ind-syr)*(nage-sage+1)+a_ind-sage+1, ] <- c(yr_ind, a_ind, CAA[yr_ind-syr+1, a_ind-sage+1])
  }
}
names(CAAReformat)=c("Year", "Age", "Catch")  
radius <- sqrt( CAAReformat$Catch/ pi ) 
with(CAAReformat, 
     symbols(Year, Age, circles=radius, inches=0.35, fg="white", bg="red", ylab="Age", xlab="Year")
     )




gear.list <- unique(compositionCatch[,2])
selectivity=list()
age <- sage:nage
for( i in 1:na_gear)
{
  ind <- which(compositionCatch[,2]==gear.list[i])
  selectivity[[i]] <- compositionCatch[ind,3:(nage-sage+3)]
  selectivity[[i]] <- selectivity[[i]] /  apply(selectivity[[i]], 1, sum)
  selectivity[[i]]<- apply(selectivity[[i]], 2, mean)
  if(i == 1 ){
    plot(selectivity[[i]]~age,type="l",xlim=c(sage, nage), ylim=c(0,1), lty=gear.list[i], col=gear.list[i], xlab="Year", ylab="Average normalised catch at age")
  } else {
    lines(selectivity[[i]]~age,  lty=gear.list[i], col=gear.list[i])
  }
}
legend("topleft", legend=paste("Gear", gear.list), col=gear.list, lty=gear.list)     



age <- sage:nage
par(mfcol=c(3,2))
for( i in 1:ngear)
  {
    ind <- which(compositionCatch[,2]==gear.list[i])
   if(length(ind)>3)
     {
       seltmp<- compositionCatch[ind,]
       seltmp[,3:(3+nage-sage)] <- seltmp[,3:(3+nage-sage)] / apply(seltmp[,3:(3+nage-sage)], 1, sum)
       CAAtmp <- data.frame(NA, ncol=3, nrow=(nage-sage+1)*(nyr-syr+1))
       for( a_ind in sage:nage){
         for(yr_ind in syr:nyr ){
           j= which(seltmp[,1]==yr_ind) 
           if(length(j)>0)
              CAAtmp[(yr_ind-syr)*(nage-sage+1)+a_ind-sage+1, ] <- c(yr_ind, a_ind,  seltmp[j,a_ind-sage+3])
         }
       }
       names(CAAtmp)=c("Year", "Age", "Catch")  
       CAAtmp <- CAAtmp[!is.na(CAAtmp[,1]),]
       radius <- sqrt( CAAtmp$Catch/ pi ) 
       radius <- (radius)/max(radius, na.rm=T)
       with(CAAtmp, 
            symbols(Year, Age, circles=radius, inches=0.1, fg="white", bg=i, ylab="Age", xlab="Year", main =paste("Gear", gear.list[i]))
            )
     }
  }



natM



      cat('linf  = ',  linf,'\n') 
      cat('k  = ',  k,'\n') 
      cat('to  = ',  t0,'\n') 
      cat(' sclw =', sclw,'#1.95e-5 #scaler in length-weight allometry')
      cat('plw = ', plw ,' #power in length-weight allometry')
      cat('m50 = ', m50, '#50% maturity')
      cat('std50 = ', std50, '#std at 50% maturity');



src.dir <- "/home/metienne/ICCAT/ICCAT-BFT/sources"
setwd(src.dir)
source('read.admb.R')
res      <- read.admb(ifile=file.path(main.dir, wdsimple,'ICCAT'))



ns <- grep("## File used as entry", RDataFiles)
cat(RDataFiles[ns+1])
selectivity <- res$log_sel
gear.list=unique(selectivity[,1])
ind <- c(1,which(diff(selectivity[,1])!=0)+1)
selectivity <- selectivity[ind,]
selectivity[,2:ncol(selectivity)] <- exp(selectivity[,2:ncol(selectivity)])/10
ngear <- res$ngear
for( i in 1:ngear)
{
  if(i==1){
    plot(res$age, selectivity[selectivity[,1]==gear.list[i], 2:(nage-sage+2)], "l", col=gear.list[i], lty=gear.list[i], ylim=c(0,1), yla="Selectivity", xlab="Age")
    ind = which(compositionCatch[,2]==gear.list[i])
    if(length(ind)>0)
      points(res$age, apply(compositionCatch[ind,],2, mean)[3:(nage-sage+3)], col=gear.list[i], cex=0.7, pch=19 )
  }
  else{
    lines(res$age, selectivity[selectivity[,1]==gear.list[i], 2:(nage-sage+2)], "l", col=gear.list[i], lty=gear.list[i])
    ind = which(compositionCatch[,2]==gear.list[i])
     if(length(ind)>0)
       points(res$age, apply(compositionCatch[ind,],2, mean)[3:(nage-sage+3)], col=gear.list[i], cex=0.7, pch=19 )
  }
  
}
legend("topleft", legend=paste("Est : Gear ", gear.list), lty=gear.list, col=gear.list)  



par( oma = c( 0, 0, 3, 0 ), mfcol=c(1,1))
split.screen(figs=c(3,2))
ind.scr =1
for(i in 1:ngear){
  screen(ind.scr)
  ind = which(compositionCatch[,2]==gear.list[i])
  if(length(ind)>0){
    ind.scr <- ind.scr +1
    plot(res$age, selectivity[selectivity[,1]==gear.list[i], 2:(nage-sage+2)], "l", col=gear.list[i], lty=gear.list[i], ylim=c(0,1), yla="Selectivity", xlab="Age")
      for(j in ind)
      points(res$age, compositionCatch[j,3:(nage-sage+3)], col=gear.list[i], cex=0.7, pch=19 )
    }
}
mtext("Selectivity at age", outer=TRUE)
close.screen(all=TRUE)



par( oma = c( 2, 2, 0, 0 ), mfcol=c(1,1), mar=c(2, 2, 1, 1))
split.screen(figs=c(3,2))
ind.scr =1
for(i in 1:ngear){
  screen(ind.scr)
  ind = which(compositionCatch[,2]==gear.list[i] & compositionCatch[,1]<=1980)
  if(length(ind)>0){
    if(ind.scr>=5){ x.axt="s"} else{x.axt="n"}
    plot(res$age, selectivity[selectivity[,1]==gear.list[i], 2:(nage-sage+2)], "l", col=gear.list[i], lty=gear.list[i], ylim=c(0,1), 
         ylab="", xlab="",  xaxt=x.axt, yaxt="n")
    if(ind.scr>=5){  print(ind.scr); mtext("Age", side=1, line=2, adj=0.5)}
#    if(!(ind.scr%%2)){  print(ind.scr); mtext("Selectivity", side=2, line=2; adj=1)}
    ind.scr <- ind.scr +1
    for(j in ind)
      points(res$age, compositionCatch[j,3:(nage-sage+3)], col=gear.list[i], cex=0.7, pch=19 )
  }
}
close.screen(all.screens=T)



par( oma = c( 2, 2, 0, 0 ), mfcol=c(1,1), mar=c(2, 2, 1, 1))
split.screen(figs=c(3,2))
ind.scr =1
for(i in 1:ngear){
  screen(ind.scr)
  ind = which(compositionCatch[,2]==gear.list[i] & compositionCatch[,1]>=1980)
  if(length(ind)>0){
    if(ind.scr>=5){ x.axt="s"} else{x.axt="n"}
    plot(res$age, selectivity[selectivity[,1]==gear.list[i], 2:(nage-sage+2)], "l", col=gear.list[i], lty=gear.list[i], ylim=c(0,1), 
         ylab="", xlab="",  xaxt=x.axt, yaxt="n")
    if(ind.scr>=5){  print(ind.scr); mtext("Age", side=1, line=2, adj=0.5)}
#    if(!(ind.scr%%2)){  print(ind.scr); mtext("Selectivity", side=2, line=2; adj=1)}
    ind.scr <- ind.scr +1
    for(j in ind)
      points(res$age, compositionCatch[j,3:(nage-sage+3)], col=gear.list[i], cex=0.7, pch=19 )
  }
}
close.screen(all.screens=T)



#print(res$A)
#print(res$Ahat)
#print(res$A_nu)



print(res$fmsy)
print(res$msy)
print(res$bmsy)
print(res$bo)
print(res$ro)
print(res$q)



print(res$steepness)



plot(x=res$yr, y=res$ft[1,],  xlab="Year",  ylab="Fishing mortality", type="b")



plot(x=res$yrs,y=res$sbt/1000,  xlab="Year", ylab="Spawning biomass (tons)", type="b")



plot(res$Fstatus[1,]~res$Bstatus[1:62],  xlab="B/Bmsy", type="b", ylab="F/Fmsy", xlim=c(0,2), ylim=c(0,2))



detach(Info)



####################################################################################
##                                                                                ##
##   exploits results from retrospectiv analysis                                 ##
##   Authors: Marie-Pierre Etienne marie.etienne@agroparistech.fr                 ##
##   Date: Aug. 2,  2013                                                         ##
##   Date: Aug,  23 2013                                                         ##
##                                                                                ##
##                                                                                ##
####################################################################################

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
des1 <- lapply(retroResults, function(d){
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
des2 <- lapply(retroResults, function(d){
  points(x=nmaxyr-length(d$yr)+1, y=log(d$bmsy), col=2, pch=19)
})  



######################################################################
##   Fmsy variation                                               ##
######################################################################

y.lim=range(sapply(retroResults, function(d) range((d$fmsy)) ), (fullYears$fmsy))
plot(x=0, y=(fullYears$fmsy),  xlim=c(0,nretro), pch=19, col=2,
     ylim=y.lim, xlab="Number of droped Years", ylab="Fmsy ", 
     main="Variation of Fmsy over retrospective analysis")
des3 <- lapply(retroResults, function(d){
  points(x=nmaxyr-length(d$yr)+1, y=(d$fmsy), col=2, pch=19)
})  



