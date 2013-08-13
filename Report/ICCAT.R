

library(knitr)
# set global chunk options
opts_chunk$set(fig.path='figure/ICCAT-', fig.align='center', fig.show='hold')
options(replace.assign=TRUE,width=90)
main.dir <- "/home/metienne/ICCAT/ICCAT-BFT"
wd       <- "bfte/2012/vpa/simple/low"
load(file.path(main.dir, 'Report','RData','Info.RData'))
setwd(main.dir)
attach(Info)



RDataFiles<- readLines(file.path(main.dir,'Report', 'RData', 'datafile.out'))
ns <- grep('(sage)', RDataFiles)
sage <- as.numeric(unlist(strsplit(RDataFiles[ns], "\t"))[2])
ns <- grep('(nage)', RDataFiles)
nage <- as.numeric(unlist(strsplit(RDataFiles[ns], "\t"))[2])
print(nage)
iSCAMDATA <- readLines(file.path(main.dir,wd, 'ICCAT.dat'))



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



ns <- grep("## ABUNDANCE INDICES", iSCAMDATA)
na_gears <- as.numeric(unlist(strsplit(iSCAMDATA[ns+2],"\t"))[2])
nobs  <- as.numeric(unlist(strsplit(iSCAMDATA[ns+3],"\t|#"))[2:(1+na_gears)])
survey <- list()
ns<- ns+11
compt <- 0
gear.tmp <- rep(NA, na_gears)
for( i in 1:na_gears)
{
    survey[[i]] <- matrix(as.numeric(unlist(strsplit(iSCAMDATA[(ns+compt):(ns+nobs[i]-1+compt)],"\t| "))), ncol=5, byrow=T)
    gear.tmp[i] = survey[[i]][1,3]
    compt <- compt+nobs[i]
    survey[[i]][,2] <- survey[[i]][,2]/ max(survey[[i]][,2])
    if(i == 1 ){
      plot(survey[[i]][,2]~survey[[i]][,1],type="l",xlim=c(1950,2011), ylim=c(0,1.4), lty=gear.tmp[i], col=gear.tmp[i], xlab="Year", ylab="Normalised abundance index")
    } else {
      lines(survey[[i]][,2]~survey[[i]][,1],  lty=gear.tmp[i], col=gear.tmp[i])
    }
}
legend("topleft", legend=paste("Gear", gear.tmp), col=gear.tmp, lty=gear.tmp)   



CAAReformat <- data.frame(NA, ncol=3, nrow=(nage-sage+1)*(nyr-syr+1))
for( a_ind in sage:nage){
  for(yr_ind in syr:nyr ){
    CAAReformat[(yr_ind-syr)*(nage-sage+1)+a_ind-sage+1, ] <- c(yr_ind, a_ind, CAA[yr_ind-syr+1, a_ind-sage+1])
  }
}
names(CAAReformat)=c("Year", "Age", "Catch")  
radius <- sqrt( CAAReformat$Catch/ pi ) 
with(CAAReformat, 
     symbols(Year, Age, circles=radius, inches=0.35, fg="white", bg="red", xlab="Year", ylab="Age")
     )



ns <- grep("## Survey Age comps", iSCAMDATA)
if(length(ns) >0)
  iSCAMDATA <- iSCAMDATA[-ns]
ns <- grep("## AGE COMPOSITION DATA", iSCAMDATA)
na_gearsSel <- as.numeric(unlist(strsplit(iSCAMDATA[ns+2],"\t"))[2])
nobsSel  <- as.numeric(unlist(strsplit(iSCAMDATA[ns+3],"\t|#"))[2:(1+na_gears)])
selectivity <- list()
ns<- ns+8
compt <- 0
gear.tmp <- rep(NA, na_gearsSel)

for( i in 1:na_gearsSel)
{
  tmp.select <- unlist(strsplit(iSCAMDATA[(ns+compt):(ns+nobsSel[i]-1+compt)],"\t| |\t | \t"))
  tmp.select <- tmp.select[tmp.select!=""]
  selectivity[[i]] <- matrix(as.numeric(tmp.select), ncol=nage+2, byrow=T)
  selectivity[[i]][,3:(2+nage)] <- selectivity[[i]][,3:(2+nage)] / 
    apply(selectivity[[i]][,3:(2+nage)], 1, sum)
  selectivity[[i]]<- apply(selectivity[[i]], 2, mean)
  gear.tmp[i] <- selectivity[[i]][2]
  if(i == 1 ){
    plot(selectivity[[i]][3:(nage+2)]~seq(sage,nage,1),type="l",xlim=c(sage, nage), ylim=c(0,1), lty=gear.tmp[i], col=gear.tmp[i], xlab="Year", ylab="Normalised catch at age")
  } else {
    lines(selectivity[[i]][3:(nage+2)]~seq(sage,nage,1),  lty=gear.tmp[i], col=gear.tmp[i])
  }
  compt <- compt+nobsSel[i]
}
legend("topleft", legend=paste("Gear", gear.tmp), col=gear.tmp, lty=gear.tmp)     



for( i in names(popParameters)  )
      cat(i, ' = ',  popParameters[[i]],'\n') 



src.dir <- "/home/metienne/ICCAT/ICCAT-BFT/sources"
setwd(src.dir)
source('read.admb.R')
res      <- read.admb(ifile=file.path(main.dir, wd,'ICCAT'))



ns <- grep("## File used as entry", RDataFiles)
cat(RDataFiles[ns+1])
selectivity <- res$log_sel
ind <- which(apply(selectivity[,2:ncol(res$log_sel)], 1, sum)!=0)
selectivity <- selectivity[ind,]
ind <- c(1,which(diff(selectivity[,1])>0)+1)
selectivity <- selectivity[ind,2:ncol(selectivity)]
selectivity <- exp(selectivity)
selectivity <- selectivity/10
ngear <- res$ngear
for( i in 1:nrow(selectivity))
{
  if(i==1){
    plot(res$age, selectivity[i,], "l", col=i, lty=i, ylim=c(0,1), yla="Selectivity", xlab="Age")
    ind = which(compositionCatch[,2]==i)
    points(res$age, apply(compositionCatch[ind,],2, mean)[3:(nage-sage+3)], col=i, cex=0.7 )
  }
  else{
    lines(res$age, selectivity[i,], "l", col=i, lty=i)
    ind = which(compositionCatch[,2]==i)
    points(res$age, apply(compositionCatch[ind,],2, mean)[3:(nage-sage+3)], col=i, cex=0.7 )
  }
  
}
legend("topleft", legend=paste("Est : Gear ", 1:ngear), lty=1:ngear, col=1:ngear)  



#print(res$A)
#print(res$Ahat)
#print(res$A_nu)



print(res$fmsy)
print(res$ft)
print(res$q)



print(res$bmsy)
print(res$bt)
print(res$bo)



print(res$m)
print(res$steepness)
print(res$F)
print(res$Bstatus)
print(res$Fstatus)


