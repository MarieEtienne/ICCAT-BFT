####################################################################################
##                                                                                ##
##   Post treatment on MLE results obtained using iscam on bfte data              ##
##   Authors: Marie-Pierre Etienne marie.etienne@agroparistech.fr                 ##
##   Date: Aug. 10,  2013                                                         ##
##   Date: Jan,  11 2014                                                          ##
##                                                                                ##
##                                                                                ##
####################################################################################

main.dir= file.path(Sys.getenv("HOME"), "ICCAT/ICCAT-BFT")
## rep where to find mcmc outputs
rep<-unlist(lapply(c(file.path('bfte/2012/vpa','inflated'),file.path('bfte/2012/vpa','reported')),
                   function(d) {file.path(d, c("high-R0", "high-Rinit"))}))
repNames <- lapply(strsplit(rep, "/"), function(d) {paste(d[4], d[5], sep="")})

palette(c("black", "red", "green3", "blue4", "maroon4", "magenta", "orangered", 
          "gray"))

outdir <- file.path(main.dir, "Report/figure")
load(file.path(main.dir, 'Report','RData','Info.RData'))
setwd(main.dir)

iSCAMR <- dir("../iSCAM/src/r-code/R/")
for(f in iSCAMR)
 source(paste("../iSCAM/src/r-code/R/", f, sep=""), echo=T)
attach(Info)
RDataFiles<- readLines(file.path(main.dir,'Report', 'RDataSave', 'datafile.out'))


nFiles=length(rep)
res      <- lapply(rep, function(d){read.admb(ifile=file.path(d,'ICCAT'))})
## reading bug in abundance index 
##last index is split on 2 lines 




survey=list()
gear.names <- Info$surveySpecification[,8]
pdf(file="ICCAT-Abundance.pdf", width=11, width=12, height=8 )
for( i in 1:nit)
{
    survey[[i]] <- iSCAMsurvey[iSCAMsurvey$gear==gear.list[i],]
    survey[[i]][,2] <- survey[[i]][,2]/ max(survey[[i]][,2])
    if(i == 1 ){
      plot(survey[[i]][,2]~survey[[i]][,1],type="l",xlim=c(1945,2011), ylim=c(0,1.6), lty=gear.list[i], col=gear.list[i], xlab="Year", ylab="Normalised abundance index")
    } else {
      lines(survey[[i]][,2]~survey[[i]][,1],  lty=gear.list[i], col=gear.list[i])
    }
}
legend("topleft", legend=paste( Info$surveyName), cex=0.9,col=gear.list, lty=gear.list, )   
dev.off()


CAAReformat <- data.frame(NA, ncol=3, nrow=(nage-sage+1)*(nyr-syr+1))
for( a_ind in sage:nage){
  for(yr_ind in syr:nyr ){
    CAAReformat[(yr_ind-syr)*(nage-sage+1)+a_ind-sage+1, ] <- c(yr_ind, a_ind, CAA[yr_ind-syr+1, a_ind-sage+1])
  }
}


pdf(file="ICCAT-Catch.pdf", width=10, height=8)
names(CAAReformat)=c("Year", "Age", "Catch")  
radius <- sqrt( CAAReformat$Catch/ pi ) 
with(CAAReformat, 
     symbols(Year, Age, circles=radius, inches=0.35, fg="white", bg="red", ylab="Age", xlab="Year")
     )
dev.off()


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
legend("topleft", legend=Info$surveyName, col=gear.list, lty=gear.list)     


ng <- ngear[[1]]
name.list=c("Comm", Info$surveyName)
age <- sage:nage
pdf(file=file.path(outdir,"AgeComposition.pdf"), onefile=T, width=12)
par(mfcol=c(2,2))
for( i in 1:ng)
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
            symbols(Year, Age, circles=radius, inches=0.1, fg="white", bg=i, ylab="Age", xlab="Year", main =paste("Gear", name.list[gear.list[i]]))
            )
     }
  }
dev.off()








selectivity <- lapply(res, function(d) {d$log_sel})
gear.list=lapply(selectivity, function(d) {unique(d[,1])})
selectivity <- lapply(selectivity, function(d) { d[ c(1,which(diff(d[,1])!=0)+1), ]})
selectivity <- lapply(selectivity, function(d) { 
  r <- cbind(d[,1],exp(d[,2:ncol(d)])/10)
  return(r)
})
ngear <- lapply(res, function(d) {d$ngear})

l<-1
lapply(selectivity, function(d)
{
  ng<- nrow(d)
  #pdf(file=file.path(outdir, paste("ICCAT-Selectivity-", repNames[i]",.pdf", sep="")), width=10, paper="a4r")
  for( i in 1:ng)
  {
    if(i==1){
      plot(sage:nage, d[d[,1]==gear.list[[l]][i], 2:(nage-sage+2)], "l", col=gear.list[[l]][i],
           lty=gear.list[[l]][i], ylim=c(0,1), yla="Selectivity", xlab="Age")
      ind = which(compositionCatch[,2]==gear.list[[l]][i])
      if(length(ind)>0)
        points(sage:nage, apply(compositionCatch[ind,],2, mean)[3:(nage-sage+3)], 
               col=gear.list[[l]][i], cex=0.7, pch=19 )
  }
  else{
    lines(sage:nage, d[d[,1]==gear.list[[l]][i], 2:(nage-sage+2)], "l",
          col=gear.list[[l]][i], lty=gear.list[[l]][i])
    ind = which(compositionCatch[,2]==gear.list[[l]][i])
     if(length(ind)>0)
       points(sage:nage, apply(compositionCatch[ind,],2, mean)[3:(nage-sage+3)], 
              col=gear.list[[l]][i], cex=0.7, pch=19 )
  }
  }
legend("topleft", legend=paste(Info$surveyName), lty=gear.list[[l]], col=gear.list[[l]])  
l<<-l+1
  })

dev.off()



library(grid)
library(ggplot2)
i <- 1
lapply(res, function(d){
  nyr <- length(d$yr)
  df <- data.frame(Fstatus=d$Fstatus[1,], Bstatus =  d$Bstatus[1:nyr], Year=d$yr)
  p<- ggplot()  + xlab("SpawningBiomass / Bmsy") + ylab ("F/Fmsy") + ylim(c(0,2.025)) + 
    xlim(c(0,5.5)) +  geom_path(data=df, aes(y=Fstatus, x=Bstatus, col=Year), arrow=arrow(type="open", length = unit(0.1, "inches")   )) +
    geom_vline(aes(xintercept=1)) + geom_hline(aes(yintercept=1))
print(p)
ggsave(filename=file.path(outdir,paste("ICCAT-KobePlot", repNames[i],".pdf", sep="")), width=14, units="cm", height=10)
  i  <<- i+1
}
)

pdf(file=file.path(outdir,"ICCAT-SelectivityByGear.pdf"), width=10, heigh=14)
par( oma = c( 0, 0, 3, 0 ), mfcol=c(1,1))
split.screen(figs=c(3,2))
ind.scr =1
for(i in 1:ng){
  screen(ind.scr)
  ind = which(compositionCatch[,2]==gear.list[i])
  if(length(ind)>0){
    ind.scr <- ind.scr +1
    plot(res$age, selectivity[selectivity[,1]==gear.list[i], 2:(nage-sage+2)], "l", col=gear.list[i], lty=gear.list[i], ylim=c(0,1), yla="Selectivity", xlab="Age", main=name.list[gear.list[i]])
      for(j in ind)
      points(res$age, compositionCatch[j,3:(nage-sage+3)], col=gear.list[i], cex=0.7, pch=19 )
    }
}
mtext("Selectivity at age", outer=TRUE)
close.screen(all=TRUE)
dev.off()


par( oma = c( 2, 2, 0, 0 ), mfcol=c(1,1), mar=c(2, 2, 1, 1))
split.screen(figs=c(3,2))
ind.scr =1
for(i in 1:ng){
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
for(i in 1:ng){
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



### collect results for all scenarios

resTable <- matrix(NA, ncol=nFiles, nrow=7)
resTable[1,] <-unlist(lapply(res, function(d) {(log(d$ro))}))
resTable[2,] <-unlist(lapply(res, function(d) {((d$steepness))}))
resTable[3,] <-unlist(lapply(res, function(d) {(d$fmsy)}))
resTable[4,] <-unlist(lapply(res, function(d) {log(d$msy)}))
resTable[5,] <-unlist(lapply(res, function(d) {log(d$bmsy)}))
resTable[6,] <-unlist(lapply(res, function(d) {(log(d$Bstatus[length(d$yr)]))}))
resTable[7,] <-unlist(lapply(res, function(d) {((d$Fstatus[1,length(d$yr)]))}))
resTable <-cbind(Name=c("logRO", "h", "fmsy", "msy", "bmsy", "Bstatus", "Fstatus"),as.data.frame(resTable))
colnames(resTable)[2:(nFiles+1)]=repNames
xtable(resTable, digits=4)



i <- 1
p<- ggplot()  + xlab("Years") + ylab ("recruits") + ylim(c(0,6.1e6)) 
lapply(res, function(d){
  nyr <- length(d$yrs)
  df <- data.frame(rt=d$rt[1:(nyr-4)], yr=d$yrs[1:(nyr-4)])
  p <<-   p +geom_line(data=df, aes(y=rt, x=yr), col=i) 
  i  <<- i+1
}
       )
print(p+ scale_color_manual(labels=unlist(namesRep),  values=1:nFiles))
ggsave(filename=file.path(outdir,paste("Recruits.pdf", sep="")), width=14, units="cm", height=10)


i <- 1
p<- ggplot()  + xlab("Years") + ylab ("Spawning biomass") + ylim(c(0,6e8)) 
lapply(res, function(d){
  nyr <- length(d$yrs)
  df <- data.frame(sbt=d$sbt[1:(nyr)], yr=d$yrs[1:(nyr)])
  p <<-   p +geom_line(data=df, aes(y=sbt, x=yr), col=i) 
  i  <<- i+1
}
       )
print(p+ scale_color_manual(labels=unlist(namesRep),  values=1:nFiles))
ggsave(filename=file.path(outdir,paste("Spawning.pdf", sep="")), width=14, units="cm", height=10)


detach(Info)


j=1
g.names <- gear.names
lapply(res,
       function(d){
         yr <- d$yr
         p <- ggplot()+ xlim(range(yr))
         i=1
         prov <-   data.frame(pit = d$pit[i,], it=d$it[i,], iyr=d$iyr[i,], gear=rep(g.names[1], length(d$it[i,]) ) )
         norm<- max(prov$it, na.rm=T)
         prov$pit <- prov$pit /norm
         prov$it <- prov$it /norm
         df <-   prov
        ## bug in reading abundance, last line split on 2 lines
         for(i in 2:(ng-1))
           {
           prov <-   data.frame(pit = d$pit[i,], it=d$it[i,], iyr=d$iyr[i,], gear=rep(g.names[i], length(d$it[i,]) ) )
           if(i==(ng-1) )
             {
             prov2 <- data.frame(pit = d$pit[i+1,], 
                                 it=d$it[i+1,], iyr=d$iyr[i+1,], 
                                 gear=rep(g.names[i], length(d$it[i,]) ) )
             prov <- rbind(prov, prov2)
             }
           norm<- max(prov$it, na.rm=T)
           prov$pit <- prov$pit /norm
           prov$it <- prov$it /norm
           df <- rbind(df, prov)
           }
         p <-ggplot() + geom_line(data=df,aes( x=iyr, y=it, col=gear)) +
           geom_line(data=df,aes(x=iyr, y=pit, col=gear), linetype="dotted", lwd=2 ) 
         print(p)
         print(file.path(outdir, paste(repNames[[j]], "Abundance.pdf", sep='')))
         ggsave(file.path(outdir, paste(repNames[[j]], "Abundance.pdf", sep='')), width=15, height=10, units="cm")
         j <<- j+1
         
       }      
)




j=1
g.names <- gear.names
lapply(res,
       function(d){
         yr <- d$yr
         p <- ggplot()+ xlim(range(yr))
         i=1
         prov <-   data.frame(pit = d$pit[i,], it=d$it[i,], iyr=d$iyr[i,], gear=rep(g.names[1], length(d$it[i,]) ) )
         norm<- max(prov$it, na.rm=T)
         prov$pit <- prov$pit /norm
         prov$it <- prov$it /norm
         df <-   prov
         ## bug in reading abundance, last line split on 2 lines
         for(i in 2:(ng-1))
         {
           prov <-   data.frame(pit = d$pit[i,], it=d$it[i,], iyr=d$iyr[i,], gear=rep(g.names[i], length(d$it[i,]) ) )
           if(i==(ng-1) )
           {
             prov2 <- data.frame(pit = d$pit[i+1,], 
                                 it=d$it[i+1,], iyr=d$iyr[i+1,], 
                                 gear=rep(g.names[i], length(d$it[i,]) ) )
             prov <- rbind(prov, prov2)
           }
           norm<- max(prov$it, na.rm=T)
           prov$pit <- prov$pit /norm
           prov$it <- prov$it /norm
           df <- rbind(df, prov)
         }
         p <-ggplot() + geom_line(data=df,aes( x=iyr, y=it, col=gear)) +
           geom_line(data=df,aes(x=iyr, y=pit, col=gear), linetype="dotted", lwd=2 ) 
         print(p)
         print(file.path(outdir, paste(repNames[[j]], "Abundance.pdf", sep='')))
         ggsave(file.path(outdir, paste(repNames[[j]], "Abundance.pdf", sep='')), width=15, height=10, units="cm")
         j <<- j+1
         
       }      
       )





j=1
lapply(res,
       function(d){
         yr <- d$yr
         p <- ggplot()+ xlim(range(yr))
         df <-   data.frame(ct = d$ct[1,], obs_ct=d$obs_ct[1,], yr=d$yr)
         p <-ggplot() + geom_line(data=df,aes( x=yr, y=ct)) +
           geom_line(data=df,aes(x=yr, y=obs_ct), linetype="dotted", lwd=2 ) 
         print(p)
         ggsave(file.path(outdir, paste(repNames[[j]], "Catch.pdf", sep='')), width=15, height=10, units="cm")
         j <<- j+1
       }      
       )