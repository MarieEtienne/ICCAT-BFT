####################################################################################
##                                                                                ##
##   Post treatment on mcmc runs obtained using iscam on bfte data                ##
##   Authors: Marie-Pierre Etienne marie.etienne@agroparistech.fr                 ##
##   Date: Aug. 10,  2013                                                         ##
##   Date: Jan,  11 2014                                                          ##
##                                                                                ##
##                                                                                ##
####################################################################################


main.dir= file.path(Sys.getenv("HOME"), "ICCAT/ICCAT-BFT")
## rep where to find mcmc outputs
rep<-unlist(lapply(c(file.path('bfte/2012/vpa','inflated'),file.path('bfte/2012/vpa','reported')),
                               function(d) {file.path(d, c("high-R0", "high-Rinit"))})
                   )
namesRep <- lapply(strsplit(rep, "/"), function(d) {paste(d[4], d[5], sep="")})

resmcmc <- lapply(rep, function(d){read.table(file.path(d,"ICCAT.mcmc"),header=TRUE)})
#removing burn in period
resmcmc <- lapply(resmcmc, function(d){d[(ceiling(nrow(d)/2)+1):nrow(d),]})

BFTStatus <- lapply(resmcmc, function(d){
    data.frame(StockStatus=d$SSB/d$bmsy, FStatus=d$fFinal/d$fmsy)
})


## Diagnostic Graph
i<- 1
p <- lapply(BFTStatus, function(d){
  ggplot(d, aes(y = StockStatus, x=seq(25001,50000,length.out=length(StockStatus))) ) + geom_line() +
     xlab("Iterations")
  ggsave(file=file.path(main.dir, "Report/figure",paste(namesRep[i], "-ConvSS.pdf", sep="")),width=14, units="cm", height=10)
  i<<-i+1
  }
            )

i<- 1
p <- lapply(BFTStatus, function(d){
  ggplot(d, aes(y = FStatus, x=seq(25001,50000,length.out=length(FStatus))) ) + geom_line() +
    xlab("Iterations")
  ggsave(file=file.path(main.dir, "Report/figure",paste(namesRep[i], "-ConvFS.pdf", sep="")),width=14, units="cm", height=10)
  i<<-i+1
}
)            

i<- 1
p <- lapply(BFTStatus, function(d){
  ggplot(d,  aes(x = StockStatus, y = FStatus)) +
    geom_point() + xlim(0, 3.5) + ylim(0, 1.5) +    
    geom_vline(xintercept = 1) + 
    geom_hline(aes(yintercept=1)) + 
    stat_density2d(aes(fill = ..level..), geom="polygon")
    ggsave(file=file.path(main.dir, "Report/figure",paste(namesRep[i], "-Kobe.pdf", sep="")),width=14, units="cm", height=10)
  i<<-i+1
}
            )            

            
i<- 1
p <- lapply(resmcmc, function(d){
  m <- ggplot( d, aes(x = log.ro)) + geom_histogram(aes(y = ..density..), binwidth=0.05) +
    geom_density()
  prior<-data.frame(x=seq(12,16,length.out=100), y=rep(1/4, 100))
  m <- m + geom_hline(aes(yintercept=0.25), col=6) + xlab("log(R0)")  + xlim(c(13.5,16)) 
  print(m)
  ggsave(file=file.path(main.dir, "Report/figure",paste(namesRep[i], "-postRO.pdf", sep="")),width=14, units="cm", height=10)
  i<<-i+1
}
            )            

i<- 1
p <- lapply(resmcmc, function(d){
  m <- ggplot( d, aes(x = log.rinit)) + geom_histogram(aes(y = ..density..), binwidth=0.1) +
    xlim(c(13,17)) +  geom_density()
  prior<-data.frame(x=seq(12,16,length.out=100), y=rep(1/4, 100))
  m <- m + geom_hline(aes(yintercept=0.25), col=6) + xlab("log(Rinit)")
  print(m)
  ggsave(file=file.path(main.dir, "Report/figure",paste(namesRep[i], "-postRinit.pdf", sep="")),width=14, units="cm", height=10)
  i<<-i+1
}
            )            





i<- 1
p <- lapply(resmcmc, function(d){
  l <-length(d$log.h)
  df1 <- data.frame(x1=seq(0,1,length.out=l),
                   y1=dbeta(seq(0,1,length.out=l),shape1=14, shape2=2.44)
                   )
  m <- ggplot( d, aes(x = (log.h))) + geom_histogram(aes(y = ..density..), binwidth=0.05) +  geom_density()
  m <- m + geom_line(data=df1, aes( x=x1,y=y1), col=6) + xlim(c(0.7,1)) +
                         xlab("h")    
  print(m)
  ggsave(file=file.path(main.dir, "Report/figure",paste(namesRep[i], "-posth.pdf", sep="")),width=14, units="cm", height=10)
  i<<-i+1
}
            )            



#Credibility interval
i<-1
CI <- lapply(resmcmc, function(d){
i<<-i+1
})


### decision table

  nRuns    <- nFiles
  TACprobs <- NULL
