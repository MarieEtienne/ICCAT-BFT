# post treatment mcmc run
dir <-c('bfte/2012/vpa/reported/high')
main.dir= file.path(Sys.getenv("HOME"), "ICCAT/ICCAT-BFT")
setwd(file.path(main.dir, dir))

resmcmc=read.table("ICCAT.mcmc",header=TRUE)
resmcmc <- resmcmc[round(nrow(resmcmc)/2):nrow(resmcmc),]
BFTStatus <- data.frame(StockStatus=resmcmc$SSB/resmcmc$bmsy, FStatus=resmcmc$fFinal/resmcmc$fmsy)

## Diagnostic Graph
p <- ggplot(BFTStatus, aes(y = StockStatus, x=seq(25001,50000,length.out=nrow(BFTStatus))) ) + geom_line() + xlab("Iterations") + ggtitle("MCMC Chains")
print(p)
ggsave(file=file.path(main.dir, "Report","InflatedConv1.pdf"),width=14, units="cm", height=10)

p <- ggplot(BFTStatus, aes(y = FStatus, x=seq(25001,50000,length.out=nrow(BFTStatus))) )+  geom_line() +xlab("Iterations") + ggtitle("MCMC Chains")
print(p)
ggsave(file=file.path(main.dir, "Report","InflatedConv2.pdf"),width=14, units="cm", height=10)


m <- ggplot(BFTStatus, aes(x = StockStatus, y = FStatus)) +
  geom_point() + xlim(0, 3.5) + ylim(0, 1.5) +    geom_vline(xintercept = 1) + geom_hline(aes(yintercept=1))
print(m +  stat_density2d(aes(fill = ..level..), geom="polygon"))
ggsave(file=file.path(main.dir, "Report","figure", "ICCAT-ReportedPostKobe.pdf"),width=14, units="cm", height=14)

post<- data.frame(log.ro= resmcmc$log.ro, log.h=resmcmc$log.h)
m <- ggplot( post, aes(x = log.ro)) + geom_histogram(aes(y = ..density..), binwidth=0.05) + geom_density() 
prior<-data.frame(x=seq(12,16,length.out=100), y=rep(1/4, 100))
m + geom_hline(aes(yintercept=0.25), col=6) + xlab("log(R0)")
ggsave(file=file.path(main.dir, "Report","figure", "ICCAT-InflatedpostRho.pdf"),width=14, units="cm", height=14)

m <- ggplot( post, aes(x = log.h ))+ geom_histogram(aes(y = ..density..), binwidth=0.01) + geom_density() 
m + geom_line(aes(x=seq(0,1,length.out=length(post$log.h)), 
                  y=dbeta(seq(0,1,length.out=length(post$log.h)),shape1=14, shape2=2.44)), col=6) + xlim(c(0.7,1)) +
                    xlab("h")
ggsave(file=file.path(main.dir, "Report","figure", "ICCAT-Inflatedposth.pdf"),width=14, units="cm", height=14)
