# post treatment mcmc run
dir <-c('bfte/2012/vpa/inflated/high')
main.dir= file.path(Sys.getenv("HOME"), "ICCAT/ICCAT-BFT")
setwd(file.path(main.dir, dir))

resmcmc=read.table("ICCAT.mcmc",header=TRUE)
BFTStatus <- data.frame(StockStatus=resmcmc$SSB/resmcmc$bmsy, FStatus=resmcmc$fFinal/resmcmc$fmsy)

## Diagnostic Graph
pdf(file=file.path(main.dir, "Report","Conv1.pdf"), width=10)
p <- ggplot(BFTStatus, aes(y = StockStatus, x=seq(1,50000,length.out=nrow(BFTStatus))) ) + geom_line() + xlab("Iterations") + ggtitle("MCMC Chains")
print(p)
dev.off()

pdf(file=file.path(main.dir, "Report","Conv2.pdf"), width=10)
p <- ggplot(BFTStatus, aes(y = FStatus, x=seq(1,40000,length.out=nrow(BFTStatus))) ) +  geom_line() +xlab("Iterations") + ggtitle("MCMC Chains")
print(p)
dev.off()

pdf(file=file.path(main.dir, "Report","PostStatus.pdf"), width=10)
m <- ggplot(BFTStatus, aes(x = StockStatus, y = FStatus)) +
  geom_point() + xlim(0, 2.8) + ylim(0, 1.5) +    geom_vline(xintercept = 1) + geom_hline(aes(yintercept=1))
print(m +  stat_density2d(aes(fill = ..level..), geom="polygon"))
dev.off()
