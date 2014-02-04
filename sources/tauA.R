####################################################################################
##                                                                                ##
##   Investigation for the effect of tau_1                                        ##
##   Authors: Marie-Pierre Etienne marie.etienne@agroparistech.fr                 ##
##   Date: Aug. 10,  2013                                                         ##
##   Date: Jan,  11 2014                                                          ##
##                                                                                ##
##                                                                                ##
####################################################################################

main.dir= file.path(Sys.getenv("HOME"), "ICCAT/ICCAT-BFT")
outdir <- file.path(main.dir, "Report/figure")

sage<- 1
nage <- 10
age <- sage:nage
muat <- rep(0.1, nage-sage+1)
#muat <- sellogistic(age, mu_a=3, sigma_a=2.5)
#muat <- diff(c(muat,1)) / sum( diff(c(0,muat)) )


Nsim <-500

svalue =c(0.1, 0.5, 1, 1.5, 2, 2.5, 5, 10)
multiLogistic <- lapply(svalue, function(s){
      res <- lapply(1:Nsim,function(i)
        {
        epsilonA <- rnorm(length(age), mean=0, sd=sqrt(s))
        X <- log(muat)+epsilonA
        X <- X-sum(X)
        pa<- exp(X)/sum(exp(X))
        pa-muat
        return(max(pa-muat))
      }
                    )
      return(abs(unlist(res)))
}
         )
    
library(munsell)
library(scales)
f<-seq_gradient_pal(low = mnsl("10B 4/6"),
                  high = mnsl("10R 4/6"), space = "Lab")

palette(f(exp(seq(-50,-10,length.out=length(svalue))) ))

i <- 1
p<- ggplot()  +ylim( c(0,1)) +  xlab("tau_A")
lapply(multiLogistic, function(d){
  df <- data.frame(s=rep(svalue[i],length(d)), p=d)
  cat("**** ", log(svalue[i]), "*****\n")
  p <<- p + geom_point(data=df, aes(x=s, y=p), col=i) 
  i<<-i+1
})    
p
ggsave(file=file.path(outdir,  "MultivarLogistic.pdf"), width=12, height=12, units="cm")