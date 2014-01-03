####################################################################################
##                                                                                ##
##  Code to simulate data and overwrite real data                                 ##
##                                                                                ##
##   Authors: Marie-Pierre Etienne marie.etienne@agroparistech.fr                 ##
##   Date: Aug. 26,  2013                                                         ##
##   Date: Nov. 25,  2013                                                         ##
##                                                                                ##
##                                                                                ##
##  The purpose of this code is to generate                                       ##                                                                              ##
##              ICCAT.dat data in iSCAM format                                    ##
##              using simulation parameters                                       ##
####################################################################################

simulationModel<- function(seed)
{
  if(seed>0)
  {
    set.seed(seed)
    # par is a list containing 
    # pop parameters R0, Rinit, so, beta, M, F, tau_R
    # selectivity 
    # age composition tau2_A
    # inidices q, tau2_I
    # indices tau2_C
    #natM
    aw <- 1.95e-5
    bw <- 3.009
    linf <- 319
    k <- 0.093
    t0 <- -0.97
    m50 <- 4
    std50 <- 0.8
    rho <- 0.4
    varphi <- 1
    tau_I <- sqrt(rho)*varphi
    tau_A <- 0.1
    tau_R <- sqrt(1-rho)*varphi
    tau_C <- 0.1
    ft    <-rep(0.2, nyr-syr+1)
    age   <-  sage : nage
    la    <- linf * (1- exp(-k*(age-t0)))
    wa    <- aw *la ^bw
    fa    <- wa/(1+ exp( (m50-age)/std50))
    sbt   <-rep(NA, nyr-syr+1) 
    surv.timing <- rep(0.5, ngear)
    q <- rep(1e-5, ngear)
    h<- 0.9
    R0 <- exp(14.64)
    kappa <- 4*h/(1-h)
    Rinit=0.9*R0
    
    natSurvivorship <- c(1,exp(-cumsum(Info$natM[1:(nage-sage)])))
    phiE <- sum(natSurvivorship * fa)
    sb0 <- R0*phiE
    beta <- (kappa -1)/(R0*phiE)
    so <- kappa/phiE
    
    
    ####################################################################################
    ##   Selectivity                                                                  ##
    ####################################################################################
    
    selectivityFile<- "selectivityTable.txt"
    tmp <- readLines(file.path(main.dir,selectivityFile))
    selected.indices=c('SM_TP', 'LL_JP1','NW_PS', 'JP_LL2','SP_BB1', 'SP_BB2')#, 'SP_BB3')
    tmp<- tmp[c(T,c('SM_TP', 'LL_JP1','NW_PS', 'JP_LL2','SP_BB1', 'SP_BB2', 'SP_BB3')%in%selected.indices)]
    tmp <- unlist(strsplit(tmp, " "))
    tmp <- as.numeric(tmp[grep("[0-9]", tmp)])
    selectivity.matrix <- exp(matrix(tmp, byrow=T, ncol=nage-sage+1))
    
    ## totalMortality
    Z <- t(sapply(1:(nyr-syr+1), 
                  function(i) {
                    tmp<- (Info$natM+ft[i]*selectivity.matrix[1,]) 
                  }
                  ))
    ## 
    ##vulnerability list
    Vul <- list()
    for( g in 1:ngear)
      Vul[[g]] <- matrix(selectivity.matrix[g,], byrow=T, nrow=nyr-syr+1, ncol=nage-sage+1)
    
    ####################################################################################
    ##   Population model                                                             ##
    ####################################################################################
    noise<- rnorm(nyr-syr+1)
    N<- matrix(NA, ncol=nage-sage+1, nrow=nyr-syr+1)
    N[1,1]=Rinit *exp(noise[1]*tau_R - tau_R^2/2)
    
    for( i in 1:(nage-sage))
    {
      N[1, i+1] <- N[1,i] *exp(-Info$natM[i])
    }
    N[1,nage-sage+1] <- N[1,nage-sage+1] + N[1,nage-sage+1] * exp(- Info$natM[nage-sage+1]) 
    
    sbt[1]<- sum(N[1,]*fa) # spawning biomass in number !
    
    for( j in 1:(nyr-syr))
    {
      N[j+1,1] <- sbt[j] * so / (1+ beta * sbt[j]) * exp(noise[j+1]*tau_R-tau_R^2/2)  
      N[j+1, 2:(nage-sage+1)] <-   N[j, 1:(nage-sage)] * exp(-Z[j, 1:(nage-sage)]) 
      N[j+1,nage-sage+1]      <- N[j+1,nage-sage+1] + N[j,nage-sage+1]* exp(-Z[j, nage-sage+1])
      sbt[j+1]<- sum(N[j+1,]*fa)
    }
    
    ####################################################################################
    ##   CAA Data                                                                     ##
    ####################################################################################
    pat <- list(NULL)
    for (g in 1:ngear)
    {
      tmp <- Vul[[g]]*N
      tmp <- tmp / apply(tmp, 1, sum)
      pat[[g]] <- tmp*exp(matrix(rnorm(length(tmp)), ncol=nage-sage+1, nrow=nyr-syr+1)*tau_A) # no need for bias correction since everything is renormalised at the end
      pat[[g]] <- tmp / apply(pat[[g]], 1, sum)
    }
    ncomp <- nrow(compositionCatch)
    for( i in 1:ncomp)
    {
      gg <- compositionCatch[i,2]
      yy <- compositionCatch[i,1]-syr+1
      compositionCatch[i, 3:(nage+2)] <<- pat[[gg]][yy,]
    }
    ####################################################################################
    ##   Abundance indices                                                            ##
    ####################################################################################
    I <- list(NULL)
    for (g in 1:ngear)
    {
      tmp <- Vul[[g]]*N*exp(-surv.timing[g]*Z)
      tmp <-  apply(tmp, 1, sum)
      I[[g]] <- q[g] * tmp * exp(rnorm(length(tmp))*tau_I-tau_I^2/2) 
    }
    nsur <- nrow(iSCAMsurvey)
    for( i in 1:nsur)
    {
      gg <- iSCAMsurvey[i,3]
      yy <- compositionCatch[i,1]-syr+1
      iSCAMsurvey[i, 2] <<- round(I[[gg]][[yy]],3)
    }
    
    ####################################################################################
    ##   Catch                                                                        ##
    ####################################################################################
    tmp <- Vul[[1]]* matrix(ft,  nrow=nyr-syr+1, ncol=nage-sage+1) * N * (1-exp(-Z))/ Z
    Catch  <<- round(apply(tmp, 1, sum)*exp(rnorm(nyr-syr+1)*tau_C-tau_C^2/2))    
    simulatedData <- list(
      N=N,
      pat=pat,
      Catch=Catch,
      rho =rho,
      varphi = varphi,
      tau_I =tau_I,
      tau_A =tau_A,
      tau_R =tau_R,
      tau_C = tau_C,
      fa  =fa,
      sbt =sbt, q=q,
      beta =beta,
      so = so, R0 =R0, Rinit=Rinit,
      h= h
      )
  save(simulatedData, file=file.path(main.dir,out,"simulatedData.Rd"))    
  }
}
   
   