parseVpaData<- function(f.in, selected.indices=c('SM_TP', 'LL_JP1'))
{
  print(selected.indices)
  vpaData <- readLines(f.in)
  eosIndices <- grep('-1*[: :]*$',vpaData) ##end of section indices
  ns <- grep('# DATA FILE FOR Continuity', vpaData)
  substring <- unlist(strsplit(vpaData[ns+1]," "))
  substring <- substring[substring!='']
  syr <- strtoi(substring[1])  ### starting year for catch data
  nyr <- strtoi(substring[2])  ### last year

  ## reading sage and nage
  ns <- grep("<--AGE", vpaData)
  substring<- unlist(strsplit(vpaData[ns]," "))
  substring<- substring[grep("[1234567890]",substring)]
  sage <- strtoi(substring[1])
  nage <- strtoi(substring[length(substring)])
  
  ## reading catch at age for commercial fisheries
  ns <- grep('# NOW ENTER THE CATCH-AT-AGE DATA. ROW=YEAR, COLUMN=AGE', vpaData)+2
  compositionCatch <- matrix(NA, ncol=2+nage, nrow=nyr-syr+1)
  Catch<-rep(0, ncol=1, nrow=nyr-syr+1)
  CAA <- matrix(NA, ncol=nage-sage+1, nrow=nyr-syr+1)
  for( ind in (ns+1):(ns+(nyr-syr+1)) ){
    substring  <- as.numeric(unlist(strsplit(vpaData[ind], '\t')))
    CAA[ind-ns,]  <- substring[2:length(substring)]
    Catch[ind-ns] <- sum(CAA[ind-ns,])  
    compositionCatch[ind-ns, 1] <- syr + ind -ns -1
    compositionCatch[ind-ns, 2] <- 1
    compositionCatch[ind-ns, 3:(nage-sage+3)]  <- CAA[ind-ns,]/Catch[ind-ns]
  }
  
  ##reading abundance indices 
  ns <- grep("# NOW ENTER IN THE ABUNDANCE INDEX SPECIFICATIONS", vpaData)+7
  ##ns+7 = first index abundance
  nf <- eosIndices[which(eosIndices>ns)[1]]-1 #last line of abundance indices
  ind <- grep(paste(selected.indices,collapse="|"), vpaData)
  ind <- ind[which(ind%in% (ns:nf))]
  ngear <- length(ind)+1  #ngear number of total gear fisheries + index
  nit <- length(ind)
  
  surveySpecification <- as.character(unlist(strsplit(vpaData[ind],' ')))
  surveySpecification <- surveySpecification[surveySpecification!='']
  surveySpecification <- matrix(surveySpecification, ncol=8, byrow=T)
  survey.num <- ind-ns+1 #survey numbers in *.d1 files
  ns <- nf+6
  nf <- eosIndices[which( eosIndices >ns )][1]-1
  relprov <- unlist(strsplit(vpaData[(ns+1):nf],'\t')) #relative indices prov, avoiding warning message when conversion to numeric
  ii<- which(!grepl('[:ABCDEFGHIJKLMNOPQRSTUVWXYZ :]', relprov))
  relIndices <- as.numeric(relprov[ii] )
  relIndices <- matrix(relIndices, ncol=4, byrow=T)
  relIndices <- relIndices[relIndices[,1]%in%survey.num,]
  nit_obs <- nOccurrences(relIndices[,1])
  
  survey_type <-   as.integer(surveySpecification[,3])
  
  
  iSCAMsurvey <- data.frame(iyr=relIndices[,2], it=relIndices[,3],
                            gear=as.numeric(as.factor(relIndices[,1]))+1, #trick to number survey from 2 to nit+1 
                            wt=rep(1, nrow(relIndices)))
  surveyTime <- 0.5*(surveySpecification[,5]=='-1')+ as.numeric(surveySpecification[,5])*(as.numeric(surveySpecification[,5])>0)/12
  
  iSCAMsurvey$timing=surveyTime[as.numeric(as.factor(relIndices[,1]))]
   
  #####################################################################################
  ## AGE COMPOSITION DATA SECTION                                                    ##
  #####################################################################################
  ns<- grep('# NOW ENTER IN THE VULNERABILITIES', vpaData)+3
  nf <- eosIndices[eosIndices>ns][1]-1
  partial_catch <- matrix(as.numeric(unlist(strsplit(vpaData[ns:nf], '\t'))), ncol=nage-sage+3, byrow=T)
  partial_catch <- partial_catch[partial_catch[,1]%in%survey.num,] ##select survey
  partial_catch[,1] <- sapply(partial_catch[,1], function(x){ return(which(survey.num==x) )})+1
  prov <- partial_catch[,1]   #switching columns for year and survey
  partial_catch[,1] <- partial_catch[,2]
  partial_catch[,2] <- prov
  compositionCatch <- rbind(compositionCatch, partial_catch)
  na_obs <- nOccurrences(compositionCatch[,2])
  na_gear <- length(na_obs)
  compositionCatch[,3:(nage-sage+3)] <- compositionCatch[,3:(nage-sage+3)]/ 
    apply(compositionCatch[,3:(nage-sage+3)], 1, sum)
  
  #####################################################################################
  ##   Weight at age  section                                                        ##
  #####################################################################################
  
  ns <-grep('# NOW ENTER IN THE WEIGHTS AT AGE FOR THE INDICES OF ABUNDANCE', vpaData)+3
  if(length(ns)>=1){
    nf <- eosIndices[eosIndices>ns][1]-1
    waa <- as.data.frame(matrix(as.numeric(unlist(strsplit(vpaData[ns:nf],'\t'))), ncol=nage+2, byrow=T))
    names(waa)<- c("Survey", "Year", paste0("Age", seq(sage:nage) ))
    waa$Survey <- waa$Survey+1
    waa$YearSurvey=paste(waa$Year, waa$Survey, sep="_")
    waa <- waa[order(waa$Year),]
    test = reshape(waa, idvar = "YearSurvey", varying = list(3:12),
                   v.names = "empWeight", direction = "long", timevar="Age")
    waa <- matrix(NA, ncol=nage-sage+2, nrow=length(unique(test$Year)))
    waa[,1] <- unique(test$Year)
    waa[, 2:(nage-sage+2)] <- matrix(as.numeric(by(test$empWeight, INDICES=list(test$Age, test$Year), mean)), 
                                     ncol=nage-sage+1, byrow=T)
  }else
  {waa=NULL}
  
  res = list(syr=syr, nyr=nyr, sage=sage, nage=nage,
       Catch=Catch, ngear=ngear, CAA=CAA,
       survey.num=survey.num, survey_type=survey_type, surveyTime=surveyTime,surveySpecification=surveySpecification,
       nit=nit, nit_obs=nit_obs, iSCAMsurvey=iSCAMsurvey,
       compositionCatch=compositionCatch, na_gear=na_gear, na_obs=na_obs,
       waa=waa)
  return(res)
  }