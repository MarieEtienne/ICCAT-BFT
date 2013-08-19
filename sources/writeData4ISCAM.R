####################################################################################
##                                                                                ##
##   file called by setiSCAMFiles.R                                               ##
##   convert vpa data *.d1 into iSCAM data                                        ##
##   Authors: Marie-Pierre Etienne marie.etienne@agroparistech.fr                 ##
##   Date: Jul. 25,  2013                                                         ##
##   Date: Aug. 13st, 2013                                                         ##
##                                                                               ##
##                                                                                ##
####################################################################################

cat("**writing Data File\n")
f.out<- file.path(main.dir,out, 'ICCAT.dat' )

outf <- character(0)

count <- 1
#############################################################
##  Model dimension section                                ##
#############################################################
outf[count:(count<-(count+2))] <- c( 
                '## ------------------------------------------------------------------------- ##',
                '#  MODEL DIMENSIONS                                                          ##',
                '## ------------------------------------------------------------------------- ##'
                )

outf[count <- count+1] <- paste('\t',syr ,'\t\t# -first year of data\t\t\t(syr)', sep='')
outf[count <- count+1] <- paste('\t', nyr,'\t\t# -last year of data\t\t\t(nyr)', sep='')
outf[count <- count+1] <- paste('\t', sage ,'\t\t# -age of youngest age class\t\t\t(sage)',sep='')
outr[countr <- countr+1] <- outf[count]
outf[count <- count+1] <- paste('\t', nage,'\t\t# -age of plus group\t\t\t(nage)',sep='')
outr[countr <- countr+1] <- outf[count]
outf[count <- count+1] <- paste('\t', ngear,'\t\t# -number of gears\t\t\t(ngear)',sep='')
outr[countr <- countr+1] <- outf[count]

#############################################################
##  Allocation for each gear section                       ##
#############################################################
outf[(count+1):(count<- count+4)] <- c('##',
                '## ------------------------------------------------------------------------- ##',
                '## Allocation for each gear in (ngear), use 0 for survey gears.              ##',
                '## ------------------------------------------------------------------------- ##')
outf[count <- count + 1 ] <- paste('\t1\t', paste(rep(0, ngear-1), collapse='\t'), sep='')
                                       
#############################################################
## Type of catch section                                   ##
#############################################################
outf[(count+1):(count<- count+6)] <- 
  c('##',
    '## Type of catch: an ivector based on legend below                           ##',
    '##               1 = catch in weight                                         ##',
    '##               2 = catch in numbers                                        ##',
    '##               3 = catch in spawn (roe)                                    ##',
    '## ------------------------------------------------------------------------- ##'
    )
outf[count <- count + 1] <- paste('\t2\t', paste(rep(1, ngear-1), collapse='\t'), sep='')
  


###############################################################################
## Age-schedule and population parameters section                            ##
###############################################################################

outf[(count+1):(count<- count+4)] <- 
  c('##',
    '## ------------------------------------------------------------------------- ##',
    '## Age-schedule and population parameters                                    ##',
    '## ------------------------------------------------------------------------- ##')
#### I have to fix the natural mortality for all ages, I use .2, other values come from 2012 report
outf[count <- count+1] <- paste('\t0.23\t\t# -natural mortality rate (m_fixed) TOBE DEPRECATED', sep='')  ### useless
outf[count <- count+1] <-"## -natural mortality rate at age"
outf[count <- count+1] <- paste("\t", paste(natMortality, collapse="\t"))

outf[count <- count+1] <- paste('\t', linf, '\t\t# -asymptotic length (linf) - cm ', sep='')
outf[count <- count+1] <- paste('\t', k,'\t\t# -body growth coefficient (k)', sep='')
outf[count <- count+1] <- paste('\t', t0, '\t\t# -theoretical age at zero length (to)',  sep='')
outf[count <- count+1] <- paste('\t', sclw ,'\t\t# -scaler in length-weight allometry', sep='')
outf[count <- count+1] <- paste('\t', plw, '\t\t# -power parameter in length-weight allometry', sep='')
outf[count <- count+1] <- paste('\t', m50, '\t\t# -age at 50% maturity (approx with log(3.0)/k)', sep='')
outf[count <- count+1] <- paste('\t',std50, '\t\t# -std at 50% maturity (CV ~ 0.1)', sep='')


#################################################################################
## TIME SERIES DATA section                                                    ##
#################################################################################
outf[(count+1):(count<- count+5)]<- 
c('##',
  '## ------------------------------------------------------------------------- ##', 
  '## TIME SERIES DATA                                                          ##',
  '## Observed catch (row dimensions syr:nyr) (col dimensions yr,1:ngear)       ##',
  '## ------------------------------------------------------------------------- ##')
outf[count <- count+1] <- paste('##  Year\t' , paste('Gear', 1:ngear, sep=' ', collapse='\t'), sep='')

for( ind in 1:length(Catch)){
  outf[count<- count+1] <- paste0('\t', syr+ind-1, '\t',Catch[ind] , 
                                  '\t', paste(rep(0, ngear-1), collapse='\t'))
}
  
#####################################################################################        
##    Extraction of the index abundance data                                       ##
#####################################################################################
## how many indices
outf[(count+1):(count<- count+4)]<- c(
  '##',
  '## ------------------------------------------------------------------------- ##',
  '## ABUNDANCE INDICES -A RAGGED ARRAY: (1,nit,1,nit_nobs,1,5)                 ##',
  '## ------------------------------------------------------------------------- ##')
nit <- nrow(surveySpecification)
outf[count<- count+1]<- paste0('\t', nit,'\t# Number of abundance series   	int(nit)')

outf[count<- count+1] <- paste('\t', 
                               paste(nit_obs, collapse="\t"),
                               '# Number of observations in series   ivector(nit_nobs(1,nit))', collapse='\t'
                               )

outf[count<- count+1] <- paste('\t', 
                               paste(survey_type, collapse='\t'),
                               '# Survey type (see key below)  	ivector(survey_type(1,nit))', collapse='\t'
                               )
outf[(count+1):(count<- count+6)] <- c(
  '## 1 = survey is proportional to vulnerable numbers',
  '## 2 = survey is proportional to vulnerable biomass',
  '## 3 = survey is proportional to spawning biomass (e.g., a spawn survey)',
  '##',
  '## survey_data',
  '##		iyr	it		  gear		wt		survey_timing')

for( ind in 1:nrow(iSCAMsurvey))
{
  outf[count<- (count+1)] <- paste(round(iSCAMsurvey[ind,],4), collapse='\t')
}

outr[countr <- countr+1] <- "## Survey timing (if 0, the gear has no associated index)"
outr[countr <- countr+1] <- paste0(0,"\t ",paste(surveyTime, collapse="\t") )


##
#####################################################################################
## AGE COMPOSITION DATA SECTION                                                    ##
#####################################################################################
outf[(count+1):(count<- count+4)]<- 
  c('##',
    '# ------------------------------------------------------------------------- ##',
    '## AGE COMPOSITION DATA (ROW YEAR, COL=AGE) Ragged object                    ##',
    '## ------------------------------------------------------------------------- ##')

outf[count<- count+1] <- paste('\t', na_gear, '\t\t# Number of gears with age-comps int(na_gears)', sep='')
outf[count<- count+1] <- paste('\t', paste(na_obs, collapse='\t'), '\t\t# Number of rows in the matrix   ivector(na_nobs)', sep='')
outf[count<- count+1] <- paste('\t', paste(rep(sage, na_gear), collapse='\t'), '\t\t# Youngest age column\t ivector(a_sage)', sep='')
outf[count<- count+1] <- paste('\t', paste(rep(nage, na_gear), collapse='\t'), '\t\t# Oldest age column +group\t ivector(a_nage)', sep='')
outf[(count+1):(count<- count+2)]<- 
  c('## year gear age colums (numbers or proportions)',
    '## Commercial Age Comps')
for( ind in 1:nrow(compositionCatch))
{
  outf[count<- count+1] <- paste('\t', paste(round(compositionCatch[ind,],4), collapse='\t'), sep='')
  if(ind== na_obs[1])
    outf[count<- count+1] <- c('## Survey Age comps')
}

#####################################################################################
##   Weight at age  section                                                        ##
#####################################################################################
outf[(count+1): (count<- count+4)]<- c(
  '##',
  '## ------------------------------------------------------------------------- ##',
  '## EMPIRICAL WEIGHT-AT-AGE DATA                                              ##',
  '## ------------------------------------------------------------------------- ##')
if(ForgotWeight)
    waa=NULL
if(!is.null(waa)){
  outf[count <- count+1]<- paste('\t', nrow(waa), '\t # Number of years of weight-at-age data int(n_wt_obs)')
  outf[count <- count+1]<-c('## year age columns (sage, nage) of weight at age data')
  for( ind in 1:nrow(waa))
  {
    outf[count<- (count+1)] <- paste(round(waa[ind,],4), collapse='\t')
  }
}else{
  outf[count <- count+1]<- paste('\t', 0, '\t # Number of years of weight-at-age data int(n_wt_obs)')
}

#####################################################################################
##     END OF FILE MARK                                                            ##
#####################################################################################
outf[(count+1):(count<- count+5)]<- 
  c('##',
    '## ------------------------------------------------------------------------- ##',
    '## MARKER FOR END OF DATA FILE (eof)                                         ##',
    '## ------------------------------------------------------------------------- ##',
    '999')

con.out = file(description=f.out, open="w")
writeLines(outf, con=con.out,  sep='\n' ) 
close.connection(con.out)
               
