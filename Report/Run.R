fileRnw <- 'ICCAT2'
require('knitr')
setwd("/home/metienne/ICCAT/ICCAT-BFT/Report")
knit(input=paste0(fileRnw[1], '.Rnw'), output=paste0(fileRnw[1], '.tex'))
knit(input=paste0(fileRnw[1], '.Rnw'), output=paste0(fileRnw[1], '.R'), tangle=T)
