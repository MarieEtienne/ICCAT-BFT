args <- 'ICCAT'
require('knitr')
setwd("/home/metienne/ICCAT/ICCAT-BFT/Report")
knit(input=paste0(args[1], '.Rnw'), output=paste0(args[1], '.tex'))
knit(input=paste0(args[1], '.Rnw'), output=paste0(args[1], '.R'), tangle=T)
