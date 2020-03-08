remove(list=ls())
#flush.console()

source("functions.R")
source("0-preprocess-fixations.R")


#getwd()
#model <- "model-psc-s"
#file <- paste(getwd(),"..", model, "fixations.txt", sep="/")
#mfile <- paste(getwd(),"..", model, "m.RData", sep="/")
#setwd("../model-psc-sh/")
setwd("../model-src-nsp_3-4-135/")
file <- "fixations.txt"

#m <- preprocess.fixations(file)
load("m.RData")
source("../scripts/2-f-statistics-SC.R")
round(fstat.cor.mean, digits=2)
round(fstat.rmsd.mean, digits=3)

#(params <- extract.params())
#(regtrials <- reg.trials(m))
#save(m,params,fstat,regtrials, file="m.RData")

dev.new(width=7, height=4)
par(mfrow=c(1,2))

#pdf(file = "plots/fstat-noreg.pdf", width=10, height=5)
#par(mfrow=c(1,2))
plot.freq.early("fstat.RData")
#source("../scripts/2.1-f-plots-SC.R")
title(main=paste("cor:",round(mean(fstat[1,]),digits=2)," rmsd:",round(mean(fstat[2,]),digits=3), sep=" "), cex=0.5)
#plot.freq.late("fstat.alldata.RData")


m1 <- exclude.regressions(m)
(fstat1 <- freq.stat.early(m1))
save(m1,params,fstat1,regtrials, file="m1.RData")
#pdf(file = "plots/fstat-noreg.pdf", width=10, height=5)
#par(mfrow=c(1,2))
plot.freq.early("fstat.RData")
title(main=paste("cor:",round(mean(fstat1[1:2]),digits=2)," rmsd:",round(mean(fstat1[3:4]),digits=3), sep=" "), cex=0.5)







###############################################################

sstat <- spu.stat.early(m)
pdf(file = "plots/spustat-norg.pdf", width=10, height=5)
par(mfrow=c(1,2))
plot.spu.early("spustat.RData")
title(main=paste("cor:",sstat[2]," rmsd:",sstat[2], sep=" "))

sstat <- spu.stat.all(m)
plot.spu.early("spustat.alldata.RData")
plot.spu.late("spustat.alldata.RData")

rstat <- retr.stat.all(m)
plot.retr.early("rstat.alldata.RData")
plot.retr.late("rstat.alldata.RData")


