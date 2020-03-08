remove(list=ls())
library(reshape)	# for aggregating


load("d.RData")

#################################################
### (0) Exclude Trials with interword regressions
#################################################
## identify trials with regressions
d$trial <- paste(d$sn, d$id, sep=".") ## add trial variable
regtrials <- na.omit(d$trial[d$TRC>0])
head(regtrials)
## percentage of trials with gregressions
round(length(unique(regtrials))/length(unique(d$trial))*100)

## remove trials from data and model
d0 <- d
dim(d0)
				### <---> ###
#d <- subset(d0, !(trial%in%regtrials)) ### COMMENT OUT IF NOT WANTED ###
				### <---> ###
dim(d)
## percentage
round(dim(d)[1]/dim(d0)[1] * 100)
# head(d)

################################################
## Summary Statictics: Frequency
################################################
# head(d)
d.m1 <- melt(d,
	id=c("sn","roi","freq.class"),
	measure=c("freq.pm","FPRT","FFD","SFD"),
	variable="Measure", na.rm = T)
# head(d.m1) # summary(d.m1) # dim(d.m1) # str(d.m1)
fstat1 <- data.frame(round(cast(subset(d.m1,value!=0), freq.class ~ Measure, mean)))
fstat1.sd <- data.frame(cast(subset(d.m1,value!=0), freq.class ~ Measure, sd))[3:5]
fstat1

d.m2 <- melt(d,
	id=c("sn","roi","freq.class"),
	measure=c("freq.pm","skipped","onefix","twofix"),
	variable="Measure", na.rm = T)
# head(d.m2) # summary(d.m2) # dim(d.m2) # str(d.m2)
fstat2 <- data.frame(round(cast(d.m2, freq.class ~ Measure, mean),digits=2))
fstat2$freq.pm <- round(fstat2$freq.pm)
fstat2 # fix(d.m2)
fstat2.sd <- sqrt((fstat2*(1-fstat2))[3:5])


save(fstat1,fstat2,fstat1.sd,fstat2.sd,file="psc.fstat.alldata.RData")
