#remove(list=ls())
library(reshape)	# for aggregating
library(plyr)

#source("1-analyze.R")

#file <- paste("data/m.",Sys.Date(),".RData",sep="")
#file <- "data/m.RData"
#load(file)
# head(m)


#################################################
### (0) Exclude Trials with interword regressions
#################################################
m0 <- m
m$trial <- paste(m$sn, m$sim, sep=".")  ## add trial variable
regtrials <- na.omit(m$trial[m$TRC>0])  ## identify trials with regressions
round(length(unique(regtrials))/length(unique(m$trial))*100)  ## percentage of trials with regressions
################### <---> ###################
m <- subset(m, !(trial%in%regtrials)) ### COMMENT OUT IF NOT WANTED ###
################### <---> ###################
dim(m0); dim(m)
round(dim(m)[1]/dim(m0)[1] * 100) ## percentage
# head(m)


################################################
## Frequency Summary Statictics
################################################
## DATA
gaze <- c(293,272,256,234,214)
FFD <- c(248,233,230,223,208)
SFD <- c(265, 249, 243, 235, 216)
SKIP <- c(.10, .13, .22, .55, .67)
FIX1 <- c(.68, .70, .68, .44, .32)
FIX2 <- c(.20, .16, .10, .02, .01)

fstat1 <- data.frame(freq.class=c(1:5), frequency=0, FPRT=gaze,FFD=FFD,SFD=SFD)
fstat2 <- data.frame(freq.class=c(1:5), frequency=0, skipped=SKIP,onefix=FIX1,twofix=FIX2)

fstat.d <- c(gaze, FFD, SFD, SKIP, FIX1, FIX2)

## SDs from my calculation
sds <- c(140, 122, 123, 111, 111,
	96, 93, 98, 105, 103,
	98, 93, 95, 107, 103,
	0.32, 0.35, 0.41, 0.50, 0.46,
	0.48, 0.47, 0.47, 0.49, 0.46,
	0.43, 0.39, 0.30, 0.14, 0.10)


## SDs Reichle et al.
sds.r <- c(38.76, 44.81, 40.29, 59.61, 71.11,
	26.34, 31.65, 31.31, 49.21, 64.75,
	26.34, 31.65, 31.31, 49.21, 64.75,		## fake
	0.3000, 0.3363, 0.4142, 0.4975, 0.4702,
	0.4665, 0.4583, 0.4665, 0.4964, 0.4665,
	0.43, 0.39, 0.30, 0.14, 0.10
	)



## Model: ##
# head(m)
m.m1 <- melt(m,
	id=c("sn","roi","freq.class"),
	measure=c("frequency","FPRT","FFD","SFD"),
	variable="Measure", na.rm = T)
# head(m.m1) # summary(m.m1) # dim(m.m1) # str(m.m1)
fstat1.m <- data.frame(round(cast(subset(m.m1,value!=0), freq.class ~ Measure, mean)))
fstat1.m



m.m2 <- melt(m,
	id=c("sn","roi","freq.class"),
	measure=c("frequency","skipped","onefix","twofix"),
	variable="Measure", na.rm = T)
# head(m.m2) # summary(m.m2) # dim(m.m2) # str(m.m2)
fstat2.m <- data.frame(round(cast(m.m2, freq.class ~ Measure, mean),digits=2))
fstat2.m$frequency <- round(fstat2.m$frequency)
fstat2.m


fstat.m <- c(fstat1.m$FPRT, fstat1.m$FFD, fstat1.m$SFD, fstat2.m$skipped, fstat2.m$onefix, fstat2.m$twofix)


## correlations: ##
fstat.cor <- NULL
for (i in c(1,6,11,16,21,26)) {
	fstat.cor <- c(fstat.cor, cor(fstat.d[i:(i+4)], fstat.m[i:(i+4)], method="pearson"))}
fstat.cor
fstat.cor.mean <- mean(fstat.cor)  	


## rmsd: ##
fstat.rmsd.mean <- sqrt(mean(((fstat.d-fstat.m)/sds.r)^2))


## SCORES ##
#scores <- as.vector(1*c(fstat1.rmsd,fstat2.rmsd) + 1*(1 - c(fstat1.cor,fstat2.cor)) + 0.01*fstat.rangediff) 
#pscore <- mean(scores, na.rm=T) + var(scores, na.rm=T) + 0.1*fstat.zeros
pscore <- mean(1 * fstat.rmsd.mean + (1-fstat.cor.mean))

round(fstat.cor.mean, digits=2)
round(fstat.rmsd.mean, digits=3)
round(pscore,digits=3)



save(fstat1, fstat2, fstat.d, sds, sds.r, fstat1.m, fstat2.m, fstat.rmsd.mean, fstat.cor, fstat.cor.mean, file="fstat.RData")


