#remove(list=ls())
library(reshape)	
library(plyr)

#source("1-analyze.R")

#file <- paste("data/m.",Sys.Date(),".RData",sep="")
file <- "data/m.RData"
load(file)
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
## Experimentel Data: ##
#load("~/Dropbox/FelixE/PSC-Model/PSC-CORPUS-DATA/psc.fstat.RData")
fstat1 <- data.frame(freq.class=1:5, freq.pm=0,
		FPRT=c(293,272,256,234,214),
		FFD=c(248,233,230,223,208),
		SFD=c(265, 249, 243, 235, 216))
fstat1.sd <- data.frame(
		FPRT=c(147, 123, 132, 122, 130),
		FFD=c(100, 100, 100, 100, 100),
		SFD=c(100, 100, 100, 100, 100))
fstat2 <- data.frame(freq.class=1:5, freq.pm=0,
		skipped=c(.10, .13, .22, .55, .67),
		onefix=c(.68, .70, .68, .44, .32),
		twofix=c(.20, .16, .10, .02, .01))
fstat2.sd <- data.frame(
		skipped=c(.39, .40, .45, .50, .47),
		onefix=c(.48, .47, .47, .49, .46),
		twofix=c(.43, .39, .30, .14, .1))



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



## correlations: ##
fstat1.cor <- round(cor(fstat1[,3:5],fstat1.m[,3:5], method="pearson"), digits=2)
fstat2.cor <- round(cor(fstat2[,3:5],fstat2.m[,3:5], method="pearson"), digits=2)
#fstat1.cor[is.na(fstat1.cor)] <- 0
#fstat2.cor[is.na(fstat2.cor)] <- 0

i <- diag(nrow=length(1:3))==1
(fstat1.cor <- fstat1.cor[,1:3][i])
(fstat2.cor <- fstat2.cor[,1:3][i])
fstat.cor.mean <- mean(c(fstat1.cor,fstat2.cor),na.rm=T)

## rmsd: ##
fstat1.rmsd <- NULL
fstat2.rmsd <- NULL
for(i in 3:5){
	fstat1.rmsd <-	cbind(fstat1.rmsd,nrmsd(fstat1[,i],fstat1.m[,i],fstat1.sd[,i-2]))
	fstat2.rmsd <-	cbind(fstat2.rmsd,nrmsd(fstat2[,i],fstat2.m[,i],fstat2.sd[,i-2]))
}
(fstat1.rmsd <- as.vector(fstat1.rmsd))
(fstat2.rmsd <- as.vector(fstat2.rmsd))
fstat.rmsd.mean <- mean(c(fstat1.rmsd,fstat2.rmsd), na.rm=T)

## range: ##
range <- colwise(max)(cbind(fstat1[3:5],100*fstat2[3:5])) - colwise(min)(cbind(fstat1[3:5],100*fstat2[3:5]))
range.m <- colwise(max)(cbind(fstat1.m[3:5],100*fstat2.m[3:5])) - colwise(min)(cbind(fstat1.m[3:5],100*fstat2.m[3:5]))
fstat.rangediff <- sqrt((range-range.m)^2)

## control for zeros ##
fstat.zeros <- dim(subset(melt(fstat1.m, variable="Measure", na.rm=T),value==0))[1] + dim(subset(melt(fstat2.m, variable="Measure", na.rm=T),value==0))[1]

## SCORES ##
scores <- as.vector(1*c(fstat1.rmsd,fstat2.rmsd) + 1*(1 - c(fstat1.cor,fstat2.cor)) + 0.01*fstat.rangediff) 
pscore <- mean(scores, na.rm=T) + var(scores, na.rm=T) + 0.1*fstat.zeros
round(fstat.cor.mean, digits=2)
round(fstat.rmsd.mean, digits=3)
round(pscore,digits=3)




save(fstat1, fstat2, fstat1.m, fstat2.m, fstat1.rmsd, fstat2.rmsd,fstat1.cor, fstat2.cor, fstat.rmsd.mean, fstat.cor.mean, fstat.rangediff, fstat.zeros, file="data/fstat.RData")


