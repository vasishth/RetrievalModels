library(reshape) # for aggregating
library(plyr)


################################################
## Frequency Summary Statictics
################################################
freq.stat.early <- function(m) {

	## Experimentel Data: ##
	load("psc.fstat.RData")
	fstat1
	fstat2
	fstat.d <- c(fstat1$FPRT, fstat1$FFD, fstat1$SFD, fstat2$skip, fstat2$onefix, fstat2$refix)
	fstat.sds.d <- c(fstat1.sd$FPRT, fstat1.sd$FFD, fstat1.sd$SFD, fstat2.sd$skip, fstat2.sd$onefix, fstat2.sd$refix)
	

	## Model: ##
	# head(m)
	m.m1 <- melt(m, id = c("sn", "roi", "freq.class"), measure = c("frequency", 
		"FPRT", "FFD", "SFD"), variable = "Measure", na.rm = T)
	# head(m.m1) # summary(m.m1) # dim(m.m1) # str(m.m1)
	fstat1.m <- data.frame(round(cast(subset(m.m1, value != 0), freq.class ~ 
		Measure, mean)))
	fstat1.m

	m.m2 <- melt(m, id = c("sn", "roi", "freq.class"), measure = c("frequency", 
		"skip", "onefix", "refix"), variable = "Measure", na.rm = T)
	# head(m.m2) # summary(m.m2) # dim(m.m2) # str(m.m2)
	fstat2.m <- data.frame(round(cast(m.m2, freq.class ~ Measure, mean), digits = 2))
	fstat2.m$frequency <- round(fstat2.m$frequency)
	fstat2.m

	fstat.m <- c(fstat1.m$FPRT, fstat1.m$FFD, fstat1.m$SFD, fstat2.m$skip, fstat2.m$onefix, fstat2.m$refix)

	## correlations: ##
	fstat.cor <- NULL
	for (i in c(1,6,11,16,21,26)) {
		fstat.cor <- c(fstat.cor, cor(fstat.d[i:(i+4)], fstat.m[i:(i+4)], method="pearson"))}
	fstat.cor
	fstat.cor.mean <- mean(fstat.cor)  	

	
	## rmsd: ##
	fstat.rmsd <- sqrt(mean(((fstat.d-fstat.m)/fstat.sds.d)^2))

	## SCORES ##
	pscore <- mean(1 * fstat.rmsd + (1-fstat.cor.mean))
	
	fit <- c(fstat.cor.mean, fstat.rmsd)

	save(fit, pscore, fstat1, fstat2, fstat.d, fstat1.m, fstat2.m, fstat.rmsd, 
		fstat.cor, fstat.cor.mean,
		file = "fstat.RData")

	return(fit)
}


freq.stat.all <- function(m) {
	## Experimentel Data: ##
	load("psc.fstat.alldata.RData")
	fstat1
	fstat2
	fstat.d <- c(fstat1$FPRT, fstat1$FFD, fstat1$SFD, fstat1$RPD, fstat1$TFT, fstat1$RRT, fstat2$skip, fstat2$onefix, fstat2$refix, fstat2$fp_reg, fstat2$reread)
	fstat.sds.d <- c(fstat1.sd$FPRT, fstat1.sd$FFD, fstat1.sd$SFD, fstat1.sd$RPD, fstat1.sd$TFT, fstat1.sd$RRT, fstat2.sd$skip, fstat2.sd$onefix, fstat2.sd$refix, fstat2.sd$fp_reg, fstat2.sd$reread)
	
	
	## Model: ##
	# head(m)
	m.m1 <- melt(m,
		id=c("sn","roi","freq.class"),
		measure=c("frequency","FPRT","FFD","SFD", "RPD", "TFT", "RRT"),
		variable="Measure", na.rm = T)
	# head(m.m1) # summary(m.m1) # dim(m.m1) # str(m.m1)
	fstat1.m <- data.frame(round(cast(subset(m.m1,value!=0), freq.class ~ Measure, mean)))	
	if(dim(fstat1.m)[2]==7) fstat1.m$RRT <- 0
	fstat1.m[, 3:8] <- round(fstat1.m[, 3:8])
	fstat1.m[, 2] <- round(fstat1.m[, 2], digits = 2)
	fstat1.m

	
	m.m2 <- melt(m,
		id=c("sn","roi","freq.class"),
		measure=c("frequency","skip","onefix","refix", "fp_reg", "reread"),
		variable="Measure", na.rm = T)
	# head(m.m2) # summary(m.m2) # dim(m.m2) # str(m.m2)
	fstat2.m <- data.frame(round(cast(m.m2, freq.class ~ Measure, mean),digits=2))
	fstat2.m$frequency <- round(fstat2.m$frequency)
	fstat2.m

	fstat.m <- c(fstat1.m$FPRT, fstat1.m$FFD, fstat1.m$SFD, fstat1.m$RPD, fstat1.m$TFT, fstat1.m$RRT, fstat2.m$skip, fstat2.m$onefix, fstat2.m$refix, fstat2.m$fp_reg, fstat2.m$reread)

	## correlations: ##
	fstat.cor <- NULL
	for (i in c(1,6,11,16,21,26,31,36,41,46,51)) {
		fstat.cor <- c(fstat.cor, cor(fstat.d[i:(i+4)], fstat.m[i:(i+4)], method="pearson"))}
	fstat.cor
	fstat.cor[is.na(fstat.cor)] <- 0
	(fstat.cor.mean <- mean(fstat.cor, na.rm=T))
	
	
	## rmsd: ##
	#fstat.m[26:30] <- 0  # no RRT
	fstat.rmsd <- sqrt(mean(((fstat.d-fstat.m)/fstat.sds.d)^2, na.rm=T))
	
	## SCORES ##
	pscore <- mean(1 * fstat.rmsd + (1-fstat.cor.mean))
	round(fstat.cor.mean, digits=2)
	round(fstat.rmsd, digits=3)
	round(pscore,digits=3)
	

	fit <- rbind(fstat.cor, rep(fstat.rmsd,11))

	colnames(fit) <- c("FPRT","FFD","SFD","RPD","TFT","RRT", "skip","onefix","refix","fp_reg","reread")
	rownames(fit) <- c("cor","rmsd")

	
	save(fstat1, fstat2, fstat1.m, fstat2.m, fstat.rmsd, fstat.cor, fstat.cor.mean, file="fstat.alldata.RData")
	
	fit[is.na(fit)] <- 0
	return(fit)
}


#################################################
### Plots
#################################################
plot.freq.early <- function(file) {
#par(mfrow=c(1,2))
	
	load(file)
	modelname = "model"
	
	## sumstat1 on frequency
	#pdf(file = paste("plots/psc",modelname,"fstat1.pdf",sep="-"),    width=5, height=5)
	
	plot(c(1,2,3,4,5),fstat1.m$FPRT,
		,type="b" ,pch=19, lwd=2
	#	,ylim=c(200,407)
		,ylim=c(180,350)
		#,main=maintext
		,xlab="Frequency Class"
		,ylab="Duration (ms)"
		#,pty=s
		)
	lines(fstat1$FPRT, lty=3, type="b", pch=1, lwd=2)
	lines(fstat1.m$FFD, type="b", pch=15, lwd=2)
	lines(fstat1$FFD, lty=3, type="b", pch=22, lwd=2)
	lines(fstat1.m$SFD, type="b", pch=17, lwd=2)
	lines(fstat1$SFD, lty=3, type="b", pch=2, lwd=2)
	legend("topright",
	       legend = c("model","data","Gaze","FFD","SFD")
	       ,lty=c(1,3,0,0,0)
	       ,pch=c(NA,NA,19,15,17)
	       ,lwd=2
	       ,cex=0.8     #magnification of legend
	       ,xjust=1
	       ,yjust=1
	       #,merge=TRUE
	       ,horiz=FALSE)#, trace=TRUE)
	
	#dev.off()
	######################
	
	#pdf(file = paste("plots/psc",modelname,"fstat2.pdf",sep="-"),   width=5, height=5)
	
	plot(1:5,fstat2.m$skip,
		,type="b" ,pch=19, ylim=c(0,1), lwd=2
		#,main="PSC"
		,xlab="Frequency Class"
		,ylab="Probability"
		#,pty=s
		)
	lines(fstat2$skip, lty=3, type="b", pch=1, lwd=2)
	lines(fstat2.m$onefix, type="b", pch=15, lwd=2)
	lines(fstat2$onefix, lty=3, type="b", pch=22, lwd=2)
	lines(fstat2.m$refix, type="b", pch=17, lwd=2)
	lines(fstat2$refix, lty=3, type="b", pch=2, lwd=2)
	
	legend("topright",
	       legend = c("model","data","skip","once","multiple")
	       ,lty=c(1,3,0,0,0)
	       ,pch=c(NA,NA,19,15,17)
	       ,lwd=2
	       ,cex=0.7     #magnification of legend
	       ,xjust=1
	       ,yjust=1
	       #,merge=TRUE
	       ,horiz=FALSE)#, trace=TRUE)
	
	#dev.off()
	######################

}


## LATE ##
plot.freq.late <- function(file) {
	
	load(file)
	modelname = "model"
	
	lty <- c(3,3,3,1,1,1)
	pch <- c(1,22,2,19,15,17)
	
	#pdf(file = paste("plots/psc",modelname,"spustat1_1.alldata.pdf",sep="-"), width=5, height=5)
	
	matplot(cbind(fstat1[,6:8],fstat1.m[,6:8]), type="b", lty=lty, col=1, pch=pch, lwd=2
	        #,x=mean.rv100
	        ,xaxp=c(1,5,4)
	#        ,ylim=c(0,1)
	        ,xlab="Frequency Class"
	        ,ylab="Reading Time")
	legend("topright", lwd=1, cex=0.9, xjust=1, yjust=1
	       ,legend = c("model","data","RPD","TFT","RRT")
	       ,lty=c(1,3,0,0,0)
	       ,pch=c(NA,NA,19,15,17)
	       ,horiz=FALSE)
	
	#dev.off()
	
	
	lty <- c(3,3,1,1)
	pch <- c(1,22,19,15)
	
	#pdf(file = paste("plots/psc",modelname,"spustat2_2.alldata.pdf",sep="-"), width=5, height=5)
	
	## Plot of rstat2 ##
	matplot(cbind(fstat2[,6:7],fstat2.m[,6:7]), type="b", lty=lty, col=1, pch=pch, lwd=2
	        #,x=mean.rv100
	        ,xaxp=c(1,5,4)
	        ,ylim=c(0,0.3)
	        ,xlab="Frequency Class"
	        ,ylab="Probability")
	legend("topright", lwd=1, cex=1.0, xjust=1, yjust=1
	       ,legend = c("model","data","fp_reg","reread")
	       ,lty=c(1,3,0,0)
	       ,pch=c(NA,NA,19,15)
	       ,horiz=F)
	#dev.off()

}
