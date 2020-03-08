library(reshape) # for aggregating
library(plyr)


################################################
## SPU Summary Statictics
################################################
spu.stat.early <- function(m) {
	table(as.factor(m$spu.class))

	## Experimentel Data: ##
	load("psc.spustat.RData")
	spustat1
	spustat2


	## Model: ##
	# head(m)
	#m.m1 <- melt(subset(m,freq.class==3),
	m.m1 <- melt(m, id = c("sn", "roi", "spu.class"), measure = c("spu", "FPRT", 
		"FFD", "SFD"), variable = "Measure", na.rm = T)
	# head(m.m1) # summary(m.m1) # dim(m.m1) # str(m.m1)
	spustat1.m <- data.frame(cast(subset(m.m1, value != 0), spu.class ~ Measure, 
		mean))
	spustat1.m[, 3:5] <- round(spustat1.m[, 3:5])
	spustat1.m[, 2] <- round(spustat1.m[, 2], digits = 2)
	spustat1.m

	#m.m2 <- melt(subset(m,freq.class==3),
	m.m2 <- melt(m, id = c("sn", "roi", "spu.class"), measure = c("spu", "skip", 
		"onefix", "refix"), variable = "Measure", na.rm = T)
	# head(m.m2) # summary(m.m2) # dim(m.m2) # str(m.m2)
	spustat2.m <- data.frame(round(cast(m.m2, spu.class ~ Measure, mean), digits = 2))
	spustat2.m


	## correlations: ##
	spustat1.cor <- round(cor(spustat1[, 3:5], spustat1.m[, 3:5], method = "pearson"), 
		digits = 2)
	spustat2.cor <- round(cor(spustat2[, 3:5], spustat2.m[, 3:5], method = "pearson"), 
		digits = 2)

	i <- diag(nrow = length(1:3)) == 1
	(spustat1.cor <- spustat1.cor[, 1:3][i])
	(spustat2.cor <- spustat2.cor[, 1:3][i])
	spustat.cor.mean <- mean(c(spustat1.cor, spustat2.cor), na.rm = T)

	## rmsd: ##
	spustat1.rmsd <- NULL
	spustat2.rmsd <- NULL
	for (i in 3:5) {
		spustat1.rmsd <- cbind(spustat1.rmsd, nrmsd(spustat1[, i], spustat1.m[, 
			i], spustat1.sd[, i - 2]))
		spustat2.rmsd <- cbind(spustat2.rmsd, nrmsd(spustat2[, i], spustat2.m[, 
			i], spustat2.sd[, i - 2]))
	}
	(spustat1.rmsd <- as.vector(spustat1.rmsd))
	(spustat2.rmsd <- as.vector(spustat2.rmsd))
	spustat.rmsd.mean <- mean(c(spustat1.rmsd, spustat2.rmsd), na.rm = T)


	## SCORES ##
	scores <- as.vector(1 * c(spustat1.rmsd, spustat2.rmsd) + 1 * (1 - c(spustat1.cor, 
		spustat2.cor)))
	pscore <- mean(scores, na.rm = T)


	round(spustat.cor.mean, digits = 2)
	round(spustat.rmsd.mean, digits = 3)
	round(pscore, digits = 3)

	fit <- c(round(pscore, digits = 3), round(spustat.cor.mean, digits = 2), 
		round(spustat.rmsd.mean, digits = 3), var(scores, na.rm = T))

	save(fit, pscore, spustat1, spustat2, spustat1.m, spustat2.m, spustat1.rmsd, spustat2.rmsd, 
		spustat1.cor, spustat2.cor, spustat.rmsd.mean, spustat.cor.mean, file = "spustat.RData")

	return(fit)
}


################################################
## SPU Summary Statictics ALLDATA
################################################
spu.stat.all <- function(m) {
	table(as.factor(m$spu.class))

	## Experimentel Data: ##
	load("~/Dropbox/FelixE/PSC-Model/PSC-CORPUS-DATA/psc.spustat.alldata.RData")
	spustat1
	spustat2


	## Model: ##
	# head(m)
#m.m1 <- melt(subset(m,freq.class==3 & TFT!=0),
m.m1 <- melt(subset(m, TFT != 0), id = c("sn", "roi", "spu.class"), measure = c("spu", 
		"FPRT", "FFD", "SFD", "RPD", "TFT", "RRT"), variable = "Measure", na.rm = T)
	# head(m.m1) # summary(m.m1) # dim(m.m1) # str(m.m1)
	spustat1.m <- data.frame(cast(subset(m.m1, value != 0), spu.class ~ Measure, 
		mean))
	if(dim(spustat1.m)[2]==7) spustat1.m$RRT <- 0
	spustat1.m[, 3:8] <- round(spustat1.m[, 3:8])
	spustat1.m[, 2] <- round(spustat1.m[, 2], digits = 2)
	spustat1.m

	#m.m2 <- melt(subset(m,freq.class==3),
	m.m2 <- melt(m, id = c("sn", "roi", "spu.class"), measure = c("spu", "skip", 
		"onefix", "refix", "fp_reg", "reread"), variable = "Measure", na.rm = T)
	# head(m.m2) # summary(m.m2) # dim(m.m2) # str(m.m2)
	spustat2.m <- data.frame(round(cast(m.m2, spu.class ~ Measure, mean), digits = 2))
	spustat2.m


	## correlations: ##
	spustat1.cor <- round(cor(spustat1[, 3:8], spustat1.m[, 3:8], method = "pearson"), digits = 2)
	spustat2.cor <- round(cor(spustat2[, 3:7], spustat2.m[, 3:7], method = "pearson"), digits = 2)

	i <- diag(nrow = length(1:6)) == 1
	(spustat1.cor <- spustat1.cor[, 1:6][i])
	i <- diag(nrow = length(1:5)) == 1
	(spustat2.cor <- spustat2.cor[, 1:5][i])
	spustat.cor.mean <- mean(c(spustat1.cor, spustat2.cor), na.rm = T)

	## rmsd: ##
	spustat1.rmsd <- NULL
	spustat2.rmsd <- NULL
	for (i in 3:8) {
		spustat1.rmsd <- cbind(spustat1.rmsd, nrmsd(spustat1[, i], spustat1.m[, 
			i], spustat1.sd[, i - 2]))
	}
	for (i in 3:7) {
		spustat2.rmsd <- cbind(spustat2.rmsd, nrmsd(spustat2[, i], spustat2.m[, 
			i], spustat2.sd[, i - 2]))
	}

	(spustat1.rmsd <- as.vector(spustat1.rmsd))
	(spustat2.rmsd <- as.vector(spustat2.rmsd))
	spustat.rmsd.mean <- mean(c(spustat1.rmsd, spustat2.rmsd), na.rm = T)


	## SCORES ##
	scores <- as.vector(1 * c(spustat1.rmsd, spustat2.rmsd) + 1 * (1 - c(spustat1.cor, spustat2.cor)))

	pscore <- mean(scores, na.rm = T)

	round(spustat.cor.mean, digits = 2)
	round(spustat.rmsd.mean, digits = 3)
	round(pscore, digits = 3)

	fit <- c(round(pscore, digits = 3), round(spustat.cor.mean, digits = 2), 
		round(spustat.rmsd.mean, digits = 3), var(scores, na.rm = T))

	save(fit, pscore, spustat1, spustat2, spustat1.m, spustat2.m, spustat1.rmsd, 
		spustat2.rmsd, spustat1.cor, spustat2.cor, spustat.rmsd.mean, spustat.cor.mean, 
		file = "spustat.alldata.RData")

	return(fit)
}


#################################################
### Plots
#################################################

## EARLY ##
plot.spu.early <- function(file) {
	load(file)
	
	modelname = "model"
	
	lty <- c(3,3,3,1,1,1)
	pch <- c(1,22,2,19,15,17)
	
	#pdf(file = paste("plots/psc",modelname,"spustat1.pdf",sep="-"), width=5, height=5)
	
	matplot(cbind(spustat1[,3:5],spustat1.m[3:5]), type="b", lty=lty, col=1, pch=pch, lwd=2
	        #,x=mean.rv100
	        ,xaxp=c(1,5,4)
	        ,ylim=c(180,270)
	        ,xlab="Surprisal Class"
	        ,ylab="Reading Time")
	legend("bottomright", lwd=1, cex=0.9, xjust=1, yjust=1
	       ,legend = c("model","data","gaze","first","single")
	       ,lty=c(1,3,0,0,0)
	       ,pch=c(NA,NA,19,15,17)
	       ,horiz=FALSE)
	
	#dev.off()
	
	
	#pdf(file = paste("plots/psc",modelname,"spustat2.pdf",sep="-"), width=5, height=5)
	
	## Plot of s2stat2 ##
	matplot(cbind(spustat2[,3:5],spustat2.m[3:5]), type="b", lty=lty, col=1, pch=pch, lwd=2
	        #,x=mean.rv100
	        ,xaxp=c(1,5,4)
	        ,ylim=c(0,1)
	        ,xlab="Surprisal Class"
	        ,ylab="Probability")
	legend("right", lwd=1, cex=1.0, xjust=1, yjust=1
	       ,legend = c("model","data","skip","onefix","refix")
	       ,lty=c(1,3,0,0,0)
	       ,pch=c(NA,NA,19,15,17)
	       ,horiz=FALSE)
	
	#dev.off()
}




## LATE ##
plot.spu.late <- function(file) {
	
	load(file)
	modelname = "model"
	
	lty <- c(3,3,3,1,1,1)
	pch <- c(1,22,2,19,15,17)
	
	#pdf(file = paste("plots/psc",modelname,"spustat1_1.alldata.pdf",sep="-"), width=5, height=5)
	
	matplot(cbind(spustat1[,6:8],spustat1.m[,6:8]), type="b", lty=lty, col=1, pch=pch, lwd=2
	        #,x=mean.rv100
	        ,xaxp=c(1,5,4)
	#        ,ylim=c(0,1)
	        ,xlab="Surprisal Class"
	        ,ylab="Reading Time")
	legend("bottomright", lwd=1, cex=0.9, xjust=1, yjust=1
	       ,legend = c("model","data","RPD","TFT","RRT")
	       ,lty=c(1,3,0,0,0)
	       ,pch=c(NA,NA,19,15,17)
	       ,horiz=FALSE)
	
	#dev.off()
	
	
	lty <- c(3,3,1,1)
	pch <- c(1,22,15,17)
	
	#pdf(file = paste("plots/psc",modelname,"spustat2_2.alldata.pdf",sep="-"), width=5, height=5)
	
	## Plot of rstat2 ##
	matplot(cbind(spustat2[,6:7],spustat2.m[,6:7]), type="b", lty=lty, col=1, pch=pch, lwd=2
	        #,x=mean.rv100
	        ,xaxp=c(1,5,4)
	        ,ylim=c(0,1)
	        ,xlab="Surprisal Class"
	        ,ylab="Probability")
	legend("right", lwd=1, cex=1.0, xjust=1, yjust=1
	       ,legend = c("model","data","fp_reg","reread")
	       ,lty=c(1,3,0,0)
	       ,pch=c(NA,NA,19,15)
	       ,horiz=F)
	#dev.off()

}
