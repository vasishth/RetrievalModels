library(reshape)	# for aggregating
library(plyr)


retr.stat.all <- function(m) {
	table(as.factor(m$rv100.class))
	
	## Experimentel Data: ##
	load("psc.rstat.alldata.RData")
	rstat1
	rstat2
	
	
	## Model: ##
	# head(m)
	#m.m1 <- melt(subset(m,(freq.class==3 & TFT!=0)),
	m.m1 <- melt(subset(m,(TFT!=0)),
		id=c("sn","roi","rv100.class"),
		measure=c("rv100","FPRT","FFD","SFD","RPD","TFT","RRT"),
		variable="Measure", na.rm = T)
	# head(m.m1) # summary(m.m1) # dim(m.m1) # str(m.m1)
	#rstat1.m <- data.frame(round(cast(subset(m.m1, !(Measure=="SFD" & value==0)), rv100.class ~ Measure, mean)))
	rstat1.m <- data.frame(round(cast(subset(m.m1, value!=0), rv100.class ~ Measure, mean)))
	if(dim(rstat1.m)[2]==7) rstat1.m$RRT <- NA
	rstat1.m
	
	#m.m2 <- melt(subset(m,freq.class==3),
	m.m2 <- melt(m,
		id=c("sn","roi","rv100.class"),
		measure=c("rv100","skip","onefix","refix","fp_reg","reread"),
		variable="Measure", na.rm = T)
	# head(m.m2) # summary(m.m2) # dim(m.m2) # str(m.m2)
	rstat2.m <- data.frame(round(cast(m.m2, rv100.class ~ Measure, mean),digits=2))
	rstat2.m[2] <- round(rstat2[2])
	rstat2.m
	
	## correlations: ##
	rstat1.cor <- round(cor(rstat1[,3:8],rstat1.m[,3:8], method="pearson"), digits=2)
	rstat2.cor <- round(cor(rstat2[,3:7],rstat2.m[,3:7], method="pearson"), digits=2)
	
	i <- diag(nrow=length(1:6))==1
	(rstat1.cor <- rstat1.cor[,1:6][i])
	i <- diag(nrow=length(1:5))==1
	(rstat2.cor <- rstat2.cor[,1:5][i])
	rstat.cor.mean <- mean(c(rstat1.cor,rstat2.cor),na.rm=T)
	
	## rmsd: ##
	rstat1.rmsd <- NULL
	rstat2.rmsd <- NULL
	for(i in 3:8){
		rstat1.rmsd <-	cbind(rstat1.rmsd,nrmsd(rstat1[,i],rstat1.m[,i],rstat1.sd[,i-2]))
	}
	for(i in 3:7){
		rstat2.rmsd <-	cbind(rstat2.rmsd,nrmsd(rstat2[,i],rstat2.m[,i],rstat2.sd[,i-2]))
	}
	
	(rstat1.rmsd <- as.vector(rstat1.rmsd))
	(rstat2.rmsd <- as.vector(rstat2.rmsd))
	rstat.rmsd.mean <- mean(c(rstat1.rmsd,rstat2.rmsd),na.rm=T)
	
	
	## SCORES ##
	scores <- as.vector(1*c(rstat1.rmsd,rstat2.rmsd) + 1*(1 - c(rstat1.cor,rstat2.cor))) 
	pscore <- mean(scores, na.rm=T)
	round(rstat.cor.mean, digits=2)
	round(rstat.rmsd.mean, digits=3)
	round(pscore,digits=3)
	
	fit <- c(round(pscore, digits = 3), 
			round(rstat.cor.mean, digits = 2), 
			round(rstat.rmsd.mean, digits = 3),
			var(scores, na.rm=T))
	
	save(fit, pscore, scores, rstat1, rstat2, rstat1.m, rstat2.m, rstat1.rmsd, rstat2.rmsd, rstat1.cor, rstat2.cor, rstat.rmsd.mean, rstat.cor.mean, file="rstat.alldata.RData")	

	return(fit)
}



#################################################
### Plots
#################################################

## EARLY ##
plot.retr.early <- function(file) {
	load(file)
	modelname = "model"
	
	lty <- c(3,3,3,1,1,1)
	pch <- c(1,22,2,19,15,17)
	
	## Plot of rstat1 ##
	#pdf(file = paste("plots/psc",modelname,"rstat1.alldata.pdf",sep="-"), width=5, height=5)
	
	matplot(cbind(rstat1[,3:5],rstat1.m[,3:5]), type="b", lty=lty, col=1, pch=pch, lwd=2
	        #,x=mean.rv100
	        ,xaxp=c(1,3,2)
	#        ,ylim=c(0,1)
	        ,xlab="Retrieval Class"
	        ,ylab="Reading Time")
	legend("topleft", lwd=1, cex=0.9, xjust=1, yjust=1
	       ,legend = c("model","data","gaze","first","single")
	       ,lty=c(1,3,0,0,0)
	       ,pch=c(NA,NA,19,15,17)
	       ,horiz=FALSE)
	
	#dev.off()
	
	
	#pdf(file = paste("plots/psc",modelname,"rstat2.alldata.pdf",sep="-"), width=5, height=5)
	
	## Plot of rstat2 ##
	matplot(cbind(rstat2[,3:5],rstat2.m[,3:5]), type="b", lty=lty, col=1, pch=pch, lwd=2
	        #,x=mean.rv100
	        ,xaxp=c(1,5,4)
	        ,ylim=c(0,1)
	        ,xlab="Retrieval Class"
	        ,ylab="Probability")
	legend("right", lwd=1, cex=1.0, xjust=1, yjust=1
	       ,legend = c("model","data","skip","onefix","refix")
	       ,lty=c(1,3,0,0,0)
	       ,pch=c(NA,NA,19,15,17)
	       ,horiz=F)
	
	#dev.off()
}


## LATE ##
plot.retr.late <- function(file) {
	load(file)
	modelname = "model"

	lty <- c(3,3,3,1,1,1)
	pch <- c(1,22,2,19,15,17)
	
	#pdf(file = paste("plots/psc",modelname,"rstat1_1.alldata.pdf",sep="-"), width=5, height=5)
	
	matplot(cbind(rstat1[,6:8],rstat1.m[,6:8]), type="b", lty=lty, col=1, pch=pch, lwd=2
	        #,x=mean.rv100
	        ,xaxp=c(1,3,2)
	#        ,ylim=c(0,1)
	        ,xlab="Retrieval Class"
	        ,ylab="Reading Time")
	legend("bottomright", lwd=1, cex=0.9, xjust=1, yjust=1
	       ,legend = c("model","data","RPD","TFT","RRT")
	       ,lty=c(1,3,0,0,0)
	       ,pch=c(NA,NA,19,15,17)
	       ,horiz=FALSE)
	
	#dev.off()
	
	
	lty <- c(3,3,1,1)
	pch <- c(1,22,15,17)
	
	#pdf(file = paste("plots/psc",modelname,"rstat2_2.alldata.pdf",sep="-"), width=5, height=5)
	
	## Plot of rstat2 ##
	matplot(cbind(rstat2[,6:7],rstat2.m[,6:7]), type="b", lty=lty, col=1, pch=pch, lwd=2
	        #,x=mean.rv100
	        ,xaxp=c(1,5,4)
	        ,ylim=c(0,1)
	        ,xlab="Retrieval Class"
	        ,ylab="Probability")
	legend("right", lwd=1, cex=1.0, xjust=1, yjust=1
	       ,legend = c("model","data","fp_reg","reread")
	       ,lty=c(1,3,0,0)
	       ,pch=c(NA,NA,19,15)
	       ,horiz=F)
	#dev.off()
}

