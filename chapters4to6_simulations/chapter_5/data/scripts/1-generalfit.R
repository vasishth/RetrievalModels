library(reshape)	# for aggregating
library(plyr)


general.fit <- function(m){	
	## Experimentel Data: ##
	load("psc.wordmeans.RData")
	
	################################################
	## Word-based Statictics
	################################################
	
	m$sn <- as.numeric(m$sn)
	m.m1 <- melt(m,
		id=c("sn","roi"),
		measure=c("FPRT","FFD","SFD","RPD","TFT","RRT"),
		variable="Measure", na.rm = T)
	# head(d.m1) # summary(d.m1) # dim(m.m1) # str(m.m1)
	m.c1 <- data.frame(round(cast(subset(m.m1,value!=0), sn+roi ~ Measure, mean)))
	if(dim(m.c1)[2]==7) m.c1$RRT <- 0
	
	m.c1[is.na(m.c1)] <- 0


	means1 <- merge(d.means1, m.c1, by=c("sn","roi"), all.x=TRUE)
	means1[is.na(means1)] <- 0
	#head(means1); dim(means1); summary(means1)
	
	head(d.means1)
	head(means1)
	summary(means1)
	
	
	## rmsd and correlation ##
	fit1 <- data.frame()
	tsize <- dim(d.means1)[2]
	#i<-3
	for(i in 3:tsize){
		fit1[1,i-2] <- cor(means1[,i], means1[,i+tsize-2])
		fit1[2,i-2] <- nrmsd(means1[,i], means1[,i+tsize-2], sd(means1[,i]))
	}
	colnames(fit1) <- c("FPRT","FFD","SFD","RPD","TFT","RRT")
	fit1
	
	#(cor(d.c1$SFD, m.c1$SFD, use="pairwise.complete.obs"))
	#res <- (m.c1$TFT - d.c1$TFT)
	#head(res)
	
	
	
	m.m2 <- melt(m,
		id=c("sn","roi"),
		measure=c("skip","onefix","refix","fp_reg","reread"),
		variable="Measure", na.rm = T)
	# head(d.m2) # summary(d.m1) # dim(m.m1) # str(m.m1)
	m.c2 <- data.frame(round(cast(m.m2, sn+roi ~ Measure, mean), digits=2))
	
	m.c2[is.na(m.c2)] <- 0
	
	head(d.means2)
	head(m.c2); dim(m.c2)
	summary(m.c2)
	
	## rmsd and correlation ##
	fit2 <- data.frame()
#	i <- 6
	for(i in 3:dim(d.means2)[2]){
		fit2[1,i-2] <- cor(d.means2[,i], m.c2[,i])
		fit2[2,i-2] <- nrmsd(d.means2[,i], m.c2[,i], sd(d.means2[,i]))
	}
	colnames(fit2) <- c("skip","onefix","refix","fp_reg","reread")
	fit2
	
	cor.rmsd <- cbind(fit1,fit2)
	
	#(cor(d.c2$RRP, m.c2$RRP, use="pairwise.complete.obs"))
	#(cor(d.c2$RO, m.c2$RO, use="pairwise.complete.obs"))
	
	cor.rmsd[is.na(cor.rmsd)] <- 0
	return(as.matrix(cor.rmsd))
}

# Extra-Sums-of-squares-F-test
# And BIC
# use residuals (Model minus data)
# (Baker, corbett & koedinger) 


