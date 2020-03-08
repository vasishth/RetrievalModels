

#hist(m$rv100)
#median(m$rv100)
m$r.class2[m$rv100<median(m$rv100)] <- 1
m$r.class2[m$rv100>=median(m$rv100)] <- 2
	
	
	m.m1 <- melt(m,
		id=c("sn","roi","freq.class","r.class2"),
		measure=c("frequency","FPRT","FFD","SFD", "RPD", "TFT", "RRT"),
		variable="Measure", na.rm = T)

	
	m.m2 <- melt(m,
		id=c("sn","roi","freq.class","r.class2"),
		measure=c("frequency","skip","onefix","refix", "fp_reg", "reread"),
		variable="Measure", na.rm = T)
	fstat2.m <- data.frame(cast(subset(m.m2,Measure!="frequency"), Measure+freq.class+r.class2 ~ ., function(x) c(M=mean(x), N=length(x))))
	fstat2.m$SD <- fstat2.m$M*(1-fstat2.m$M)
	fstat2.m$SE <- sqrt(fstat2.m$M*(1-fstat2.m$M))/sqrt(fstat2.m$N)
	fstat2.m$CI.lower <- fstat2.m$M + qt(.025, df=fstat2.m$N-1) * fstat2.m$SE
	fstat2.m$CI.upper <- fstat2.m$M + qt(.975, df=fstat2.m$N-1) * fstat2.m$SE
#	fstat2.m


	m.m1$r.class2 <- as.numeric(m.m1$r.class2)	
	fstat1r1.m <- data.frame(round(cast(subset(m.m1,(value != 0 & Measure!="frequency" & r.class2==1)), freq.class ~ Measure, mean)))
	fstat1r2.m <- data.frame(round(cast(subset(m.m1,(value != 0 & Measure!="frequency" & r.class2==2)), freq.class ~ Measure, mean)))
	fstat1rsd.m <- data.frame(cast(subset(m.m1,(value != 0 & Measure!="frequency" & r.class2==1)), freq.class ~ Measure, function(x) sd(x)/sqrt(length(x))))

fstat1rdiff.m <- (fstat1r2.m-fstat1r1.m)/fstat1rsd.m



	m.m2$r.class2 <- as.numeric(m.m2$r.class2)	
	fstat2r1.m <- data.frame(round(cast(subset(m.m2,(Measure!="frequency" & r.class2==1)), freq.class ~ Measure, mean),digits=2))
	fstat2r2.m <- data.frame(round(cast(subset(m.m2,(Measure!="frequency" & r.class2==2)), freq.class ~ Measure, mean),digits=2))
	cbind(fstat2r1.m, fstat2r2.m[,-1])
#	fstat2rsd.m <- data.frame(cast(subset(m.m2,(Measure!="frequency" & r.class2==1)), freq.class ~ Measure, function(x) SD=x*(1-x)))
	fstat2rsd.m <- data.frame(cast(subset(fstat2.m,(Measure!="frequency" & r.class2==1)), freq.class ~ Measure, function(x) x,value="SE"))

fstat2rdiff.m <- (fstat2r2.m-fstat2r1.m)/fstat2rsd.m


	
