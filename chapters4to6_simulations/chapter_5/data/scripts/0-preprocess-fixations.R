library(em)

preprocess.fixations <- function(filename = "../output/fixations"){
	f <- read.table(filename)
	colnames(f) <- c("exp", "simulation","sentence","region","word","duration")
	# head(f) # summary(f)
	
	f$simulation <- as.factor(f$simulation)
	f$sentence <- as.factor(f$sentence)
	
	etm <- with(f,
	            em(region, duration,
	               trialId=list(simulation,sentence),
	               trialInfo=list(sim=simulation, sn=sentence)))
	                              
	# head(etm) # summary(etm)
	#save(etm, file=paste("data/psc-sim-etm.",Sys.Date(),".RData",sep=""))
	#save(etm, file="psc-sim-etm.RData")
	
	
	################################################
	### WORD INFO
	################################################
	wi <- read.table("psc-wordinfo-items.txt", header=TRUE)
	colnames(wi)[c(1:2)] <- c("sn","roi")
	# head(wi)
	
	## Add word count to word info
	wi$wordcount <- 0
	lastrois <- NULL
	for(i in 1:max(wi$sn)){
		last_i <- max(wi$roi[wi$sn==i])
		lastrois <- append(lastrois,last_i)
		wi$wordcount[wi$sn==i] <- last_i
		}
	# head(wi)
	
	
	################################################
	## Merge with measures
	################################################
	dim(etm)
	m <- merge(etm, wi, by=c("sn","roi"), all.x=TRUE)
	dim(m)
	# head(m) # summary(m)
	
	
	################################################
	## (1) Remove some words from data
	################################################
	## Remove first and last word from data
	m1 <- m
	m <- subset(m, (roi>1 & roi<wordcount))
	dim(m)
	
	##### Some data cleaning
	m1.1 <- m
	dim(m)
	## Remove words with FFD < 30ms
	(dim(subset(m,(FFD<30 & FFD>0)))[1]) / dim(m)[1]
	m <- subset(m,(FFD>=30 | FFD==0))
	dim(m)
	## Remove words with FFD > 1s
	(dim(subset(m,FFD>1000))[1]) / dim(m)[1]
	m <- subset(m,FFD<=1000)
	dim(m)
	## Remove words with TFT or FPRT > 1.5s
	(dim(subset(m,TFT>1500 | FPRT>1500))[1]) / dim(m)[1]
	m <- subset(m,(TFT<=1500 & FPRT<=1500))
	dim(m)
	## number of words removed
	dim(m1.1)[1]-dim(m)[1]
	
	
	################################################
	## (2) Some Probabilities
	################################################
	## skipping prob:
	m$skip<-ifelse(m$FPRT==0,1,0)
	
	## single and refix
	m$onefix<-ifelse((m$skip==0 & m$SFD>0),1,0)
	m$refix<-ifelse((m$skip==0 & m$SFD==0),1,0)

	## first-pass regression prob.
	m$fp_reg <- ifelse(m$RBRC>0,1,0) 
	
	## re-reading prob:
	m$reread<-ifelse(m$RRT>0,1,0)

	#save(m, file=paste("data/m.",Sys.Date(),".RData",sep=""))
#	save(m, file="m.RData")
	
	return(m)
}