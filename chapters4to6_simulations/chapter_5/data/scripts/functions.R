################################################
## Root Mean Square Deviation
################################################
nrmsd.s01 <- function(data, model, sds) sqrt(mean(((data - model)/sds)^2))

nrmsd <- function(data, model, sds) sqrt(mean(((data - model)^2)/sds))
#nrmsd <- function(data, model, sds) mean(sqrt(((data - model)^2)/sds))




# normalized root mean squared scaled deviation
nrmssd <- function(obs, pred) sqrt(mean(( (obs-mean(obs))-(pred-mean(pred)))^2, na.rm=TRUE)/sd(obs, na.rm=TRUE))

rm <- function(squared.rmsd) sqrt(mean(squared.rmsd, na.rm=TRUE))
rmsd <- function(obs, pred) sqrt(mean((obs-pred)^2, na.rm=TRUE))


se <- function(x)
      {
        y <- x[!is.na(x)] # remove the missing values, if any
        sqrt(var(as.vector(y))/length(y))
}

ci <- function (scores){
	m <- mean(scores)
	stderr <- se(scores)
	len <- length(scores)
	upper <- m + qt(.975, df=len-1) * stderr 
	lower <- m + qt(.025, df=len-1) * stderr 
	return(data.frame(lower=lower,upper=upper))
}



### Count Trials with interword regressions
reg.trials <- function(m) {
	m$trial <- paste(m$sn, m$sim, sep=".")  ## add trial variable
	regtrials <- na.omit(m$trial[m$TRC>0])  ## identify trials with regressions
	## return percentage
	round(length(unique(regtrials))/length(unique(m$trial))*100)
}


### Exclude Trials with interword regressions
exclude.regressions <- function(m){
	m0 <- m
	m$trial <- paste(m$sn, m$sim, sep = ".") ## add trial variable
	regtrials <- na.omit(m$trial[m$TRC > 0]) ## identify trials with regressions
	round(length(unique(regtrials))/length(unique(m$trial)) * 100) ## percentage of trials with regressions
	################### <---> ###################
	m <- subset(m, !(trial %in% regtrials)) ### COMMENT OUT IF NOT WANTED ###
	################### <---> ###################
	dim(m0)
	dim(m)
	round(dim(m)[1]/dim(m0)[1] * 100) ## percentage
	# head(m)
	
	return(m)
}


### extract parameters from file
extract.params <- function(file="params.txt"){
  tmp <- scan(file, what="character", nlines=30, sep=" ", strip.white = T)
	enc <- tmp[grep(":VISUAL-ENCODING-FACTOR", tmp) + 1 ]
	encexp <- tmp[grep(":VISUAL-ENCODING-EXPONENT", tmp) + 1 ]
	prep <- tmp[grep(":SACCADE-PREPARATION-TIME", tmp) + 1 ]
	lf <- tmp[grep(":LF", tmp) + 1 ]
	sf <- tmp[grep(":SURPRISAL-FACTOR", tmp) + 1 ]
	hsf <- tmp[grep(":SURPRISAL-HL-FACTOR", tmp) + 1 ]
	retr <- tmp[grep(":Retrieval-ON", tmp) + 1 ]
	surpr <- tmp[grep(":Surprisal-ON", tmp) + 1 ]
	hlsurpr <- tmp[grep(":Surprisal-HL-ON", tmp) + 1 ]
#	hlsurpr <- NA

	params <- c(enc,encexp, prep,lf,sf,hsf,retr,surpr,hlsurpr)
	names(params) <- c("enc","encexp","prep","lf","sf","hsf","r-on","s-on","hs-on")
	params
	}

