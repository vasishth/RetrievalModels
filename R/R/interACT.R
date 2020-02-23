library(tidyr)
library(dplyr)

###############################################################
## Global parameters
###############################################################
reset_params <- function(){
  lf        <<- 0.15       # latency factor
  le        <<- 1       # latency exponent
  rth 	    <<- -1.5    # retrieval threshold
  bll       <<- 0.5     # decay parameter
  ans       <<- 0.2    # activation noise
  mas       <<- 1       # maximum associative strength 
  mp        <<- 1       # mismatch penalty
  ga        <<- 1       # goal source activation
  rand_time <<- 3       # latency variability
  blc       <<- 0       # base-level activation
  ##
  lp        <<- 1     # default time since last presentation (msec)
  ldp       <<- 1       # last distractor presentation (msec)
  ## Distractor control:
  ndistr    <<- 1				# number of distractors
  dbl       <<- 0       # distractor base-level
  ##
  ## Cue weighting, cue confusion and prominence:
  # cueweights <<- c(1,1) # vector of cue weightings
  cueweighting <<- 1     # Strength of structural cue as ratio str/sem
  normalizeWeights <<- TRUE 
  qcf 		  <<- 1 			# match quality correction factor
  qco 		  <<- -2*rth 			# match quality correction offset
  psc       <<- 1       # prominence scaling constant C1
  pic       <<- 0       # prominence scaling constant C2
  tprom     <<- 0       # target prominence 
  dprom     <<- 0       # distractor prominence
  # cl 				<<- 0 			# cue confusion level (0-100)
  cuesim    <<- -1    # cue-feature similarity [-1..0]
  #
  # Fitted meta-parameters
	meta_recent <<- 0.7
	meta_distant <<- 1.3
	meta_lowprom <<- -0.5
	meta_medprom <<- 0
	meta_highprom <<- 2.5
	meta_conflevel <<- 0
	meta_memory <<- 1
  meta_deptype <<- 0
  meta_method <<- 0
  #
  VERBOSE <<- TRUE
}
reset_params()

cuesim2cl <- function(x=cuesim){
	cl <<- (x+1)*100
	cl
}

cl2cuesim <- function(x=cl){
	cuesim <<- x/100-1
	cuesim
}

strWeight <- function(ratio=cueweighting, normalize=normalizeWeights){
  ifelse(normalize, ratio/(ratio+1)*2, ratio)
}
semWeight <- function(ratio=cueweighting, normalize=normalizeWeights){
  ifelse(normalize, 1/(ratio+1)*2, 1)
}

idnames <- c("Set","Iteration","Condition","Target","Distractor")
actrnames <- c("weights","bl1","bl2","times1","times2","noise1","noise2","blact1","blact2","act1","act2","activation","latency","retrieved","acc","miss","fail")
paramnames <- c("lf","le","rth","bll","ans","mas","mp","ga","rand_time","lp","blc","ldp","dbl","ndistr","cueweighting","psc","pic","qcf", "qco" ,"cuesim","tprom","dprom" #,"meta_distant","meta_recent","meta_lowprom", "meta_medprom","meta_highprom", "meta_method", "meta_deptype"
	)

set_prameters <- function(){
	parameters <<- list(lf,le,rth,bll,ans,mas,mp,ga,rand_time,lp,blc,ldp,dbl,ndistr,cueweighting,psc,pic,qcf, qco,cuesim,tprom,dprom #,meta_distant,meta_recent,meta_lowprom,meta_medprom,meta_highprom, meta_method, meta_deptype
		)
	names(parameters) <<- paramnames
}
set_prameters()


# compute_int_means <- function(results){
# 	## Extract interference effect
# 	int <- select(filter(results, Distractor=="Match"), -Condition, -Distractor)
# 	dim(int)
# 	int$int <- filter(results, Distractor=="Match")$latency - filter(results, Distractor=="Mismatch")$latency
# 	#
# 	## Compute means
# 	simMeans <- group_by(int, Set, Target, lf, ans, mas, mp, rth, bll, psc, pic, qcf, cuesim, tprom, dprom, ndistr, lp, ldp, dbl, distant,recent,medprom,highprom) %>% summarise(Effect=mean(int), SE=sd(int)/sqrt(length(int)))
# 	ungroup(simMeans)
# }

compute_int_means <- function(results){
	params <- results %>% select(Set:weights, -Iteration, -Condition, -Distractor, -latency) %>% distinct()
	intMeans <- results %>% select(Set, Iteration, Target, Distractor, latency) %>% spread(Distractor, latency) %>% mutate(Effect1 = Match-Mismatch) %>% group_by(Set, Target) %>% summarise(Effect = mean(Effect1), SE=sd(Effect1, na.rm=TRUE)/sqrt(n())) %>% ungroup()
	left_join(intMeans, params)
}

compute_cond_means <- function(results){
	params <- results %>% select(Set:weights, -Iteration, -latency) %>% distinct()
	condMeans <- results %>% group_by(Set, Condition, Target, Distractor) %>% summarise(Latency = mean(latency), SE=sd(latency, na.rm=TRUE)/sqrt(n()), Acc=mean(acc), Miss=mean(miss), Fail=mean(fail)) %>% ungroup()
	left_join(condMeans, params)
}


###############################################################
## RUN MODEL
###############################################################
#model <- model_4cond
#iterations <- 2

##
## PARAMETER MATRIX
##
create_param_matrix <- function(model, iterations=1000){
	print("creating parameter matrix...")
	#
	cl <<- (cuesim+1)*100
	set_prameters()
	#
	n_params <- length(parameters);
	#
	## The total number of combinations is the product of the number of values
	## for each parameter
	n_sets <- prod(unlist(lapply(parameters, length)))
	n_cond <- length(model$target_match)
	total <- iterations*n_sets*n_cond
	#
	print(paste("Conditions: ",n_cond))
	print(paste("Combinations: ",n_sets))
	print(paste("Total runs: ",total))
	#
	## Set up matrix of parameter combinations.  Rows are model experiments,
	## columns are parameters.
	param_combs <- matrix(nrow=n_sets, ncol=n_params);
	#
	cumulative_num_combs <- 1;
	for (p in 1:n_params) {
		param_combs[,p] <- rep(parameters[p][[1]], each=cumulative_num_combs, length.out=n_sets);
		cumulative_num_combs <- cumulative_num_combs * length(parameters[p][[1]]);
	}
	#
	param_matrix <- matrix(data=t(param_combs), nrow=total,ncol=n_params, byrow=TRUE);
 # 
	condnames <- 1:length(model$target_match)
	header <- c(idnames,paramnames,actrnames)
#
	id_matrix <- matrix(nrow=total, ncol=length(idnames))
	actr_matrix <- matrix(nrow=total, ncol=length(actrnames))
	d <- data.frame(cbind(id_matrix,param_matrix,actr_matrix))
	colnames(d) <- header
# d$chunk <- rep(chunknames,total/n_chunks)
	d$Set <- 1:n_sets
	d$Condition <- rep(condnames, each=n_sets)
	d$Target <- rep(model$Target, each=n_sets)
	d$Distractor <- rep(model$Distractor, each=n_sets)
	d$match1 <- rep(model$target_match, each=n_sets)
	d$match2 <- rep(model$distractor_match, each=n_sets)
	d$Iteration <- rep(1:iterations, each=n_sets*n_cond)
	d$weights <- model$weights
	return(d)
}



run <- function(d){
	if(VERBOSE) print("computing base-levels...")
	# 
	# belowzeros <<- NULL
	total <- nrow(d)
	d$times1 <- d$lp
	d$times2 <- d$ldp
	d$bl1 <- d$blc
	d$bl2 <- d$dbl
	d$cl <- (cuesim+1)*100
	#
	d$noise1 <- act_r_noise_n(total, d$ans)
	d$noise2 <- act_r_noise_n(total, d$ans)
	d$blact1 <- base_act(prom=d$tprom, times=d$times1, C1=d$pic, C2=d$psc, b=d$bl1,  noise=d$noise1, dec=d$bll)
	d$blact2 <- base_act(prom=d$dprom, times=d$times2, C1=d$pic, C2=d$psc, b=d$bl2,  noise=d$noise2, dec=d$bll)
	#
	## Cue match 
	match1 <- matrix(unlist(d$match1),nrow=total,ncol=length(d$match1[[1]]),byrow=TRUE)
	match2 <- matrix(unlist(d$match2),nrow=total,ncol=length(d$match2[[1]]),byrow=TRUE)
	weights <- matrix(unlist(d$weights),nrow=total,ncol=2,byrow=TRUE)
	#
	## Match quality (including cue confusion and scaled by base-level activation)
	cueconf1 <- match1-((match1-1)*(1+d$cuesim))
	cueconf1[cueconf1==0] <- 1
	cueconf2 <- match2-((match2-1)*(1+d$cuesim))
	cueconf2[cueconf2==0] <- 1
	if(VERBOSE) print("computing match quality...")
	Qj1 <- match_quality(match=match1, sim=d$cuesim, blact=d$blact1, tau=d$rth, s=d$ans, q=d$qcf, q2=d$qco)
	Qj2 <- match_quality(match=match2, sim=d$cuesim, blact=d$blact2, tau=d$rth, s=d$ans, q=d$qcf, q2=d$qco)
	# 
	## Probability of item given cue 
	if(VERBOSE) print("computing probability P(i|j)...")
	P1j <- Qj1 / (Qj1+(Qj2*d$ndistr) +0.0001)
	P2j <- Qj2 / (Qj1+(Qj2*d$ndistr) +0.0001)
	# 
	## Spreading activation
	if(VERBOSE) print("computing spreading activation...")
	Sj1 <- spreading_act(Pij=P1j, S=d$mas)
  Sj2 <- spreading_act(Pij=P2j, S=d$mas)
	d$Sji_neg <- rowMeans(ifelse(Sj1 < 0 | Sj2 < 0, T, F))
	# 
	## Activations
	if(VERBOSE) print("computing activations...")
  d$act1 <- d$blact1 + weight_spreading_act(Sji=Sj1, weights=weights*cueconf1, W=d$ga) + mismatch_penalty(cuematch=match1, P=d$mp) + d$noise1
  d$act2 <- d$blact2 + weight_spreading_act(Sji=Sj2, weights=weights*cueconf2, W=d$ga) + mismatch_penalty(cuematch=match2, P=d$mp) + d$noise2
  
	##
	## FINAL VALUES
	##
	if(VERBOSE) print("computing latencies...")
	d$activation <- ifelse(d$act1>d$act2, d$act1, d$act2)
	retrieved <- ifelse(d$act1>d$act2, 1, 2)
	d$retrieved <- ifelse(d$activation>d$rth, retrieved, 0)
	d$latency <- latency(d$activation, F=d$lf, f=d$le, tau=d$rth)
	d$acc <- ifelse(d$retrieved==1, 1, 0)
	d$miss <- ifelse(d$retrieved==2, 1, 0)
	d$fail <- ifelse(d$retrieved==0, 1, 0)
	#
	if(VERBOSE) print("FINISHED")
	#
	return(tbl_df(d))
}







###############################################################
## ACT-R
###############################################################
activation <- function(Pij=matrix(c(1,1),1,2,T), prom=0, weights=matrix(c(1,1),1,2,T), times=lp, b=blc, noise=act_r_noise(ans), W=ga, dec=bll, S=mas, P=mp, C1=pic, C2=psc){
  # base_act <- log(times^(-dec)) + bl
  baseact <- base_act(prom=prom, times=times, C1=C1, C2=C2, b=b, noise=noise, dec=dec)
  Sji <- S+log(Pij)
  Sji <- ifelse(Sji==-Inf | is.na(Sji), 0, Sji)
  ifelse(Sji < 0, print("!!! WARNING: Sji < 0 !!!"), T)
  Sji <- ifelse(Sji < 0, 0, Sji)
  Wkj <- W*weights/rowSums(weights)
  penalty <- Pij-1
  Pi <- rowSums(P*penalty)
  act <- baseact + rowSums(Wkj*Sji) + Pi + noise
  return(act)
}


spreading_act <- function(Pij=matrix(c(1,1),1,2,T), S=mas){
	Sji <- S+log(Pij)
  Sji <- ifelse(Sji==-Inf | is.na(Sji), 0, Sji)
  ifelse(Sji < 0, print("!!! WARNING: Sji < 0 !!!"), T)
  # belowzeros <<- ifelse(Sji < 0, T, F)
  Sji <- ifelse(Sji < 0, 0, Sji)
  Sji
}

weight_spreading_act <- function(Sji=matrix(c(0,0),1,2,T), weights=matrix(c(1,1),1,2,T), W=ga){
	Wkj <- W*weights/rowSums(weights)
	rowSums(Wkj*Sji)
}

mismatch_penalty <- function(cuematch=matrix(c(1,1),1,2,T), P=mp){
	penalty <- cuematch-1
	# penalty <- ifelse(cuematch==0, -1, 0)
  Pi <- rowSums(P*penalty)
  Pi
}


base_act <- function(prom=0, times=lp, C1=pic, C2=psc, b=blc, noise=act_r_noise(ans), dec=bll){
  log(times^(-dec)) + b + (C2*(prom+C1))
}


match_quality <- function(match=matrix(c(1,1),1,2,T), sim=cuesim, blact=0, tau=rth, s=ans, q=qcf, q2=qco){
	qcorr <- 1/(1+q*exp(-(blact-tau-q2)))
	conf <- match[,c(2,1)]*(1+sim) 
	(match+conf) * qcorr
}


latency <- function(A, F=lf, f=le, tau=rth){
  t <- ifelse(A>=tau, F*exp(-f*A)*1000, F*exp(-f*tau)*1000)
  round(randomize_time(t))
}


act_r_noise_n <- function(n, s=ans){
  var <- pi^2/3*s^2
  rnorm(n, 0, sqrt(var))
}

act_r_noise <- function(s=ans){
  var <- pi^2/3*s^2
  rnorm(1, 0, sqrt(var))
}


## Random component ##
randomize_time <- function(time, n=rand_time){
  if(n>0) runif(length(time), time*(n-1)/n, time*(n+1)/n) else time
}


noise_off <- function(){
  ans <<- 0
  rand_time <<- 0
}

noise_on <- function(){
  ans <<- 0.15
  rand_time <<- 3
}



###############################################################
## Standard interference model with 4 conditions
###############################################################
model_4cond <- list(
	target_match = list(c(1,1), c(1,1), c(1,0), c(1,0)),
	distractor_match = list(c(0,1), c(0,0), c(0,1), c(0,0)),
	Target = c("Match","Match","Mismatch","Mismatch"),
	Distractor = c("Match","Mismatch","Match","Mismatch"),
	weights = list(c(strWeight(),semWeight()))
	)
