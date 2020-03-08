library(dplyr)

###############################################################
## Global parameters
###############################################################
reset_params <- function(){
  lf        <<- 0.15       # latency factor
  le        <<- 1       # latency exponent
  rth 	      <<- -1.5    # retrieval threshold
  bll       <<- 0.5     # decay parameter
  ans       <<- 0.15    # activation noise
  mas       <<- 1       # maximum associative strength 
  mp        <<- 1.5       # mismatch penalty
  ga         <<- 1      # goal source activation
  rand_time <<- 3       # latency variability
  lp        <<- 1       # default time since last presentation (sec)
  blc       <<- 0       # base-level activation
  ##
  ## Distractor control:
  ldp       <<- 1       # last distractor presentation (sec)
  dbl       <<- 0       # distractor base-level
  ndistr    <<- 1				# number of distractors
  ##
  ## Cue weighting, cue confusion and activation-sensitivity:
  # cueweights <<- c(1,1) # vector of cue weightings
  cueweighting <<- 1     # Strength of structural cue as ratio str/sem
  normalizeWeights <<- TRUE 
  pc        <<- 0       # prominence correction factor C
  pco 	    <<- 1.3     # prominence correction offset x_0
  cuesim    <<- -1      # cue-feature similarity [-1..0]
  cl 				<<- 0 			# cue confusion level (0-100)
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
actrnames <- c("fan1","fan2","weights","bl1","bl2","times1","times2","noise1","noise2","blact1","blact2","fs1","fs2","act1","act2","activation","latency","retrieved","acc","miss","fail")
paramnames <- c("lf","le","rth","bll","ans","mas","mp","ga","rand_time","lp","blc","ldp","dbl","ndistr","cueweighting","pc","pco","cuesim","cl")



###############################################################
## RUN MODEL
###############################################################
#model <- model_4cond
run <- function(model, iterations=1000){
	##
	## PARAMETER MATRIX
	##
	print("creating parameter matrix...")
	#
	parameters <- list(lf,le,rth,bll,ans,mas,mp,ga,rand_time,lp,blc,ldp,dbl,ndistr,cueweighting,pc,pco,cuesim,cl)
	#
	n_params <- length(parameters);
	#
	## The total number of combinations is the product of the number of values
	## for each parameter
	n_sets <- prod(unlist(lapply(parameters, length)))
	n_cond <- length(model$target_fan)
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



	##
	## DATA TABLE
	##
	print("creating data table...")
	#
	condnames <- 1:length(model$target_fan)
	# c("lf","le","bll","ans","mas","mp","rth","rand_time","w","lp","ldp","blc","dbl","pc","pco","cl","cuesim","cueweighting","ndistr")
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
	d$fan1 <- rep(model$target_fan, each=n_sets)
	d$fan2 <- rep(model$distractor_fan, each=n_sets)
	d$Iteration <- rep(1:iterations, each=n_sets*n_cond)
	d$times1 <- d$lp
	d$times2 <- d$ldp
	d$bl1 <- d$blc
	d$bl2 <- d$dbl
	d$cl <- (cuesim+1)*100
	d$weights <- model$weights
	d$noise1 <- act_r_noise_n(total, d$ans)
	d$noise2 <- act_r_noise_n(total, d$ans)
	head(d)


	##
	## ACTIVATION
	##
	print("computing activations...")
	#
	fan1<-matrix(unlist(d$fan1),nrow=total,ncol=length(d$fan1[[1]]),byrow=TRUE)
	fan2<-matrix(unlist(d$fan2),nrow=total,ncol=length(d$fan1[[1]]),byrow=TRUE)
	weights<-matrix(unlist(d$weights),nrow=total,ncol=2,byrow=TRUE)
	#
	fan1 <- ifelse(fan1==0,NA,fan1)
	fan2 <- ifelse(fan2==0,NA,fan2)
	f1 <- ifelse(!is.na(fan1) & fan1>0, 1, 0)
	f2 <- ifelse(!is.na(fan2) & fan2>0, 1, 0)
	match1 <- f1-1
	match2 <- f2-1
	d$blact1 <- activation(fan=fan1, match=match1, bl=d$bl1, times=d$times1, weights=weights, noise=d$noise1, W=d$ga, dec=d$bll, S=d$mas, P=d$mp)
	d$blact2 <- activation(fan=fan2, match=match2, bl=d$bl2, times=d$times2, weights=weights, noise=d$noise2, W=d$ga, dec=d$bll, S=d$mas, P=d$mp)
	#
	d$fs1 <- fan_strength(d$blact1,d$blact2, C=d$pc, x0=d$pco)
	d$fs2 <- fan_strength(d$blact2,d$blact1, C=d$pc, x0=d$pco)
  #
  print("computing cue confusion and fan strength...")
  #
	cueconf1 <- cbind(f2[,-1],f2[,1])*(1+d$cuesim)
	ffan1 <- fan1*1+(f2+cueconf1)*d$ndistr*d$fs1
	cueconf2 <- cbind(f1[,-1],f1[,1])*(1+d$cuesim)
	ffan2 <- fan2*d$ndistr+(f1+cueconf2)*1*d$fs2
	# 
	d$act1 <- activation(fan=ffan1, match=match1, bl=d$bl1, times=d$times1, weights=weights, noise=d$noise1, W=d$ga, dec=d$bll, S=d$mas, P=d$mp)
	d$act2 <- activation(fan=ffan2, match=match2, bl=d$bl2, times=d$times2, weights=weights, noise=d$noise2, W=d$ga, dec=d$bll, S=d$mas, P=d$mp)
	

	##
	## FINAL VALUES
	##
	print("computing final values...")
	d$activation <- ifelse(d$act1>d$act2, d$act1, d$act2)
	retrieved <- ifelse(d$act1>d$act2, 1, 2)
	d$retrieved <- ifelse(d$activation>d$rth, retrieved, 0)
	d$latency <- latency(d$activation, F=d$lf, f=d$le, tau=d$rth)
	d$acc <- ifelse(d$retrieved==1, 1, 0)
	d$miss <- ifelse(d$retrieved==2, 1, 0)
	d$fail <- ifelse(d$retrieved==0, 1, 0)
	#
	print("FINISHED")
	#
	return(tbl_df(d))
}







###############################################################
## ACT-R
###############################################################
activation <- function(fan=matrix(c(1,1),1,2,T), match=matrix(c(0,0),1,2,T), weights=matrix(c(1,1),1,2,T), times=lp, bl=blc, noise=act_r_noise(ans), W=ga, dec=bll, S=mas, P=mp){
  Wkj <- W*weights/rowSums(weights)
  base_act <- log(times^(-dec)) + bl
  Sji <- S-log(fan)
  Sji <- ifelse(Sji==Inf | is.na(Sji), 0, Sji)
  ifelse(Sji < 0, print("!!! WARNING: Sji < 0 !!!"), T)
  Sji <- ifelse(Sji < 0, 0, Sji)
  Pi <- rowSums(P*match)
  act <- base_act + rowSums(Wkj*Sji) + Pi + noise
  return(act)
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


fan_strength <- function(a1, a2, C=pc, x0=pco){
  ifelse(C > 0, 1/(1+exp(-C*(x0-(a1-a2)))), 1)
}


noise_off <- function(){
  ans <<- 0
  rand_time <<- 0
}

noise_on <- function(){
  ans <<- 0.15
  rand_time <<- 3
}

