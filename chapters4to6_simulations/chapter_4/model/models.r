


#################################################################
## Standard interference model with 4 conditions
#################################################################
model_4cond <- list(
	target_fan = list(c(1,1), c(1,1), c(1,NA), c(1,NA)),
	distractor_fan = list(c(NA,1), c(NA,NA), c(NA,1), c(NA,NA)),
	Target = c("Match","Match","Mismatch","Mismatch"),
	Distractor = c("Match","Mismatch","Match","Mismatch"),
	weights = list(c(strWeight(),semWeight()))
	)






###############################################################
## FUNCTIONS
###############################################################
compute_means_cond <- function(d){
summarise(group_by(d, Set, Condition, Target, Distractor, lf, ans, mas, mp, ga, lp, ldp, dbl, ndistr, cueweighting, pc, pco, cuesim, cl), M=mean(latency), N=length(latency), SD=sd(latency), SE=sd(latency)/sqrt(length(latency)), SE.lower=M-SE, SE.upper=M+SE, CI.lower=ci(latency)$lower, CI.uppper=ci(latency)$upper, retrieved=mean(retrieved), acc=mean(acc), miss=mean(miss), fail=mean(fail), activation=mean(activation))
}

compute_means_int <- function(d){
interference <- select(filter(d, Distractor=="Match"), -Condition, -Distractor)
interference$interference <- filter(d, Distractor=="Match")$latency - filter(d, Distractor=="Mismatch")$latency
#
summarise(group_by(interference, Set, Target, lf, ans, mas, mp, ga, lp, ldp, dbl, ndistr, cueweighting, pc, pco, cuesim, cl), M=mean(interference), N=length(interference), SD=sd(interference), SE=sd(interference)/sqrt(length(interference)), SE.lower=M-SE, SE.upper=M+SE, CI.lower=ci(interference)$lower, CI.upper=ci(interference)$upper)
}


convert2log <- function(x){
	ifelse(x>=1, log(x), ifelse(x<=-1, -log(abs(x)), 0))
}

convert2log10 <- function(x){
	ifelse(x>=1, log10(x), ifelse(x<=-1, -log10(abs(x)), 0))
}
