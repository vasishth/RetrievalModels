library(dplyr)
library(tidyr)
library(ggplot2)

source("interACT.R")

rmsd <- function (obs, pred) {
	sqrt(mean((obs - pred)^2, na.rm = TRUE))
}

compute_int_means <- function(d){
	int <- select(filter(d, Distractor=="Match"), -Condition, -Distractor)
	dim(int)
	int$int <- filter(d, Distractor=="Match")$latency - filter(d, Distractor=="Mismatch")$latency
	#
	# means <- group_by(int, Set, Target, lf, ans, mas, mp, rth, bll, lp, ldp, blc, dbl, ndistr) %>% summarise(Effect=mean(int), SE=sd(int)/sqrt(length(int))) %>% ungroup() %>% mutate(lower=Effect-SE, upper=Effect+SE)
	# means
	means <- group_by(int, Set, Target, lf, ans, mas, mp, rth, bll, psc, pic, qcf, qco, cuesim, tprom, dprom, lp, ldp, blc, dbl, ndistr, cueweighting) %>% summarise(Effect=mean(int), SE=sd(int)/sqrt(length(int)), Sji_neg=sum(Sji_neg)) %>% ungroup() %>% mutate(lower=Effect-SE, upper=Effect+SE)
	means
}

convert2log <- function(x){
	ifelse(x>=1, log(x), ifelse(x<=-1, -log(abs(x)), 0))
}

convert2log10 <- function(x){
	x <- ifelse(x>-1 & x<1, 0, x)
	x <- ifelse(x<=-1, -log10(abs(x)), x) 
	x <- ifelse(x>=1, log10(abs(x)), x)
}

## this function does the work of estimating effect sizes:
iterate_lf <- function(values){
  maxset <- 0
  means <- NULL
  for(v in values){
    lf <<- v
    pmatr <- create_param_matrix(model_4cond, 1000) 
    results <- run(pmatr)
    means2 <- compute_int_means(results)
    means2$Set <- means2$Set+maxset
    means <- bind_rows(means, means2)
  }
  means
}

## we set the parameters:
reset_params()
psc <<- 0
qcf <<- 0
cuesim <<- -1
bll <<- 0.5
#mp <<- seq(0,3,1)
#mp <<-  seq(0.15,.35,0.1)
## default in Engelmann et al 2019 Cog Sci paper
mp <<- 0.15
#mas <<- seq(1,2,0.5)
## default in Engelmann et al 2019 Cog Sci paper
mas <<- 1.5
#mas<<-sort(rnorm(50,mean=1.5,sd=0.25))
#hist(mas)
#ans <<- seq(0.1,0.3,.1)
# default in Engelmann et al 2019 Cog Sci paper
ans <<- 0.2
##rth <<- seq(-2,-1,.5)
# default in Engelmann et al 2019 Cog Sci paper
rth <<-  -1.5
# dbl <<- seq(-2,2,1)
dbl <<- 0
#cueweighting <<- seq(1,2,by=0.5)
cueweighting <<- 1 ## must be estimated

latency_factor <<- seq(0.1,0.5,.1)
#latency_factor <<- sort(abs(rnorm(100,mean=0.3,sd=0.1)))

## for Engelmann Vasishth book:
# simulations using cuewt 1
means <- iterate_lf(latency_factor)
lv05pspaceEVcuewt1 <- means
save(lv05pspaceEVcuewt1, file="lv05pspaceEVcuewt1.Rd")

# simulations using cuewt 2
cueweighting <<- 2
means <- iterate_lf(latency_factor)
lv05pspaceEVcuewt2 <- means
save(lv05pspaceEVcuewt2, file="lv05pspaceEVcuewt2.Rd")

# simulations using cuewt 4
cueweighting <<- 4
means <- iterate_lf(latency_factor)
lv05pspaceEVcuewt4 <- means
save(lv05pspaceEVcuewt4, file="lv05pspaceEVcuewt4.Rd")
system("cp lv05pspaceEVcuewt* ../data/")

with(means,tapply(Effect,Target,mean))


## figures continued in c02SCCPVasishthEngelmann.Rnw


