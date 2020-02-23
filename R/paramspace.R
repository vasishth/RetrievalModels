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
	means <- group_by(int, Set, Target, lf, ans, mas, mp, rth, bll, psc, pic, qcf, qco, cuesim, tprom, dprom, lp, ldp, blc, dbl, ndistr) %>% summarise(Effect=mean(int), SE=sd(int)/sqrt(length(int)), Sji_neg=sum(Sji_neg)) %>% ungroup() %>% mutate(lower=Effect-SE, upper=Effect+SE)
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

iterate_lf <- function(values){
  maxset <- 0
  means <- NULL
  for(v in values){
    lf <<- v
    pmatr <- create_param_matrix(model_4cond, 6000) 
    results <- run(pmatr)
    means2 <- compute_int_means(results)
    means2$Set <- means2$Set+maxset
    means <- bind_rows(means, means2)
  }
  means
}

reset_params()
psc <<- 0
qcf <<- 0
cuesim <<- -1
bll <<- 0.5
#mp <<- seq(0,3,1)
mp <<-  seq(0.15,.35,0.1)
#mas <<- seq(1,2,0.5)
mas <<- 1.5
ans <<- seq(0.1,0.3,.1)
rth <<- seq(-2,-1,.5)
# dbl <<- seq(-2,2,1)
dbl <<- 0

lf <<- seq(0.1,0.4,.01)
means <- iterate_lf(lf)

unique(means$lf)

# meansl <- filter(means, mp<=2 & dbl==0) %>% gather(Parameter, Value, lf,ans, mas, dbl, rth, mp) %>% mutate(logEffect = convert2log10(Effect))
# # 
# (ggplot(data=subset(meansl), aes(Value, Effect, shape=Target, linetype=Target, color=Target))
# + geom_point(position=position_jitter(width=0.05))
# + scale_colour_manual(values=c("black","gray"))
# + geom_hline(aes(yintercept=0), colour="gray10")
# + geom_smooth(method=lm, formula=y~poly(x,3), col="black", fullrange=TRUE, se=TRUE)
# + facet_wrap(~Parameter, scales="free")
# + ylim(-100,50)
# + theme_bw()
# )

## for Engelmann Vasishth book:
summary(means$mas)
unique(means$mas)
unique(means$rth)
summary(means$mp)
unique(means$mp)
summary(means$lf)

lv05pspaceEV <- means
save(lv05pspaceEV, file="../data/lv05pspaceEV.Rd")






# ##
# ## BASE-LEVEL
# ##
# reset_params()
# lf <<- seq(0.1,0.4,.1)
# mp <<- seq(0,1,1)
# mas <<- seq(1,3,1)
# ans <<- seq(0.1,0.2,.1)
# dbl <<- seq(-2.5,6,.5)
# results <- run(model_4cond, 2000)
# means <- compute_int_means(results)

# # meansl <- gather(means, Parameter, Value, dbl)
# # meansl <- mutate(meansl, logValue = convert2log10(Value))
# # (ggplot(data=subset(meansl), aes(Value, Effect, shape=Target, linetype=Target, color=Target))
# # + geom_point(position=position_jitter(width=0.05))
# # + scale_colour_manual(values=c("black","gray"))
# # + geom_hline(aes(yintercept=0), colour="gray10")
# # + geom_smooth(method=lm, formula=y~poly(x,3), col="black", fullrange=TRUE, se=TRUE)
# # + facet_wrap(~Parameter, scales="free")
# # + ylim(-100,50)
# # + theme_bw()
# # )

# lv05bl <- means
# save(lv05bl, file="lv05bl.Rd")


## plotting

library(ggplot2)

load("data/lv05pspace.Rd")

mismatch<-subset(means,Target=='Target-Mismatch')
dim(mismatch)

match<-subset(means,Target=='Target-Match')


match_reduced<-match[,c(3,4,5,6,7,21)]
match_reduced$ans<-factor(match_reduced$ans)
levels(match_reduced$ans)[levels(match_reduced$ans)==0.1]  <- "Noise: 0.1"
levels(match_reduced$ans)[levels(match_reduced$ans)==0.2]  <- "Noise: 0.2"
levels(match_reduced$ans)[levels(match_reduced$ans)==0.3]  <- "Noise: 0.3"

match_reduced$mp<-factor(match_reduced$mp)
levels(match_reduced$mp)[levels(match_reduced$mp)=="0"]  <- "Mismatch penalty: 0"
levels(match_reduced$mp)[levels(match_reduced$mp)=="1"]  <- "Mismatch penalty: 1"
levels(match_reduced$mp)[levels(match_reduced$mp)=="2"]  <- "Mismatch penalty: 2"

match_reduced$ret_threshold<-factor(match_reduced$rth)

plot_match<-ggplot(match_reduced, aes(x=LF, y=Effect, 
                                      shape=ret_threshold, 
                                      color = ret_threshold)) + 
  geom_point()+
  facet_wrap( ~ factor(ans)+factor(mp), nrow=3)+
  theme_bw()+scale_colour_manual(values=c("pink","red", "orange","blue","gray","black"))



plot_match

