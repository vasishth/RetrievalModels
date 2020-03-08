source("model/act-s.r")
source("model/models.r")
library(dplyr)
options(dplyr.width=1000)


###############################################################
## Parameter space original model
###############################################################
reset_params()
cuesim <<- -1
pc <<- 0
# ndistr <<- c(1,3)

# lf <<- seq(0.1,0.7,.1)
# mp <<- seq(0,3,.5)
# mas <<- seq(0.5,4,.5)
# ans <<- seq(0,0.5,.1)
# dbl <<- c(-0.5,0,1,3)
# pspace_orig <- run(model_4cond, iterations=2000)
# #
# pspace_orig <- tbl_df(pspace_orig)
# glimpse(pspace_orig)
# #
# # means_cond_orig <- compute_means_cond(pspace_orig)
# means_int_orig <- compute_means_int(pspace_orig)
# means_int_orig_log <- mutate_each(means_int_orig, funs(convert2log))

lf <<- seq(0.1,0.2,.1)
mp <<- seq(0,3,.5)
mas <<- seq(1,5,.5)
ans <<- seq(0.1,0.5,.1)
# dbl <<- c(-0.5,0,1,3)
psp1 <- run(model_4cond, iterations=3000)
means1 <- compute_means_int(psp1)
#
lf <<- seq(0.3,0.5,.1)
mp <<- seq(0,3,.5)
mas <<- seq(1,5,.5)
ans <<- seq(0.1,0.5,.1)
# dbl <<- c(-0.5,0,1,3)
psp2 <- run(model_4cond, iterations=3000)
means2 <- compute_means_int(psp2)
means2$Set <- means2$Set+max(means1$Set)
means2 <- rbind(means1,means2)
#
means_int_orig <- means2
means_int_orig_log <- mutate_each(means_int_orig, funs(convert2log10))
#
require(tidyr)
means_int_orig_log_long <- gather(means_int_orig_log, "parameter", "value", lf, ans, mas, mp, dbl)


save(means_int_orig, means_int_orig_log, means_int_orig_log_long, file="pspace_original.Rd")




###############################################################
## Parameter space extended model, standard conditions
###############################################################
# load("pspace_extended_matrix.Rd")

## Prominence
reset_params()
ans <<- 0.15
mp <<- 1.5
mas <<- c(1,3)
lf <<- c(0.15,0.2,0.25)
dbl <<- seq(-0.5,4,0.25)
pc <<- c(0,5)
cuesim <<- -1
# ndistr <<- c(1,3)
#
pspace_prom <- run(model_4cond, iterations=3000)
pspace_prom <- tbl_df(pspace_prom)
glimpse(pspace_prom)

means_cond_prom <- compute_means_cond(pspace_prom)
means_int_prom <- compute_means_int(pspace_prom)
means_int_prom_log <- mutate_each(means_int_prom, funs(convert2log))


## Cue confusion
reset_params()
ans <<- 0.15
mp <<- 1.5
mas <<- 1
lf <<- c(0.15,0.2,0.25)
dbl <<- seq(-0.2,0.2,.2)
pc <<- 5
cuesim <<- seq(-1,0,0.1)
ndistr <<- c(1,3)
#
pspace_cueconf <- run(model_4cond, iterations=3000)
pspace_cueconf <- tbl_df(pspace_cueconf)
glimpse(pspace_cueconf)
#
means_cond_cueconf <- compute_means_cond(pspace_cueconf)
means_int_cueconf <- compute_means_int(pspace_cueconf)
means_int_cueconf_log <- mutate_each(means_int_cueconf, funs(convert2log))



# ## Simulations
# reset_params()
# ans <<- 0.15
# mp <<- 1.5
# mas <<- 1
# lf <<- c(0.15,0.2,0.25)
# dbl <<- seq(-0.5,4,0.1)
# pc <<- c(0,5)
# cuesim <<- seq(-1,0,0.1)
# ndistr <<- c(1,3)
# #
# simspace <- run(model_4cond, iterations=2000)
# simspace <- tbl_df(simspace)
# glimpse(simspace)
# #
# means_cond_simspace <- compute_means_cond(simspace)
# means_int_simspace <- compute_means_int(simspace)


# save(means_cond_prom, means_int_prom, means_cond_cueconf, means_int_cueconf, means_int_prom_log, means_int_cueconf_log, file="pspace_extended.Rd")









require(ggplot2)

(qplot(dbl, M, linetype=Target, geom=c("line"),
  xlab="Distractor base-level activation",
  ylab="Interference effect in ms",
  data=subset(means_int_ext, lf==0.15 & cuesim==-1 & ndistr==1)) 
+ geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) 
+ scale_y_continuous(breaks=seq(-300,50,20))
+ theme_bw()
)


summary(factor(means_int_ext$dbl))
summary(factor(subset(means_int_ext,lf==.15 & ndistr==1 & ans==0.15 & mp==1.5 & mas==1)$dbl))

(qplot(cuesim, M, linetype=Target, shape=factor(dbl), color=factor(dbl), geom=c("line","point"), 
  data=subset(means_int_ext, lf==.15 & ndistr==1 & factor(dbl)%in%c(-0.2,0,0.2)),
  xlab="Cue confusion in %",
  ylab="Mean latency difference in ms",
  )
+ geom_hline(aes(yintercept=0), colour="grey10")
# + geom_smooth()
+ geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) 
+ scale_colour_manual("", values=c("gray","black","gray"), guide=FALSE)
+ scale_y_continuous(breaks=seq(-60, 80, 20))
+ theme_bw()
)

















load("pspace_original.Rd")
require(ggplot2)
#
ylabs <- c(-300,-100,-20,-5,0,5,20,100)
#
(p.orig <- ggplot(aes(value, M, shape=Target, linetype=Target, color=Target),
  data=subset(means_int_orig_log_long, parameter!="dbl" & parameter%in%c("lf","ans","mp","mas")))
+ xlab("Parameter values")
+ ylab("Mean latency difference in ms")
+ geom_hline(aes(yintercept=0), colour="gray10")
+ geom_point(position=position_jitter(width=0.05), aes(color=Target), data=subset(means_int_orig_log_long, parameter%in%c("lf","ans")))
+ geom_point(position=position_jitter(width=0.5), aes(color=Target), data=subset(means_int_orig_log_long, parameter%in%c("mas","mp")))
# + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) 
+ geom_smooth(method=lm, formula=y~poly(x,3), col="black", fullrange=TRUE)
+ scale_colour_manual(values=c("gray70","gray50")) 
+ scale_shape_manual(values=c(1,2))
+ scale_y_continuous(breaks=convert2log10(ylabs), labels=ylabs)
+ theme_classic()
+ theme(legend.position="bottom"
  ,legend.key=element_blank()
  ,panel.grid.major.y=element_line(color="gray80")
  ,panel.grid.minor.y=element_blank()
  # ,panel.grid.major.x=element_line(color="gray80")
  ,panel.grid.major.x=element_blank()
  ,panel.grid.minor.x=element_blank()
  )
+ facet_wrap(~parameter, scales="free")
)




#load("pspace_original_matrix.Rd")
require(ggplot2)
#
head(means_int_orig)
library(tidyr)
orig <- subset(means_int_orig_log, dbl==0)
orig <- gather(orig, "parameter", "value", lf, ans, mas, mp, dbl)
nsets <- max(orig$Set)
#
(p.orig <- qplot(Target, M,
  ,geom=c("boxplot"),
  data=subset(orig, parameter!="dbl" & parameter%in%c("lf","ans","mp","mas")),
  xlab="",
  ylab="Mean latency difference in ms",
  )
+ geom_hline(aes(yintercept=0), colour="gray10")
+ scale_colour_manual("Target: ", values=c("black","gray50")) 
+ scale_shape_manual("Target: ", values=c(16,2))
+ theme_bw()
+ theme(legend.position="bottom"
  ,legend.key=element_blank()
  ,panel.grid.major.y=element_line(color="gray80")
  ,panel.grid.minor.y=element_blank()
  ,panel.grid.major.x=element_blank()
  ,panel.grid.minor.x=element_blank()
  )
# + facet_wrap(~parameter, scales="free")
)






## subset mismatch > 0
mispos <- subset(means_int_orig, Target=="Mismatch" & M>0 & mp<4)
lpos <- dim(mispos)[1]
l <- dim(subset(means_int_orig, Target=="Mismatch"))[1]
posprop <- lpos/l
Mpos <- mean(mispos$M)
mppos <- mean(mispos$mp)
anspos <- mean(mispos$ans)
lfpos <- mean(mispos$lf)
maspos <- mean(mispos$mas)
