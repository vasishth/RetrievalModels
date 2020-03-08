# clean working directory
rm(list = ls(all = TRUE))
library(dplyr)
options(dplyr.width=1000)

source("model/act-s.r")
source("model/models.r")

ci <- function (x) {
	m <- mean(x, na.rm = TRUE)
	n <- length(x[!is.na(x)])
	s <- sd(x, na.rm = TRUE)
	upper <- m + qt(0.975, df = n - 1) * (s/sqrt(n))
	lower <- m + qt(0.025, df = n - 1) * (s/sqrt(n))
	return(data.frame(lower = lower, upper = upper))
}

rmsd <- function (obs, pred) {
	sqrt(mean((obs - pred)^2, na.rm = TRUE))
}


# x <- sapply(dat$ID, function(x) filter(dat, ID==x))[,31]
# x$ID
# x <- subset(dat, ID==30)
fit_data <- function(x){
	x <- data.frame(x)
	data <- x$M
	x1 <- x[1,]
	id <- x1$ID
	print(id)
	lf <<- x1$lf
	cuesim <<- x1$cuesim
	dbl <<- x1$dbl
	lp <<- x1$lp
	ldp <<- x1$ldp
	ans <<- x1$ans
	pc <<- 5
	dbl <<- seq(-0.5,4,0.25)
	if(cuesim!=-1) cuesim <<- seq(-0.8,-0.5,0.05)
	ndistr <<- as.numeric(as.character(x1$DistrNumb))
	subj <- x1$Subject
	saliency <- as.character(x1$Saliency)
	design <- x1$Design
	intType <- as.character(x1$IntType)
	#
	pspace <- run(model_4cond, 2000)
	means <- compute_means_int(pspace)
	# means$M <- ifelse(is.na(data), NA, means$M)
	means$data <- data
	means$M[is.na(means$data)] <- NA
	means$CI.lower[is.na(means$data)] <- NA
	means$CI.upper[is.na(means$data)] <- NA
	means$SE.lower[is.na(means$data)] <- NA
	means$SE.upper[is.na(means$data)] <- NA
	means$dev <- means$data - means$M
	# if(data[1]!=0) means$dev[means$Target=="Mismatch" & means$M < -150] <- 0.25*means$dev[means$Target=="Mismatch" & means$M < -150]
	if(data[1]!=0) means$dev <- ifelse(means$Target=="Mismatch" & !is.na(means$M) & means$M > -150, 0.25*means$dev, means$dev)
		# means$dev[means$Target=="Mismatch" & means$M < -150] <- 0.25*means$dev[means$Target=="Mismatch" & means$M < -150]

	# means$dev[means$Target=="Mismatch" & data[1]==0] <- 0
	means$dev[means$Target=="Mismatch" & !is.na(means$M) & means$data>0 & cuesim==-1 & means$M > -150] <- 0
	# rmsd2 <- 0
	# 	if(!is.na(data[2]) & res$means[2] < -150) rmsd2 <- 0.25*rmsd(data[2],res$means[2])
	# 	if(!is.na(data[2]) & data[1] == 0) rmsd2 <- 0.25*rmsd(data[2],res$means[2])
	#
	fit <- summarise(group_by(means, Set, dbl,cuesim), rmsd=sqrt(mean(dev^2, na.rm=T)))
	fit_s <- subset(fit, rmsd < min(rmsd)+2)
	fit_s2 <- subset(fit_s, abs(dbl) == min(abs(dbl)))
	fit_s3 <- subset(fit_s2, cuesim == min(cuesim))
	best_s  <- fit_s3$Set
	dbl <<- fit_s3$dbl
	cuesim <<- fit_s3$cuesim
	# best_s  <- with(fit, Set[rmsd==min(rmsd)])
	# dbl <<- filter(fit, Set==best_s)$dbl
	x$dbl <- dbl
	x$cuesim <- cuesim
	#
	pc <<- 0
	cuesim <<- -1
	pspace.o <- run(model_4cond, 2000)
	means.o <- compute_means_int(pspace.o)
	means.o$data <- data
	means.o$M <- ifelse(is.na(means.o$data), NA, means.o$M)
	means.o$CI.lower[is.na(means.o$data)] <- NA
	means.o$CI.upper[is.na(means.o$data)] <- NA
	means.o$SE.lower[is.na(means.o$data)] <- NA
	means.o$SE.upper[is.na(means.o$data)] <- NA
	#
	ids <- cbind(Experiment=paste(x1$ID, as.character(x1$Experiment), sep="-"), select(x, -Set, -Experiment, -M))
	simulations <<- rbind(simulations,
		cbind(ids, Set="Data", M=data, SE.lower=NA, SE.upper=NA, CI.lower=NA, CI.upper=NA),
		cbind(ids, Set="Original", select(ungroup(means.o), M, SE.lower, SE.upper, CI.lower, CI.upper)),
		cbind(ids, Set="Extended", select(ungroup(filter(means, Set==best_s)), M, SE.lower, SE.upper, CI.lower, CI.upper))
	)
	return(TRUE)
}

##############################################




##
## LOAD DATA
##
load("litdata.Rd")
head(lit)

##
## SUBSET DATA
##
dat1 <- tbl_df(subset(lit, Type!="Number agreement" & Set!="data2" & Method%in%c("SPR","ET")))
dat1 <- dplyr::rename(dat1, Target = Condition)
n_dat <- dim(dat1)[1]

parameters <- list(lf,le,rth,bll,ans,mas,mp,ga,rand_time,lp,blc,ldp,dbl,ndistr,cueweighting,pc,pco,cuesim,cl)
n_params <- length(parameters);
param_matrix <- matrix(unlist(parameters),nrow=n_dat, ncol=n_params, byrow=T);
colnames(param_matrix) <- paramnames
dat <- cbind(dat1,param_matrix)

##
## PRESET PARAMETERS
##
dat$lf <- 0.15
dat$mp <- 1.5
dat$mas <- 1
dat$ans <- 0.15
dat$pc <- 5
dat$cuesim <- -1
dat$dbl <- -1
dat$lp <- 1
dat$ldp <- 1
#
## Que confusion
# dat$cuesim[dat$Subtype=="Recipr"] <- -0.7
# dat$cuesim[dat$Lang=="CN"] <- -0.65
dat$cuesim[dat$ID==30] <- -0.7
dat$cuesim[dat$ID==17] <- -0.65
# dat$cuesim[dat$ID==18] <- 0
#
## Design
dat$Design <- "standard"
dat$Design[grep("ParkerPhillips", dat$Experiment)] <- "2FeatureMismatch"
#


##
## SIMULATE
##
reset_params()
pc <<- 5
pco <<- 1.3
mp <<- 1.5
mas <<- 1
#
dat <- subset(dat, ID!=13)
dat <- arrange(dat, ID, Target, Verb, Measure, AOI)
head(dat)
simulations <<- NULL
r <- lapply(unique(dat$ID), function(x) fit_data(filter(dat, ID==x)))
summary(simulations)
head(simulations)

save(simulations, file="simulations.Rd")






##
## PLOTS
##
require(reshape)
load("simulations.Rd")
#
simulations$i <- as.numeric(as.character(simulations$i))
simulations$ID <- as.numeric(as.character(simulations$ID))
simulations$M <- as.numeric(as.character(simulations$M))
simulations$CI.upper <- as.numeric(as.character(simulations$CI.upper))
simulations$CI.lower <- as.numeric(as.character(simulations$CI.lower))
simulations$Set <- factor(simulations$Set, levels=c("Data","Original","Extended"))
#
## Sort:
sims <- simulations[with(simulations, order(ID,Set,Target)), ]
## Select experiments:
sims <- subset(sims, !ID%in%c(3,4,5,10,13,27,54,55,61,63,65,69))
#
nExp <- length(unique(sims$Experiment))
sims$i <- NULL
i <- data.frame(i=1:nExp,ID=unique(sims$ID))
sims <- merge(sims,i,by="ID")
sims <- sims[with(sims, order(i,Set,Target)), ]


require(ggplot2)
require(grid)
#
idmap <- function(id){
	unique(sims$i[sims$ID%in%unique(id)])
}
#
anY0 <- -210
anY1 <- anY0+13
anY2 <- anY1+13
anY3 <- anY2+13
anY4 <- anY3+13
anY5 <- 55
#
(p.sims <- ggplot(sims, aes(x=i, y=M, group=Target), shape=21# size=2.5, #fill=I("gray"), 
	)
+ xlab("")
+ ylab("Interference effect in ms")
+ geom_point(data=subset(sims, Set=="Data"), aes(shape=Target), col="black", size=2.9, position=position_dodge(0.8))
+ geom_bar(data=subset(sims, Set=="Data"), stat="identity", width=I(0.15), fill=I("gray"), position=position_dodge(0.8))
# + geom_bar(data=subset(sims, Set=="Data" & Significance=="marg"), stat="identity", width=I(0.15), fill=I("gray"), position=position_dodge(0.8))
+ geom_hline(aes(yintercept=0), colour="gray40", lwd=.4)
+ geom_point(data=subset(sims, Target=="Match"), aes(x=i-.2, fill=Set, col=Set, size=Set), shape=21)
+ geom_point(data=subset(sims, Target=="Mismatch"), aes(x=i+.2, fill=Set, col=Set, size=Set), shape=24, show_guide=FALSE)
+ geom_point(data=subset(sims, Set=="Data" & Target=="Match" & Significance=="marg"), aes(x=i-.2,y=anY5), shape="?", size=2.5)
+ geom_point(data=subset(sims, Set=="Data" & Target=="Mismatch" & Significance=="marg"), aes(x=i+.2,y=anY5), shape="?", size=2.5)
+ scale_shape_manual("Target: ", values=c(21,24), labels=c("Match","Mismatch"))
+ scale_fill_manual("", values=c("gray","black",NA))
+ scale_colour_manual("", values=c("gray","black","black"))
+ scale_size_manual("", values=c(3.2,1.8,3))
# + guides(shape=guide_legend(order=1))
# + geom_errorbar(data=subset(sims,set!="Data"), aes(min=CI.lower, max=CI.upper, width=0.6), size=0.3,position=position_dodge(0.8))
+ theme_bw()
+ theme(legend.position="bottom"
	,legend.key=element_blank()
	,panel.grid.minor.y=element_blank()
	,panel.grid.minor.x=element_line(color="gray80")
	,panel.grid.major.x=element_blank()
	,axis.text.x = element_text(angle = 45, hjust = 1)
	,plot.margin = unit(c(0.5, 0.5, 0.5, 1.5), "cm")
	)
+ coord_cartesian(xlim=c(0.51,nExp+0.49), ylim=c(-220,65))
+ scale_y_continuous(breaks=seq(-150, 100, 25))
+ scale_x_continuous(labels=unique(sims$Experiment), breaks=seq(1,nExp, 1))
+ annotate("text", x=1.8,y=anY0,label="Reflexives >>", size=2.5)
# + annotate("text", x=19,y=anY1,label="Prep.", size=2.5)
+ annotate("text", x=idmap(25), y=anY0,label="Prep", size=2.5)
# + annotate("text", x=20.5,y=anY1,label="Possess.", size=2.5)
+ annotate("text", x=idmap(28), y=anY0,label="Poss.", size=2.5)
# + annotate("text", x=22.5,y=anY1,label="Reciprocals", size=2.5)
+ annotate("text", x=idmap(29)+0.5, y=anY0,label="Reciprocals", size=2.5)
+ annotate("text", x=idmap(56)+1,y=anY0,label="Subj-Verb >>", size=2.5)
# + annotate("text", x=idmap(c(17,22)),y=anY1,label="CN", size=2.5)
+ annotate("text", x=unique(sims$i), y=anY1,label=sims$Lang[sims$Target=="Match" & sims$Set=="Data"], size=2.5)
+ annotate("text", x=idmap(sims$ID[sims$Method=="ET"]), y=anY2,label="ET", size=2.5)
+ annotate("text", x=idmap(sims$ID[sims$Method=="SPR"]), y=anY2,label="SPR", size=2.5)
+ annotate("text", x=idmap(sims$ID[sims$IntType=="pro"]), y=anY3,label="pro", size=2.5)
+ annotate("text", x=idmap(sims$ID[sims$IntType=="retro"]), y=anY3,label="retro", size=2.5)
+ annotate("text", x=idmap(sims$ID[sims$IntType=="memory"]), y=anY3,label="mem", size=2.5)
+ annotate("text", x=idmap(sims$ID[sims$Subject & sims$Saliency=="topic"]), y=anY4,label="S/T", size=2.5)
+ annotate("text", x=idmap(sims$ID[sims$Subject & sims$Saliency!="topic"]), y=anY4,label="S", size=2.5)
+ annotate("text", x=idmap(sims$ID[!sims$Subject & sims$Saliency=="topic"]), y=anY4,label="T", size=2.5)
# + annotate("text", x=idmap(d1$ID[d1$IntType=="pro"]), y=anY3,label="pro", size=2.5)
)





# library(tidyr)
# params1 <- sims %>%  
# 	filter(Set=="Extended") %>% 
# 	gather("variable", "value", lf,cl,dbl) %>% 
# 	select(i, ID, variable, value) %>% 
# 	unique()

params <- unique(melt(filter(sims, Set=="Extended"), i=c("i","ID"), measure=c("lf","cl","dbl")))
params$i <- as.numeric(as.vector(params$i))
params$value <- as.numeric(as.vector(params$value))
#
(p.params <- qplot(i, value, #shape=variable, #color=variable, #linetype=I(3), 
	geom=c("bar"), #size=I(3), 
	# lwd=I(.5),
	# col=I("black"),
	fill=I("gray"),
  xlab="",
  ylab="Distr. base-level",
  stat="identity",
  data=subset(params,variable%in%c("dbl")))
+ geom_hline(aes(yintercept=0), colour="gray40", lwd=.4)
# + facet_grid(variable ~., scales="free")
+ geom_point(size=2, shape=1)
+ theme_bw() 
+ theme(legend.position="right"
	,legend.key=element_blank()
	,panel.grid.minor.y=element_blank()
	,panel.grid.minor.x=element_blank()
	,strip.background=element_rect(color="white", fill=NA)
	,axis.text.x = element_text(angle = 45, hjust = 1)
	,plot.margin = unit(c(0.5, 0.5, 0.5, 1), "cm")
	)
# + coord_cartesian(xlim=c(0.51,nExp+0.49), ylim=c(-220,80))
+ scale_y_continuous(breaks=seq(-.5, 3.5, .5))
+ scale_x_continuous(labels=unique(sims$Experiment), breaks=seq(1,nExp, 1), limits=c(0,nExp+.5))
)






## Linear model distractor base-line
# lm1 <- with(droplevels(subset(d1)), lm(DBL~PromClass)+Lang+Method+IntType+TypeClass))
simulations$IntType <- factor(simulations$IntType, levels=c("pro","retro","memory"))
lm1 <- with(droplevels(subset(simulations,Target=="Match" & Set=="Data")), lm(dbl~ordered(as.factor(PromClass))+Lang+Method+ordered(IntType)+TypeClass))
summary(lm1)
lmcoefProm <- round(summary(lm1)$coefficients[2,],digits=2)
lmcoefType <- round(summary(lm1)$coefficients[9,],digits=2)

library(lme4)
lm2 <- with(droplevels(subset(simulations,Target=="Match" & Set=="Data")), lmer(dbl~ordered(as.factor(PromClass))+Lang+Method+ordered(IntType)+(1|TypeClass)))
summary(lm2)

## RMSE
simulations$M <- as.numeric(simulations$M)
e <- c(
with(subset(simulations), rmsd(M[Set=="Data"],M[Set=="Original"])),
with(subset(simulations, Target=="Match"), rmsd(M[Set=="Data"],M[Set=="Original"])),
with(subset(simulations, Target=="Mismatch"), rmsd(M[Set=="Data"],M[Set=="Original"])),
with(subset(simulations), rmsd(M[Set=="Data"],M[Set=="Extended"])),
with(subset(simulations, Target=="Match"), rmsd(M[Set=="Data"],M[Set=="Extended"])),
with(subset(simulations, Target=="Mismatch"), rmsd(M[Set=="Data"],M[Set=="Extended"]))
)
e

## Correlation
r <- c(
with(subset(simulations,!is.na(M)), cor(M[Set=="Data"],M[Set=="Original"])),
with(subset(simulations, !is.na(M) & Target=="Match"), cor(M[Set=="Data"],M[Set=="Original"])),
with(subset(simulations, !is.na(M) & Target=="Mismatch"), cor(M[Set=="Data"],M[Set=="Original"])),
with(subset(simulations,!is.na(M)), cor(M[Set=="Data"],M[Set=="Extended"])),
with(subset(simulations, !is.na(M) & Target=="Match"), cor(M[Set=="Data"],M[Set=="Extended"])),
with(subset(simulations, !is.na(M) & Target=="Mismatch"), cor(M[Set=="Data"],M[Set=="Extended"]))
)
r

s2 <- simulations
s2$DBL[s2$Target=="Match" & s2$PromClass==2 & s2$M==0] <- 2.25
