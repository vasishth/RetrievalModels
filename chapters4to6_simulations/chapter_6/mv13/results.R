#+ setup, echo=F, include=F
source("~/.Rprofile")
library(xtable)
library(gdata)
library(reshape)
library(MASS)
library(ggplot2)
library(em2)
library(lme4)
library(knitr)
setwd("data")
opts_chunk$set(fig.path='plot-')
## Use: spin("results.R", format="Rnw"); knit2pdf("results.Rnw")
## or: spin("results.R", format="Rmd")


#nrmsd <- function(data, model, sds) sqrt(mean(((data - model)/sds)^2))

#+ input, echo=F, include=F
##
## Input
##
f <- read.table("fixations.txt", header=T)
colnames(f) <- c("exp","iteration","cond","pos","word","dur")
# head(f)
# str(f); head(f,50); summary(f)
#xtabs(~cond+exp,f)
f$iteration <- as.factor(as.character(f$iteration))

## with pre-V region 10:12
f2 <- f
f2$pos[f2$pos%in%10:12] <- 10
head(f2,20)
f2$pos[f2$pos>10] <- f2$pos[f2$pos>10]-2

etm <- with(f, em2(pos, dur, data.frame(exp, iteration, cond)))
etm2 <- with(f2, em2(pos, dur, data.frame(exp, iteration, cond)))

# head(etm, 100)
# summary(etm)
save(etm, file="sim-etm.RData")



##
## ADDITIONAL MEASURES
##
m <- etm
m2 <- etm2
m$fp_reg <- ifelse(m$RBRC>0,1,0) ## first-pass regression prob
m$skip<-ifelse(m$FPRT==0,1,0)  ## skipping prob
m$reread<-ifelse(m$RRT>0,1,0)  ## re-reading prob
m$refix <- ifelse(m$FPRT>m$FFD,1,0)  ## re-fixation prob
#m$onefix <- ifelse((m$SFD>0 & m$FFP==1),1,0)
# head(m)
m$skip.prev <- factor(c(m$skip[dim(m)[1]],m$skip[1:dim(m)[1]-1]))
m$skip.next <- factor(c(m$skip[2:dim(m)[1]], m$skip[1]))
m$reg.prev <- factor(c(m$fp_reg[dim(m)[1]],m$fp_reg[1:dim(m)[1]-1]))
m$reg.next <- factor(c(m$fp_reg[2:dim(m)[1]], m$fp_reg[1]))
# head(m)


m$amb <- ifelse(m$cond=="UNAMB", "unambiguous", "ambiguous")




#+ otherinput, echo=F, include=F, warning=F
##
## OTHER INFO
##
## encoding times ##
enc <- read.table("enctimes.txt", header=TRUE)
# head(enc,20); dim(enc)
colnames(enc) <- c("exp","iteration","cond","roi","word","enc","ecc","freq1")
# Only store first encoding of each word
enc$trialroi <- paste(enc$exp, enc$iteration, enc$cond, enc$roi); #length(enc$trialroi)
enc$dupl <- duplicated(enc$trialroi)
enc1 <- subset(enc, !dupl); # head(enc1[-c(9,10)]); dim(enc1)
m <- merge(m, enc1[-c(9,10)], by=c("exp","iteration","cond","roi"), all.x=TRUE)

## time outs ##
tmo <- read.table("timeouts.txt", header=TRUE)
# head(tmo)

## attachment times ##
retr <- read.table("attachments.txt", header=TRUE)
colnames(retr) <- c("exp","iteration","cond","roi","word","retr")
retr$retr <- as.integer(retr$retr)

# Only store first encoding of each word
retr$trialroi <- paste(retr$exp, retr$iteration, retr$cond, retr$roi); #length(retr$trialroi)
retr$dupl <- duplicated(retr$trialroi)
retr1 <- subset(retr, !dupl); # head(retr1); dim(retr1)
# head(retr1,20)
# head(retr[-c(5,7,8)])
m <- merge(m, retr1[-c(5,7,8)], by=c("exp","iteration","cond","roi"), all.x=TRUE, all.y=FALSE)
# head(m)
m$retr[is.na(m$retr)] <- 20

## trial messages
msg <- read.table("trialmessages.txt", header=F)
colnames(msg) <- c("exp","iteration","cond","pos","word","var","val")
msg <- reshape(msg, idvar = c("exp","iteration","cond","pos","word"), timevar="var", direction="wide")
msg$attached <- ifelse(is.na(msg$val.attachment),0,1)
msg$timeout <- ifelse(is.na(msg$val.timeout), 0, 1)
msg$revision <- ifelse(!is.na(msg$val.revision), 1, 0)
msg$s.reread <- ifelse(msg$val.regression=="reread", 1, 0)
msg$s.reread[is.na(msg$s.reread)] <- 0
msg$fail <- 0
msg$fail[msg$val.fail=="T"] <- 1
# head(msg) # summary(msg)




trialinfo <- unique(m[1:3])
trialinfo <- merge(trialinfo, subset(msg[c('exp','iteration','cond','attached')], attached==1), by=c("exp","iteration","cond"), all.x=TRUE, all.y=TRUE)
trialinfo <- merge(trialinfo, subset(msg[c('exp','iteration','cond','revision')], revision==1), by=c("exp","iteration","cond"), all.x=TRUE, all.y=TRUE)
trialinfo <- merge(trialinfo, subset(msg[c('exp','iteration','cond','s.reread')], s.reread==1), by=c("exp","iteration","cond"), all.x=TRUE, all.y=TRUE)
trialinfo <- merge(trialinfo, subset(msg[c('exp','iteration','cond','fail')], fail==1), by=c("exp","iteration","cond"), all.x=TRUE, all.y=TRUE)
trialinfo[is.na(trialinfo)] <- 0
trialinfo <- merge(trialinfo, subset(msg[c('exp','iteration','cond','val.attachment')], !is.na(val.attachment)), by=c("exp","iteration","cond"), all.x=T, all.y=T)
trialinfo <- drop.levels(trialinfo)
# head(trialinfo); summary(trialinfo)

## subject info ##
subjects <- read.table("subjects.txt", header=F)
colnames(subjects) <- c("exp","subj","ga")
# subjects$wmc <- as.factor(ifelse(subjects$ga>1,"highWMC","lowWMC"))
#subjects$wmc <- as.factor(ifelse(subjects$ga<1,"lowWMC","highWMC"))
subjects$wmc <- factor(ifelse(subjects$ga >= median(subjects$ga), "high WMC", "low WMC"))
summary(subjects$wmc)
trialinfo <- merge(trialinfo, subjects, by=c("exp"), all.x=T, all.y=T)

## Accuracy ##
trialinfo$acc <- 0
trialinfo$acc[trialinfo$cond=="HIGH" & trialinfo$val.attachment=="high"] <- 1
trialinfo$acc[trialinfo$cond=="HIGH" & trialinfo$revision==1] <- 1
trialinfo$acc[trialinfo$cond=="LOW" & trialinfo$val.attachment=="low"] <- 1
trialinfo$acc[trialinfo$cond=="LOW" & trialinfo$revision==1] <- 1
trialinfo$acc[trialinfo$cond=="UNAMB" & trialinfo$val.attachment=="low"] <- 1
trialinfo$acc[trialinfo$cond=="UNAMB" & trialinfo$revision==1] <- 1
trialinfo$acc[trialinfo$fail==1] <- 0

trialinfo$acc2 <- trialinfo$acc
trialinfo$acc2[trialinfo$attached==0 & trialinfo$fail==0 & trialinfo$cond=="HIGH"] <- .25
trialinfo$acc2[trialinfo$attached==0 & trialinfo$fail==0 & trialinfo$cond!="HIGH"] <- .75
trialinfo$acc2[trialinfo$fail==1] <- .5

dim(m)
m <- merge(m, trialinfo, by=c("exp","iteration","cond"), all.x=TRUE)
dim(m)

colnames(m)[4] <- "pos"
m <- merge(m, msg[c(1:4,12)], by=c("exp","iteration","cond","pos"), all.x=TRUE)
m$timeout[is.na(m$timeout)] <- 0
#head(m)

m$enc.prev <- c(m$enc[dim(m)[1]],m$enc[1:dim(m)[1]-1])
m$enc.next <- c(m$enc[2:dim(m)[1]], m$enc[1])


#+ ROIs, include=F
##
## REGIONS OF INTEREST
##
#m$roi <- as.vector(as.numeric(m.means$roi))
m$roi[m$pos %in% 1:3] <- "Matrix"
m$roi[m$pos %in% 4:8] <- "Compl"
m$roi[m$pos == 9] <- "pre-conj"
m$roi[m$pos == 10] <- "CONJ"
m$roi[m$pos == 11] <- "DET"
m$roi[m$pos == 12] <- "N"
m$roi[m$pos == 13] <- "V"
m$roi[m$pos == 14] <- "SP1"
m$roi[m$pos == 15] <- "SP2"
m$roi[m$pos %in% 16:18] <- "SP3"
# m$roi[!m$pos %in% 3:10] <- "other"
m$roi <- factor(m$roi, levels=c("Matrix", "Compl", "pre-conj", "CONJ", "DET", "N", "V","SP1", "SP2", "SP3"))
# levels(m$roi)
roilabels <- c("Compl", "pre-conj", "CONJ", "DET", "N", "V","SP1", "SP2", "SP3")

m$roi2 <- as.vector(m$roi)
m$roi2[m$pos %in% 10:12] <- "pre-V"
m$roi2 <- factor(m$roi2, levels=c("Matrix", "Compl", "pre-conj", "pre-V", "V","SP1", "SP2", "SP3"))


#+ Save, echo=F
save(m, file="m.RData")
save(trialinfo, file="m.trials.RData")
save(subjects, file="m.subjects.RData")


##'
##' DATA CLEAN-UP
##'
#+ clean-up
#load("m.RData")
m.all <- m
m <- subset(m, fail==0)
m <- subset(m, pos!=1 & word!="*")

m$trial <- paste(m$exp, m$iteration, m$cond)
(n.trials <- length(unique(m$trial)))
m$trialpos <- paste(m$exp, m$iteration, m$cond, m$pos)
(length(unique(m$trialpos)))
# head(m)
m$dupl <- duplicated(m$trialpos)
dim(subset(m,dupl))




# # remove trials where si is skipped
dim(m)
skip9.11 <- subset(m, pos%in%9:11 & skip==1)
skip9.11.trials <- unique(skip9.11$trial)
length(skip9.11.trials)/n.trials
# m <- subset(m, !trial%in%skip9.11.trials)
# dim(m)
# skipconj <- subset(m, pos==10 & skip==1)
# skipconj.trials <- unique(skipconj$trial)
# length(skipconj.trials)/n.trials
# # m <- subset(m, !trial%in%skipconj.trials)
# dim(m)
# skipdet <- subset(m, pos==11 & skip==1)
# skipdet.trials <- unique(skipdet$trial)
# length(skipdet.trials)/n.trials
# # m <- subset(m, !trial%in%skipdet.trials)
# # dim(m)



summary(m$enc)
summary(m$retr)
summary(m$FPRT)
summary(m$TFT)
#boxplot(m$enc)
#m <- subset(m, enc < 2*mean(m$enc))




#+ means, echo=F, include=F, warning=F
##
## MEANS
##
# head(m)
# head(trialinfo)
# head(subjects)
n.subj <- length(levels(m$exp))
n.it <- length(levels(m$iteration))
m$FPRT.corr <- 0
library(car)
m$FPRT.corr <- m$FPRT

#lmm1 <- lmer(log(FPRT) ~ (skip.prev + skip.next + factor(fp_reg) + log(enc) + enc.next + scale(log(retr),sc=F))*factor(amb)*scale(ga,scale=F) + (1|exp) + (1|iteration), data=subset(m, pos==9 & FPRT>0)); summary(lmm1)


# # # str(m); head(m)
# boxcox(FPRT ~ skip.prev + skip.next + factor(fp_reg), data=subset(m,pos==9 & FPRT>0 & cond=="UNAMB"))
# lm1 <- lm(log(FPRT) ~ skip.prev + skip.next + factor(fp_reg), data=subset(m, pos==9 & FPRT>0 & cond=="UNAMB")); summary(lm1)
# qqPlot(residuals(lm1))
# effect <- predict(lm1) - coef(lm1)[1]
# m$FPRT.corr[m$pos==9 & m$FPRT>0 & m$cond=="UNAMB"] <- exp(log(m$FPRT[m$pos==9 & m$FPRT>0 & m$cond=="UNAMB"])-effect)

# boxcox(FPRT ~ skip.prev + skip.next + factor(fp_reg), data=subset(m,pos==9 & FPRT>0 & cond!="UNAMB"))
# lm2 <- lm(log(FPRT) ~ skip.prev + skip.next + factor(fp_reg) , data=subset(m, pos==9 & FPRT>0 & cond!="UNAMB")); summary(lm2)
# qqPlot(residuals(lm2))
# effect <- predict(lm2) - coef(lm2)[1]
# m$FPRT.corr[m$pos==9 & m$FPRT>0 & m$cond!="UNAMB"] <- exp(log(m$FPRT[m$pos==9 & m$FPRT>0 & m$cond!="UNAMB"])-effect)

par(mfrow=c(2,2), cex=0.7)
## pos 10 (CONJ)
boxcox(FPRT ~ skip.prev + skip.next, data=subset(m,pos==10 & FPRT>0 & cond=="UNAMB"))
lm3 <- lm((FPRT)^-.5 ~ skip.prev + skip.next, data=subset(m, pos==10 & FPRT>0 & cond=="UNAMB")); summary(lm3)
qqPlot(residuals(lm3))
effect <- predict(lm3) - coef(lm3)[1]
m$FPRT.corr[m$pos==10 & m$FPRT>0 & m$cond=="UNAMB"] <- ((m$FPRT[m$pos==10 & m$FPRT>0 & m$cond=="UNAMB"]^-.5)-effect)^-2

boxcox(FPRT ~ skip.prev + skip.next , data=subset(m,pos==10 & FPRT>0 & cond!="UNAMB"))
lm4 <- lm(sqrt(FPRT) ~ skip.prev + skip.next, data=subset(m, pos==10 & FPRT>0 & cond!="UNAMB")); summary(lm4)
qqPlot(residuals(lm4))
effect <- predict(lm4) - coef(lm4)[1]
m$FPRT.corr[m$pos==10 & m$FPRT>0 & m$cond!="UNAMB"] <- ((m$FPRT[m$pos==10 & m$FPRT>0 & m$cond!="UNAMB"]^.5)-effect)^2


# ## pos 11
# boxcox(FPRT ~ skip.prev + skip.next, data=subset(m,pos==11 & FPRT>0 & cond=="UNAMB"))
# lm5 <- lm(log(FPRT) ~ skip.prev + skip.next, data=subset(m, pos==11 & FPRT>0 & cond=="UNAMB")); summary(lm5)
# qqPlot(residuals(lm5))
# effect <- predict(lm5) - coef(lm5)[1]
# m$FPRT.corr[m$pos==11 & m$FPRT>0 & m$cond=="UNAMB"] <- exp(log(m$FPRT[m$pos==11 & m$FPRT>0 & m$cond=="UNAMB"])-effect)

# boxcox(FPRT ~ skip.prev + skip.next + reg.prev + factor(fp_reg), data=subset(m,pos==11 & FPRT>0 & cond!="UNAMB"))
# lm6 <- lm(log(FPRT) ~ skip.prev + skip.next + factor(fp_reg) , data=subset(m, pos==11 & FPRT>0 & cond!="UNAMB")); summary(lm6)
# qqPlot(residuals(lm6))
# effect <- predict(lm6) - coef(lm6)[1]
# m$FPRT.corr[m$pos==11 & m$FPRT>0 & m$cond!="UNAMB"] <- exp(log(m$FPRT[m$pos==11 & m$FPRT>0 & m$cond!="UNAMB"])-effect)


# ## pos 12
# boxcox(FPRT ~ skip.prev + skip.next, data=subset(m,pos==12 & FPRT>0 & cond=="UNAMB"))
# lm7 <- lm(1/sqrt(FPRT) ~ skip.prev + skip.next, data=subset(m, pos==12 & FPRT>0 & cond=="UNAMB")); summary(lm7)
# qqPlot(residuals(lm7))
# effect <- predict(lm7) - coef(lm7)[1]
# m$FPRT.corr[m$pos==12 & m$FPRT>0 & m$cond=="UNAMB"] <- (1/(1/sqrt(m$FPRT[m$pos==12 & m$FPRT>0 & m$cond=="UNAMB"])-effect))^2

# boxcox(FPRT ~ skip.prev + skip.next + reg.prev + factor(fp_reg), data=subset(m,pos==12 & FPRT>0 & cond!="UNAMB"))
# lm8 <- lm(1/sqrt(FPRT) ~ skip.prev + skip.next + factor(fp_reg) , data=subset(m, pos==12 & FPRT>0 & cond!="UNAMB")); summary(lm8)
# qqPlot(residuals(lm6))
# effect <- predict(lm8) - coef(lm8)[1]
# m$FPRT.corr[m$pos==12 & m$FPRT>0 & m$cond!="UNAMB"] <- (1/(1/sqrt(m$FPRT[m$pos==12 & m$FPRT>0 & m$cond!="UNAMB"])-effect))^2




## Reading times ROI
mlt <- melt(m, id=c("roi","cond","wmc"), measure=c("FFD","FPRT","FPRT.corr","TFT","RPD","retr","enc"), na.rm = T)
cst <- cast(subset(mlt,value>0), variable+roi+cond+wmc ~ ., function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x), CI=ci(x)))
# head(cst)
means.t <- cst


## RT Positions
mlt <- melt(m, id=c("pos","cond","wmc"), measure=c("FFD","FPRT","FPRT.corr","TFT","RPD","retr","enc"), na.rm = T)
cst <- cast(subset(mlt,value>0), variable+pos+cond+wmc ~ ., function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x), CI=ci(x)))
means.t2 <- cst

## Ambiguity advantage ##
# Reading times ROI, AMB
mlt <- melt(m, id=c("roi","amb","wmc"), measure=c("FFD","FPRT","FPRT.corr","TFT","RPD","retr","enc"), na.rm = T)
cst <- cast(subset(mlt,value>0), variable+roi+amb+wmc ~ ., function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x), CI=ci(x)))
# head(cst)
means.t3 <- cst
levels(means.t3$variable)[2] <- "Gaze"

## Ambiguity advantage pre-verbal region ##
# Reading times ROI, AMB
mlt <- melt(m, id=c("roi2","amb","wmc"), measure=c("FFD","FPRT","FPRT.corr","TFT","RPD","retr","enc"), na.rm = T)
cst <- cast(subset(mlt,value>0), variable+roi2+amb+wmc ~ ., function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x), CI=ci(x)))
# head(cst)
means.t4 <- cst
levels(means.t4$variable)[2] <- "Gaze"




## Probabilities
mlt <- melt(m, id=c("roi","cond","wmc"), measure=c("refix","reread","fp_reg","skip","timeout"), na.rm=T)
cst <- cast(mlt, variable+roi+cond+wmc ~ ., function(x) c(M=mean(x), N=length(x)))
cst$SE <- sqrt(cst$M*(1-cst$M))/sqrt(cst$N)
cst$CI.lower <- cst$M + qt(.025, df=cst$N-1) * cst$SE
cst$CI.upper <- cst$M + qt(.975, df=cst$N-1) * cst$SE
# head(cst)
means.p <- cst
levels(means.p$variable)[3] <- "reg"

## Trial Probabilities
mlt <- melt(trialinfo, id=c("cond","wmc"), measure=c("s.reread","revision","attached","fail","acc","acc2"), na.rm=T)
cst <- cast(mlt, variable+cond+wmc ~ ., function(x) c(M=mean(x), N=length(x)))
cst$SE <- sqrt(cst$M*(1-cst$M))/sqrt(cst$N)
cst$CI.lower <- cst$M + qt(.025, df=cst$N-1) * cst$SE
cst$CI.upper <- cst$M + qt(.975, df=cst$N-1) * cst$SE
# head(cst)
means.p2 <- cst



##'
##' OVERVIEW
##'
#+ Overview, fig.keep='all', fig.show='asis', echo=F
summary(subjects$wmc)
summary(subjects$ga)

(plot0.wm <- qplot(roi, M, colour=cond, linetype=cond, group=cond, data=droplevels(subset(means.t, (roi%in%roilabels & variable%in%c("FFD","FPRT","FPRT.corr")))), geom=c("line", "point"), main="Model reading times", facets=variable+wmc~.)  + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) + theme_bw()+ scale_color_brewer(palette="Set1") + ylab("Mean duration in ms") + xlab("Region") + theme(legend.position="bottom"))


#(plot0.1.wm <- qplot(roi, M, colour=cond, linetype=cond, group=cond, data=droplevels(subset(means.t, (roi%in%roilabels & variable%in%c("FFD","FPRT.corr")))), geom=c("line", "point"), main="Model reading times", facets=variable+wmc~.)  + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) + theme_bw()+ scale_color_brewer(palette="Set1") + ylab("Mean duration in ms") + xlab("Region") + theme(legend.position="bottom"))

#(rawVScorr <- qplot(roi, M, colour=cond, linetype=cond, group=cond, data=droplevels(subset(means.t, (roi%in%roilabels & variable%in%c("FPRT","FPRT.corr")))), geom=c("line", "point"), main="Model reading times", facets=variable+wmc~.)  + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) + theme_bw()+ scale_color_brewer(palette="Set1") + ylab("Mean duration in ms") + xlab("Region") + theme(legend.position="bottom"))

#(plot1.wm <- qplot(roi, M, colour=cond, linetype=cond, group=cond, data=droplevels(subset(means.t, (roi%in%roilabels & variable%in%c("FPRT")))), geom=c("line", "point"), main="Model reading times", facets=variable+wmc~.)  + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) + theme_bw()+ scale_color_brewer(palette="Set1") + ylab("Mean duration in ms") + xlab("Region") + theme(legend.position="bottom"))

(plot1.wm.all <- qplot(pos, M, colour=cond, linetype=cond, group=cond, data=droplevels(subset(means.t2, (variable%in%c("FPRT.corr")))), geom=c("line", "point"), main="Model reading times", facets=wmc~.)  + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) + theme_bw()+ scale_color_brewer(palette="Set1") + ylab("Mean duration in ms") + xlab("Region") + theme(legend.position="bottom"))


(plot4.wm <- qplot(roi, M, colour=cond, linetype=cond, group=cond, data=subset(means.p, (roi%in%roilabels & variable%in%c("skip","refix"))), geom=c("line", "point"), main="Model probabilities", facets=variable+wmc~.)  + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) + theme_bw() + scale_color_brewer(palette="Set1") + ylab("Probability") + xlab("Region") + theme(legend.position="bottom"))


(plot2.wm <- qplot(roi, M, colour=cond, linetype=cond, group=cond, data=subset(means.p, (roi%in%roilabels & variable%in%c("reread","reg"))), geom=c("line", "point"), main="Model probabilities", facets=variable+wmc~.)  + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) + theme_bw() + scale_color_brewer(palette="Set1") + ylab("Probability") + xlab("Region") + theme(legend.position="bottom"))

(plot3.wm <- qplot(roi, M, colour=cond, linetype=cond, group=cond, data=droplevels(subset(means.t, (roi%in%roilabels  & variable%in%c("retr","enc")))), geom=c("line", "point"), main="Attachment and encoding", facets=variable+wmc~.)  + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) + theme_bw() + scale_color_brewer(palette="Set1") + theme(legend.position="bottom"))

(plot.to.wm <- qplot(roi, M, colour=cond, linetype=cond, group=cond, data=subset(means.p, (roi%in%roilabels & variable%in%c("timeout"))), geom=c("line", "point"), main="Model probabilities", facets=variable+wmc~.)  + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) + theme_bw() + scale_color_brewer(palette="Set1") + ylab("Probability") + xlab("Region") + theme(legend.position="bottom"))




#+ Results, echo=F, include=T
##
## RESULTS
##
n.subj <- length(levels(m$exp))
n.iterations <- length(levels(m$iteration))

## Attachment sites
head(trialinfo)
(att.hwmc <- xtabs(~val.attachment+cond,subset(trialinfo, wmc=="highWMC"))/(summary(subjects$wm)[1]*n.iterations))
(att.lwmc <- xtabs(~val.attachment+cond,subset(trialinfo, wmc=="lowWMC"))/(summary(subjects$wm)[2]*n.iterations))
## Attachment by wmc
(att.total <- with(trialinfo, tapply(attached, IND=list(cond), mean)))
(att <- with(trialinfo, tapply(attached, IND=list(wmc,cond), mean)))
## Revision
(rev.a <- round(with(trialinfo,tapply(revision,IND=list(wmc,cond),mean)), digits=3))
(rev <- round(with(subset(trialinfo,fail==0),tapply(revision,IND=list(wmc,cond),mean)), digits=3))
## Rereading
(rer <- round(with(trialinfo,tapply(s.reread,IND=list(wmc,cond),mean)), digits=3))
(rer.1 <- round(with(subset(trialinfo,fail==0),tapply(s.reread,IND=list(wmc,cond),mean)), digits=3))
## Accuracy
(acc <- round(with(trialinfo,tapply(acc,IND=list(wmc,cond),mean)), digits=2))
(acc2 <- round(with(trialinfo,tapply(acc2,IND=list(wmc,cond),mean)), digits=2))
## Accuracy plus 50/50 chance for unattached items
(acc2 <- acc+(1-(att.1 <- with(subset(trialinfo, fail==0), tapply(attached, IND=list(wmc,cond), mean))))/2)
## fails
(fail <- round(with(trialinfo,tapply(fail,IND=list(wmc,cond),mean)), digits=2))

## Ambiguity advantage
#(ambadv <- round(with(subset(means.t3, roi=="CONJ" & variable=="Gaze" & M!=0),tapply(M,IND=list(wmc,amb),mean)), digits=3))
(ambadv <- round(with(subset(means.t4, roi2%in%"pre-V" & variable=="Gaze"),tapply(M,IND=list(wmc,amb),mean)), digits=3))



##'
##' RESULTS
##'
#+ Tables, results='asis'
xtable(att)
xtable(rev)
xtable(rer)
xtable(acc)
xtable(ambadv)









##+ Data, echo=F, include=F
#data1 <- read.table("data-mv13_1.txt")
data <- read.table("data-mv13.txt")


#head(data); summary(data)
# colnames(data)[23]<-"wmc"
levels(data$cond) <- c("HIGH","LOW","UNAMB")
data$amb <- ifelse(data$cond=="UNAMB", "unambiguous", "ambiguous")
data$FPRT.c.exp <- exp(data$FPRT.c)
data$wmc <- factor(ifelse(data$pcu >= median(data$pcu), "high WMC", "low WMC"))

## mean rereading
mlt <- melt(data, id=c("cond","wmc"), measure=c("isa","acc"), na.rm=T)
cst <- cast(mlt, variable+cond+wmc ~ ., function(x) c(M=mean(x), N=length(x)))
cst$SE <- sqrt(cst$M*(1-cst$M))/sqrt(cst$N)
cst$CI.lower <- cst$M + qt(.025, df=cst$N-1) * cst$SE
cst$CI.upper <- cst$M + qt(.975, df=cst$N-1) * cst$SE
# head(cst)
d.means.p2 <- cst

## Ambiguity advantage ##
mlt <- melt(data, id=c("amb","wmc"), measure=c("FPRT.c.exp"), na.rm = T)
cst <- cast(subset(mlt,value>0), variable+amb+wmc ~ ., function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x), CI=ci(x)))
# head(cst)
d.means.t3 <- cst
d.means.t3$wmc <- rep(c("low WMC", "high WMC"),2)





##'
##' FIT TO DATA
##'
#+ Fit,
rer.data <- matrix(NA,2,3)
rer.data[1:2,1:3] <- c(0.265, 0.195, 0.24, 0.185, 0.22, 0.205)
rer.data2 <- data.frame(variable="s.reread", "cond"=c(rep("HIGH",2),rep("LOW",2),rep("UNAMB",2)), wmc=rep(c("high WMC","low WMC"),3), data=c(0.265, 0.195, 0.24, 0.185, 0.22, 0.205))
ambadv.data <- matrix(NA,2,2)
ambadv.data[1:2,1:2] <- c(498.12, 463.17, 496.15, 498.74)
ambadv.data2 <- data.frame(variable="FPRT.corr", roi="CONJ","amb"=c(rep("ambiguous",2),rep("unambiguous",2)), wmc=rep(c("high WMC","low WMC"),2), data=c(498.12, 463.17, 496.15, 498.74))

rer[rer==0] <- 0.0001
#rmsd(c(rer.data*1000,ambadv.data), c(rer*1000,ambadv))
rmsd(c(rer.data[,1]*1000,ambadv.data), c(rer[,1]*1000,ambadv))

(f1a <- rmsd(c(rer.data[,1]*1000), c(rer[,1]*1000)))
#cor(as.vector(rer.data[,1:2]),as.vector(rer[,1:2]))
(f1b <- cor(c(rer.data),c(rer)))

(f2a <- rmsd(c(ambadv.data), c(ambadv)))
(f2b <- cor(c(ambadv.data),c(ambadv)))

(score <- f1a+f2a-(100*(f1b+f2b)))


#+ Plots, fig.keep='all', fig.show='asis', echo=F
##
## PLOTS
##
par(mfrow=c(1,2), cex=0.7)
barplot(att.hwmc, beside=T, legend=T,  main="Attachment site high WMC")
barplot(att.lwmc, beside=T, legend=T, main="Attachment site low WMC", args.legend=c(x="topleft"))

par(mfrow=c(2,2), cex=0.5)
barplot(att[,1:3], beside=T, legend=T, main="Predicted attachment proportion", args.legend = list(x="topleft"), xlab="Attachment condition")
#colnames(rev) <- c("ambiguous HIGH", "ambiguous LOW", "unambiguous LOW")
barplot(rev, beside=T, legend=T, main="Predicted reanalysis proportion", args.legend = list(x="topright"), xlab="Attachment condition")
#colnames(rer) <- c("ambiguous HIGH", "ambiguous LOW", "unambiguous LOW")
barplot(rer, beside=T, legend=T, main="Rereading proportion", args.legend = list(x="topright"), xlab="Attachment condition")
#colnames(acc) <- c("ambiguous HIGH", "ambiguous LOW", "unambiguous LOW")
barplot(acc2, beside=T, legend=T, main="Accuracy", args.legend = list(x="topleft"), xlab="Attachment condition")




t1 <- theme(legend.position="right", text=element_text(family="ScalaSans"), legend.key=element_blank(), legend.title=element_blank(), axis.ticks.y=element_blank() ,axis.title.x=element_blank() ,axis.ticks.x=element_blank() ,panel.grid.major=element_line(color="gray60") ,panel.grid.major.x=element_blank() ,panel.grid.minor=element_blank() ,panel.border=element_blank() ,strip.background=element_rect(fill="gray85", color=NA)) 

#t2 <- theme(legend.position="right", text=element_text(family="Helvetica"), legend.key=element_blank(), legend.title=element_blank(), axis.ticks.y=element_blank() ,axis.title.x=element_blank() ,axis.ticks.x=element_blank() ,panel.grid.major=element_line(color="gray60") ,panel.grid.major.x=element_blank() ,panel.grid.minor=element_blank() ,panel.border=element_blank() ,strip.background=element_rect(fill="gray85", color=NA)) 




#+ Rereading, echo=F, fig.keep='all', fig.show='asis', warning=F
## Rereading prob.
levels(means.p2$wmc) <- c("high WMC", "low WMC")
dodge1 <- position_dodge(width=0.6)
dodge2 <- position_dodge(width=0.04)

p1 <- ggplot(aes(cond, M, fill=wmc, group=wmc), data=droplevels(subset(means.p2, (variable%in%c("s.reread"))))) + ylab("") + ggtitle("Model rereading proportion") + theme_bw() 
p1 <- p1 + scale_fill_manual("cond", values=UPpalette(firstcolor="humfak",secondcolor="darkblue")) + scale_x_discrete(expand=c(0.1,0)) + scale_y_continuous(expand=c(0,0))
p1 <- p1 +geom_bar(position=dodge1, stat="identity") + geom_linerange(aes(max=CI.upper, min=CI.lower), position=dodge1, color="gray40")
p1 <- p1 + t1
dev.new(width=4, height=3)
p1
#ggsave("rereading.eps", p1, width=4, height=3)
#ggsave("rereading.pdf", p1, width=4, height=3)

## data
p1.d <- ggplot(aes(cond, M, fill=wmc, group=wmc), data=droplevels(subset(d.means.p2, (variable%in%c("isa"))))) + ylab("") + ggtitle("Data rereading proportion") + theme_bw() 
p1.d <- p1.d + scale_fill_manual("cond", values=UPpalette(firstcolor="humfak",secondcolor="darkblue")) + scale_x_discrete(expand=c(0.1,0)) + scale_y_continuous(expand=c(0,0), limits=c(0,.31))
p1.d <- p1.d +geom_bar(position=dodge1, stat="identity") + geom_linerange(aes(max=CI.upper, min=CI.lower), position=dodge1, color="gray40")
p1.d <- p1.d + t1
p1.d <- p1.d #+ theme(legend.position="none")
dev.new(width=4, height=3)
p1.d
#ggsave("rereading-data.eps", p1.d, width=4, height=3)



#+ AmbAdv, echo=FALSE, fig.keep='all', fig.show='asis', warning=F
## Ambiguity Advantage
means.t3$amb <- as.factor(means.t3$amb)
levels(means.t3$wmc) <- c("high WMC", "low WMC")
dodge1 <- position_dodge(width=0.5)
dodge2 <- position_dodge(width=0.08)
#dodge2 <- position_dodge(width=0.2)

## CONJ
p2 <- ggplot(aes(amb, M, col=wmc, group=wmc, lty=wmc, shape=wmc), data=droplevels(subset(means.t3, (roi%in%c("CONJ") & variable%in%c("FPRT.corr")))))
p2 <- p2 + ylab("Mean gaze duration in ms") + ggtitle("Model (cuando/si)") + theme_bw()
p2 <- p2 + scale_colour_manual("wmc", values=UPpalette(firstcolor="humfak",secondcolor="darkblue")) + scale_x_discrete(expand=c(0.1,0)) + scale_y_continuous(expand=c(0.0,0))
p2 <- p2 + t1
#2 <- p2 + geom_line(colour="gray90", aes(y=data, linetype=wmc)) + geom_point(colour="gray90", size=2, aes(y=data))
p2 <- p2 + geom_line(position=dodge2) + geom_point(position=dodge2, size=3) + geom_linerange(aes(max=CI.upper, min=CI.lower),position=dodge2)
dev.new(width=4, height=3)
p2
#ggsave("ambadv-model.eps", p2, width=4, height=3)

## pre-V region
levels(means.t4$wmc) <- c("high WMC", "low WMC")
p3 <- ggplot(aes(amb, M, col=wmc, group=wmc), data=droplevels(subset(means.t4, (M!=0 & roi2%in%c("pre-V") & variable%in%c("FPRT.corr")))))
p3 <- p3 + ylab("Mean gaze duration in ms") + ggtitle("Model reading times in pre-verbal region") + theme_bw()
p3 <- p3 + scale_colour_manual("wmc", values=UPpalette(firstcolor="humfak",secondcolor="darkblue")) + scale_x_discrete(expand=c(0.1,0)) + scale_y_continuous(expand=c(0.0,0))
p3 <- p3 + geom_line(position=dodge2) + geom_point(position=dodge2) + geom_linerange(aes(max=CI.upper, min=CI.lower),position=dodge2)
p3 + t1

## data
p2.d <- ggplot(aes(amb, M, col=wmc, group=wmc, lty=wmc, shape=wmc),#, ymax=509, ymin=460), 
data=subset(d.means.t3,variable=="FPRT.c.exp"))
p2.d <- p2.d + ylab("Mean gaze duration in ms") + ggtitle("Data (pre-verbal region)") + theme_bw()
p2.d <- p2.d + scale_colour_manual("wmc", values=UPpalette(firstcolor="humfak",secondcolor="darkblue")) + scale_x_discrete(expand=c(0.1,0)) + scale_y_continuous(expand=c(0.0,0))
p2.d <- p2.d + t1
#p2 <- p2 + geom_line(colour="gray90", aes(y=data, linetype=wmc)) + geom_point(colour="gray90", size=2, aes(y=data))
p2.d <- p2.d + geom_line(position=dodge2) + geom_point(position=dodge2, size=3) + geom_linerange(aes(max=CI.upper, min=CI.lower),position=dodge2)
p2.d <- p2.d # + theme(legend.position="none")
dev.new(width=4, height=3)
p2.d
#ggsave("ambadv-data.eps", p2.d, width=4, height=3)





#+ Attachment, echo=F, fig.keep='all', fig.show='asis', warning=F
dodge1 <- position_dodge(width=0.6)
dodge2 <- position_dodge(width=0.04)
levels(means.p2$wmc) <- c("high WMC", "low WMC")
p5 <- ggplot(aes(cond, M, fill=wmc, group=wmc), data=droplevels(subset(means.p2, variable%in%c("attached")))) + ylab("") + ggtitle("Attachment proportion") + theme_bw() 
p5 <- p5 + scale_fill_manual("cond", values=UPpalette(firstcolor="humfak",secondcolor="darkblue")) + scale_x_discrete(expand=c(0.1,0)) + scale_y_continuous(expand=c(0,0))
p5 <- p5 +geom_bar(position=dodge1, stat="identity") + geom_linerange(aes(max=CI.upper, min=CI.lower), position=dodge1, color="gray40")
p5 <- p5 + t1
dev.new(width=4, height=3)
p5
#ggsave("attachments.eps", p5, width=4, height=3)


#+ Timeout, echo=F, fig.keep='all', fig.show='asis', warning=F
dodge1 <- position_dodge(width=0.6)
dodge2 <- position_dodge(width=0.04)
levels(means.p$wmc) <- c("high WMC", "low WMC")
p6 <- ggplot(aes(cond, M, fill=wmc, group=wmc), data=droplevels(subset(means.p, variable%in%c("timeout") & roi=="CONJ"))) + ylab("") + ggtitle("Time out proportion on cuando/si") + theme_bw() 
p6 <- p6 + scale_fill_manual("cond", values=UPpalette(firstcolor="humfak",secondcolor="darkblue")) + scale_x_discrete(expand=c(0.1,0)) + scale_y_continuous(expand=c(0,0))
p6 <- p6 +geom_bar(position=dodge1, stat="identity") + geom_linerange(aes(max=CI.upper, min=CI.lower), position=dodge1, color="gray40")
p6 <- p6 + t1
dev.new(width=4, height=3)
p6
#ggsave("timeouts.eps", p6, width=4, height=3)


#+ Attachment-time, echo=F, fig.keep='all', fig.show='asis', warning=F
dodge1 <- position_dodge(width=0.5)
dodge2 <- position_dodge(width=0.08)

p <- ggplot(aes(amb, M, col=wmc, group=wmc, lty=wmc), data=droplevels(subset(means.t3, (roi%in%c("CONJ") & variable%in%c("retr")))))
p <- p + ylab("Mean duration in ms") + ggtitle("Attachment duration on (cuando/si)") + theme_bw()
p <- p + scale_colour_manual("wmc", values=UPpalette(firstcolor="humfak",secondcolor="darkblue")) + scale_x_discrete(expand=c(0.1,0)) + scale_y_continuous(expand=c(0.0,0))
p <- p + t1
#p <- p + geom_line(colour="gray90", aes(y=data, linetype=wmc)) + geom_point(colour="gray90", size=2, aes(y=data))
p7 <- p + geom_line(position=dodge2) + geom_point(position=dodge2, size=3, shape=15) + geom_linerange(aes(max=CI.upper, min=CI.lower),position=dodge2)
p7
ggsave("att-time.eps", p7, width=4, height=3)



#+ acc, echo=F, fig.keep='all', fig.show='asis', warning=F
dodge1 <- position_dodge(width=0.6)
dodge2 <- position_dodge(width=0.04)
p <- ggplot(aes(cond, M, fill=wmc, group=wmc), data=droplevels(subset(means.p2, variable%in%c("acc")))) + ylab("") + ggtitle("Model comprehension accuracy") + theme_bw() 
p <- p + scale_fill_manual("cond", values=UPpalette(firstcolor="humfak",secondcolor="darkblue")) + scale_x_discrete(expand=c(0.1,0)) + scale_y_continuous(expand=c(0,0))
p <- p +geom_bar(position=dodge1, stat="identity") + geom_linerange(aes(max=CI.upper, min=CI.lower), position=dodge1, color="gray40")
p8 <- p + t1
p8
ggsave("acc-model.eps", p8, width=5, height=4)

p <- ggplot(aes(cond, M, fill=wmc, group=wmc), data=droplevels(subset(means.p2, variable%in%c("acc2")))) + ylab("") + ggtitle("Model comprehension accuracy") + theme_bw() 
p <- p + scale_fill_manual("cond", values=UPpalette(firstcolor="humfak",secondcolor="darkblue")) + scale_x_discrete(expand=c(0.1,0)) + scale_y_continuous(expand=c(0,0))
p <- p +geom_bar(position=dodge1, stat="identity") + geom_linerange(aes(max=CI.upper, min=CI.lower), position=dodge1, color="gray40")
p9 <- p + t1
p9
ggsave("acc2-model.eps", p9, width=5, height=4)


p <- ggplot(aes(cond, M, fill=wmc, group=wmc), data=droplevels(subset(d.means.p2, variable%in%c("acc")))) + ylab("") + ggtitle("Data comprehension accuracy") + theme_bw() 
p <- p + scale_fill_manual("cond", values=UPpalette(firstcolor="humfak",secondcolor="darkblue")) + scale_x_discrete(expand=c(0.1,0)) + scale_y_continuous(expand=c(0,0))
p <- p +geom_bar(position=dodge1, stat="identity") + geom_linerange(aes(max=CI.upper, min=CI.lower), position=dodge1, color="gray40")
p9.d <- p + t1
p9.d
ggsave("acc-data.eps", p9.d, width=5, height=4)



##'
##' Data Check
##'
##+ DataCheck, echo=T, include=T
head(m); summary(m)
head(trialinfo); summary(trialinfo)
head(subjects); summary(subjects)



##'
##' R-SUMMARY
##'
##+ RSummary, echo=F, include=F
summary(subjects$wmc)
summary(subjects$ga)

att
rev
rer
acc
ambadv


