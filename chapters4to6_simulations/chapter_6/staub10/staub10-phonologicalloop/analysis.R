
#+ setup, include=FALSE
rm(list=ls())
source("~/.Rprofile")
library(knitr)
library(em2)
library(reshape)
library(xtable)
library(gdata)
library(ggplot2)
opts_chunk$set(fig.path='figure/silk-')
# setwd("~/Dropbox/Workspace/ACT-R_EMMA/ACTR6SentenceParserWithEMMA/extensions/staub10/e2-p110-r2")
#setwd("e2-p110-r2")
setwd("output")
#setwd("e2-p110-r2")


#+ Read simulation data
f <- read.table("fixations.txt", header=T)
colnames(f) <- c("exp","iteration","cond","pos","word","dur")
# head(f) #str(f) # head(f,50)



##-------------------------------------------------------------------------------
## EM
##-------------------------------------------------------------------------------
f$iteration <- as.factor(as.character(f$iteration))
etm <- with(f, em2(pos, dur, data.frame(exp, iteration, cond)))
# head(etm) # summary(etm)
save(etm, file="sim-etm.RData")

##-------------------------------------------------------------------------------
## OTHER INFO
##-------------------------------------------------------------------------------
m <- etm

#' attachment times ##
retr <- read.table("attachments.txt", header=TRUE)
colnames(retr) <- c("exp","iteration","cond","roi","word","retr")
# head(retr[-5]) # str(retr); head(retr,25)
m <- merge(etm, retr[-5], by=c("exp","iteration","cond","roi"), all.x=TRUE)

## encoding times ##
enc <- read.table("enctimes.txt", header=TRUE)
colnames(enc) <- c("exp","iteration","cond","roi","word","enc","ecc","freq1")
#head(enc) # str(enc); head(enc,25)
m <- merge(m, enc, by=c("exp","iteration","cond","roi"), all.x=TRUE)
#head(m)



##-------------------------------------------------------------------------------
## ADDITIONAL MEASURES
##-------------------------------------------------------------------------------
## first-pass regression prob.
m$fp_reg <- ifelse(m$RBRC>0,1,0) 
## skipping prob:
m$skip<-ifelse(m$FPRT==0,1,0)
## re-reading prob:
m$reread<-ifelse(m$RRT>0,1,0)
## re-fixation prob:
m$refix <- ifelse(m$FPRT>m$FFD,1,0)
#m$onefix <- ifelse((m$SFD>0 & m$FFP==1),1,0)
head(m)



##-------------------------------------------------------------------------------
## ROIs
##-------------------------------------------------------------------------------
colnames(m)[4] <- "pos"
m$pos <- as.vector(as.numeric(m$pos))
m$roi <- m$pos

m$roi[m$pos == 3] <- "REL"
m$roi[m$pos == 7] <- "V"
m$roi[!m$pos %in% 3:7] <- "other"
m$roi[m$cond == "SRC" & m$pos == 4] <- "RC-V"
m$roi[m$cond == "SRC" & m$pos == 5] <- "DET"
m$roi[m$cond == "SRC" & m$pos == 6] <- "N"
m$roi[m$cond == "ORC" & m$pos == 4] <- "DET"
m$roi[m$cond == "ORC" & m$pos == 5] <- "N"
m$roi[m$cond == "ORC" & m$pos == 6] <- "RC-V"


m$roi2 <- m$roi
m$roi2[m$roi %in% c("DET","N")] <- "DET/N"

#levels(m$roi)
m$roi <- factor(m$roi, levels=c("REL", "DET", "N", "RC-V", "V", "other"))
#levels(m$roi2)
m$roi2 <- factor(m$roi2, levels=c("REL", "DET/N", "RC-V", "V", "other"))

# #head(d,20)
# d$roi2 <- as.vector(d$roi)
# d$roi2[d$roi2%in%c("DET","N")] <- "DET/N"



##-------------------------------------------------------------------------------
## INSPECTION AND SAVE
##-------------------------------------------------------------------------------
## cond by iteration
xtabs(~cond+iteration,m)
# head(m,20)
# levels(m$cond)
# m$cond <- factor(m$cond)
# summary(m)
save(m, file="m.RData")
#load("m.RData")



##-------------------------------------------------------------------------------
## MEANS
##-------------------------------------------------------------------------------
# head(m)
m$roi <- as.factor(m$roi)

## Reading times ##
mlt <- melt(m, id=c("roi","cond","word"), measure=c("FFD","FPRT","TFT","RRT","RPD","retr","enc"), na.rm = T)
cst <- cast(subset(mlt,value>0), variable+roi+cond+word ~ ., function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x), CI=ci(x)))
# head(cst); summary(cst)
means.t <- cst

mlt <- melt(m, id=c("roi2","cond"), measure=c("FFD","FPRT","TFT","RRT","RPD","retr","enc"), na.rm = T)
cst <- cast(subset(mlt,value>0), variable+roi2+cond ~ ., function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x), CI=ci(x)))
# head(cst); summary(cst)
means.t2 <- cst

mlt <- melt(m, id=c("pos","cond","word"), measure=c("FFD","FPRT","TFT","RRT","RPD","retr","enc"), na.rm = T)
cst <- cast(subset(mlt,value>0), variable+pos+cond+word ~ ., function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x), CI=ci(x)))
# head(cst); summary(cst)
means.t.all <- cst


## Probabilities ##
mlt <- melt(m, id=c("roi","cond","word"), measure=c("refix","reread","fp_reg","skip"), na.rm=T)
cst <- cast(mlt, variable+roi+cond+word ~ ., function(x) c(M=mean(x), N=length(x)))
cst$SE <- sqrt(cst$M*(1-cst$M))/sqrt(cst$N)
cst$CI.lower <- cst$M + qt(.025, df=cst$N-1) * cst$SE
cst$CI.upper <- cst$M + qt(.975, df=cst$N-1) * cst$SE
# head(cst); summary(cst)
means.p <- cst

mlt <- melt(m, id=c("roi2","cond"), measure=c("refix","reread","fp_reg","skip"), na.rm=T)
cst <- cast(mlt, variable+roi2+cond ~ ., function(x) c(M=mean(x), N=length(x)))
cst$SE <- sqrt(cst$M*(1-cst$M))/sqrt(cst$N)
cst$CI.lower <- cst$M + qt(.025, df=cst$N-1) * cst$SE
cst$CI.upper <- cst$M + qt(.975, df=cst$N-1) * cst$SE
# head(cst); summary(cst)
means.p2 <- cst

mlt <- melt(m, id=c("pos","cond","word"), measure=c("refix","reread","fp_reg","skip"), na.rm=T)
cst <- cast(mlt, variable+pos+cond+word ~ ., function(x) c(M=mean(x), N=length(x)))
cst$SE <- sqrt(cst$M*(1-cst$M))/sqrt(cst$N)
cst$CI.lower <- cst$M + qt(.025, df=cst$N-1) * cst$SE
cst$CI.upper <- cst$M + qt(.975, df=cst$N-1) * cst$SE
# head(cst); summary(cst)
means.p.all <- cst

m.means <- rbind(means.t,means.p)
head(m.means)
m.means2 <- rbind(means.t2,means.p2)
#head(m.means2)



#head(m.means)
#head(m.means$roi)
#levels(m.means$roi)



##-------------------------------------------------------------------------------
##' DATA
##-------------------------------------------------------------------------------
d <- read.table("../staub10-data.txt", header=T)
colnames(d)[1] <- "variable"
head(d) # str(d) # str(m.means)
#m.means$type <- "model"
d2 <- d
d2$roi2 <- as.vector(d2$roi)
d2$roi2[d2$roi2%in%c("DET","N")] <- "DET/N"
d2 <- cast(d2, variable+roi2+cond ~ ., value="data", mean)
colnames(d2)[4] <- "data"
#head(d2)

dim(m.means)
means <- merge(m.means, d, by=c("variable","cond","roi"), all.x=T)
dim(means)
dim(m.means2)
means2 <- merge(m.means2, d2, by=c("variable","cond","roi2"), all.x=T)
# head(means2)
dim(means2)



##-------------------------------------------------------------------------------
## PLOT
##-------------------------------------------------------------------------------
roilabels <- c("REL", "DET", "N", "RC-V", "V")
levels(means$variable)[2] <- "Gaze"
levels(means$variable)[5] <- "Go-past"
levels(means$variable)[10] <- "reg"

roilabels2 <- c("REL", "DET/N", "RC-V", "V")
levels(means2$variable)[2] <- "Gaze"
levels(means2$variable)[5] <- "Go-past"
levels(means2$variable)[10] <- "reg"

## Overview ##
(p.all.t <- qplot(pos, M, colour=cond, linetype=cond, group=cond, data=droplevels(subset(means.t.all, (M!=0 & pos>1 & variable%in%c("FFD","FPRT","RPD")))), geom=c("line", "point"), main="Reading times", facets=variable~.)  + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) + theme_bw() + scale_color_brewer(palette="Set1"))

(p.all.p <- qplot(pos, M, colour=cond, linetype=cond, group=cond, data=subset(means.p.all, (variable%in%c("reread","fp_reg","refix"))), geom=c("line", "point"), main="Probabilities", facets=variable~.)  + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) + theme_bw()+ scale_color_brewer(palette="Set1"))

(p.rois.t <- qplot(roi, M, colour=cond, linetype=cond, group=cond, data=droplevels(subset(means, (M!=0 & variable%in%c("FFD","Gaze","Go-past") & roi%in%roilabels))), geom=c("line", "point"), main="Reading times", facets=variable~.)  + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) + theme_bw() + scale_color_brewer(palette="Set1"))

(p.info <- qplot(roi, M, colour=cond, linetype=cond, group=cond, data=droplevels(subset(m.means, (variable%in%c("retr","enc") & roi%in%roilabels))), geom=c("line", "point"), main="Word information", facets=variable~.)  + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) + theme_bw()+ scale_color_brewer(palette="Set1"))




dodge1 <- position_dodge(width=0.6)
dodge2 <- position_dodge(width=0.05)
(p.d1 <- ggplot(aes(roi2, M, col=cond, group=cond),
	data=droplevels(subset(means2, (M!=0 & roi2%in%c("DET/N","RC-V","V")  & variable%in%c("FFD")))))
	+ ylab("Mean first fixation duration (ms)") 
	+ xlab("Region")
	+ ggtitle("Model reading times")
	+ theme_bw()
	+ theme(legend.position="right"
			,text=element_text(family="Helvetica")
			,legend.key=element_blank()
			,legend.title=element_blank(),
		    # ,axis.line.y=element_line(color="gray85")
		    # ,axis.ticks.y=element_blank()
			# ,axis.line.x=element_line(color="gray85")
			# ,axis.title.x=element_blank()
			# ,axis.ticks.x=element_blank()
			# ,panel.grid.major=element_line(color="gray65")
			# ,panel.grid.major.x=element_blank()
			# ,panel.grid.minor=element_blank()
			# ,panel.border=element_blank()
			# ,panel.background=element_blank()
			strip.background=element_rect(fill="gray90", color=NA))
	+ scale_colour_manual("cond", values=UPpalette(firstcolor="humfak",secondcolor="darkblue"))
	+ scale_x_discrete(expand=c(0.1,0))
	+ scale_y_continuous(expand=c(0.1,0), limits=c(200,350))
	+ geom_line(colour="gray90", linetype=2, aes(y=data))
	+ geom_point(colour="gray90", size=2, aes(y=data))
	# + geom_bar(position=dodge1, stat="identity")
	+ geom_line(position=dodge2)
	+ geom_point(position=dodge2)
	# + geom_linerange(aes(max=CI.upper, min=CI.lower),position=dodge2)
	+ geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0),position=dodge2)
	# + geom_smooth(aes(ymin = CI.lower, ymax = CI.upper), stat="identity")
	# + facet_grid(variable~.)
)
ggsave("staub10-model+data-rt.pdf", p.d1, width=5, height=4)






(plot1 <- qplot(roi, M, colour=cond, linetype=cond, group=cond, data=droplevels(subset(means, (M!=0 & roi%in%roilabels  & variable%in%c("FFD","Gaze")))), geom=c("line", "point"), main="Model reading times", facets=variable~.)  + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) + theme_bw()+ scale_color_brewer(palette="Set1") + ylab("Mean duration in ms") + xlab("Region") + theme(legend.position="bottom"))

(plot2 <- qplot(roi, M, colour=cond, linetype=cond, group=cond, data=subset(means, (roi%in%roilabels & variable%in%c("reg","reread"))), geom=c("line", "point"), main="Model probabilities", facets=variable~.)  + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) + theme_bw() + scale_color_brewer(palette="Set1") + ylab("Probability") + xlab("Region") + opts(legend.position="bottom"))

(plot3 <- qplot(roi, M, colour=cond, linetype=cond, group=cond, data=droplevels(subset(m.means, (roi%in%roilabels  & variable%in%c("retr","enc")))), geom=c("line", "point"), main="Reading times", facets=variable~.)  + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) + theme_bw())




# p1 <- m.means
# roilabels <- c("pre-crit", "ignoriert", "hatte")
# levels(p1$roi)[16:18] <- roilabels
# colnames(p1)[4] <- "probability"

# (plot2 <- qplot(roi, probability, colour=cond, linetype=cond, group=cond, data=subset(p1, (roi%in%roilabels & Measure%in%c("reread","fp_reg"))), geom=c("line", "point"), facets=Measure~.)  + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) + theme_bw())

# ggsave("staub10-model-times-all.pdf", p.rois.t, width=6, height=4)
# ggsave("staub10-model-probs-all.pdf", plot0.2, width=6, height=4)

# ggsave("staub10-model-times.pdf", plot1, width=4, height=4)
# ggsave("staub10-model-probs1.pdf", plot2, width=4, height=4)
# ggsave("staub10-model-retr-enc.pdf", plot3, width=6, height=4)


# (plot1.1 <- plot1 + geom_line(colour=grey, size=0.2, aes(y=data)))
# ggsave("staub10-model-times+data.pdf", plot1.1, width=6, height=4)








##-------------------------------------------------------------------------------
## TABLES
##-------------------------------------------------------------------------------
# means.refix<-with(cr,tapply(refix,IND=list(cond,roi),mean))
# se.refix<-with(cr,tapply(refix,IND=list(cond,roi),se))
# (means.se.refix<-paste(unmatrix(round(means.refix,digits=2),byrow=TRUE),
       # paste(paste("(",unmatrix(round(se.refix,digits=2),byrow=TRUE),sep=""),
       # ")",sep=""),sep=" "))
# means.se.refix.xtable<-xtable(matrix(means.se.refix,3,3))
# print(xtable(means.se.refix.xtable),file="means.se.refix.txt")
