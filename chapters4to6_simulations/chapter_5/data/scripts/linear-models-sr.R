remove(list=ls())
#flush.console()
library(reshape)
library(xtable)
library(lme4)
library(Hmisc)


#############################################

load("../PSC-CORPUS-DATA/d.RData")
#head(d)

## correlations:
with(d, cor(spu,rv100))
with(d, cor(spu,log(rv100)))

with(d, cor(spu,rv1))
with(d, cor(rv1,rv100))
with(d, cor(spu,sp100))

#############################################

print.lm <- function(m){
	lm.sfd <- glm(-1000/SFD~scale(log(frequency),s=F)+scale(len,s=F)+scale(log(rv100),s=F)+scale(spu,s=F)-1, subset(m, SFD!=0), family="gaussian")
	lm.ffd <- lm(-1000/FFD~scale(log(frequency),s=F)+scale(len,s=F)+scale(log(rv100),s=F)+scale(spu,s=F)-1, subset(m, FFD!=0))
	lm.gaze <- lm(-1000/FPRT~scale(log(frequency),s=F)+scale(len,s=F)+scale(log(rv100),s=F)+scale(spu,s=F)-1, subset(m, FPRT!=0))
	lm.trt <- lm(-1000/TFT~scale(log(frequency),s=F)+scale(len,s=F)+scale(log(rv100),s=F)+scale(spu,s=F)-1, subset(m, TFT!=0))
	lm.rpd <- lm(-1000/RPD~scale(log(frequency),s=F)+scale(len,s=F)+scale(log(rv100),s=F)+scale(spu,s=F)-1, subset(m, RPD!=0))
	lm.fp_reg <- glm(fp_reg~scale(log(frequency),s=F)+scale(len,s=F)+scale(log(rv100),s=F)+scale(spu,s=F)-1, m, family="binomial")
	lm.reread <- glm(reread~scale(log(frequency),s=F)+scale(len,s=F)+scale(log(rv100),s=F)+scale(spu,s=F)-1, m, family="binomial")
	lm1 <- cbind(as.data.frame(coef(summary(lm.sfd))), confint(lm.sfd))
	lm2 <- cbind(as.data.frame(coef(summary(lm.ffd))), confint(lm.ffd))
	lm3 <- cbind(as.data.frame(coef(summary(lm.gaze))), confint(lm.gaze))
	lm4 <- cbind(as.data.frame(coef(summary(lm.trt))), confint(lm.trt))
	lm5 <- cbind(as.data.frame(coef(summary(lm.rpd))), confint(lm.rpd))
	lm6 <- cbind(as.data.frame(coef(summary(lm.fp_reg))), confint(lm.fp_reg))
	lm7 <- cbind(as.data.frame(coef(summary(lm.reread))), confint(lm.reread))
#coef(lm1)[, "t value"]
#coef(lm1)[, "Pr(>|t|)"]

	colnames(lm1)[3:6] <- 
	colnames(lm2)[3:6] <- 
	colnames(lm3)[3:6] <- 
	colnames(lm4)[3:6] <- 
	colnames(lm5)[3:6] <- 
	colnames(lm6)[3:6] <- 
	colnames(lm7)[3:6] <- c("t,z","p","CI.lower","CI.upper")

	report <- rbind(lm1,lm2,lm3,lm4,lm5,lm6,lm7)
	pred <- rep(c("freq","len","rv","sp"),7)
	measure <- c(rep("SFD",4),rep("FFD",4),rep("gaze",4),rep("TRT",4),rep("RPD",4),rep("FPREG",4),rep("REG",4))
	rownames(report) <- NULL
	report <- cbind(measure,pred,report)
	report
}



load("../model-psc/m.RData")
model <- rep("emma",28)
report <- cbind(model,print.lm(m))

load("../model-psc-s/m.RData")
model <- rep("emma+s",28)
report <- rbind(report,cbind(model,print.lm(m)))

load("../model-psc-r/m.RData")
model <- rep("emma+r",28)
report <- rbind(report,cbind(model,print.lm(m)))

load("../model-psc-sh/m.RData")
model <- rep("emma+sh",28)
report <- rbind(report,cbind(model,print.lm(m)))

load("../model-psc-sr/m.RData")
model <- rep("emma+rs",28)
report <- rbind(report,cbind(model,print.lm(m)))

load("../model-psc-shr/m.RData")
model <- rep("emma+rsh",28)
report <- rbind(report,cbind(model,print.lm(m)))

lms <- report
save(lms, file="lms.RData")



########################################################
########################################################
library(xtable)
load("lms.RData")
#report <- lms

report1 <- subset(lms, (measure%in%c("SFD","FFD","gaze","TRT","RPD","FPREG") & model=="emma+rsh" & pred%in%c("rv","sp")))

# srpr first:
#boston11.coeff <- c(5.209e-02,1.594e-04, NA,NA, 5.530e-02,9.903e-05, 1.689e-01,2.645e-04)
#boston11.se <- c(1.793e-03,7.538e-06, NA,NA, 2.534e-03,1.060e-05, 1.767e-02,7.667e-05)
#boston11.t <- c(29,21.1, NA,NA, 21.8,9.3, 9.56,3.45)

# rv first:
boston11.coeff <- c(1.545e-04,4.384e-02, 1.594e-04,5.209e-02, NA,NA, 8.008e-05,4.588e-02, 9.903e-05,5.530e-02, 2.645e-04,1.689e-01)
boston11.se <- c(8.504e-06,1.998e-03,  7.538e-06,1.793e-03, NA,NA, 9.976e-06,2.386e-03, 1.060e-05,2.534e-03, 7.667e-05,1.767e-02)
boston11.t <- c(18.2,21.9, 21.1,29, NA,NA, 8,19.2, 9.3,21.8, 3.45,9.56)

measures <- c("SFD"," ","FFD"," ","gaze"," ","TRT"," ","RPD"," ","FPREG"," ")
pred <- rep(c("Retrieval","Surprisal"),6)
compreport <- data.frame(measures,pred,report1[4],report1[5],report1[6],rep(" ",12), boston11.coeff, boston11.se, boston11.t)

rownames(compreport) <- NULL
colnames(compreport) <- c("Measure","Predictor", "Coef.","SE","t or z", " ", "Coef.","SE","t or z")
compreport.x <- xtable(compreport, label="tab:lmreport", caption=".", digits=c(0,0,0,3,3,1,0,5,5,1), align="lllrrrlrrr")
print(compreport.x, include.rownames=FALSE, sanitize.text.function = function(x){x}, size="small", only.contents=TRUE)

########################################################
########################################################


library(Hmisc)

#report
par()
load("lms.RData")
head(lms)
lms$Estimate.norm <- lms$Estimate/(lms[,5])

mar1 <- c(5.1,10.1,4.1,2.1)
mar2 <- c(5.1,4.1,4.1,2.1)
tx <- -0.6; ty <- 0.05; tcex <- 1.8; pcex <- 1.3; acex <- 1.3; acex2 <- 1.5; lcex <- 1.3

layout(matrix(1:8, 4, 2, byrow = TRUE), widths=c(4.64,4))

# p <- subset(lms,(pred=="sp" & measure=="SFD")) 
	# par(mar=mar1)
	# plot(1:6,p$Estimate, type="p", pch=19 ,ylim=c(-0.06,0.145)
		# ,xlab="EMMA Simulation", ylab="Estimate" ,main="Surprisal"
		# ,bty="n" ,xaxt="n", cex=pcex, cex.axis=acex, cex.lab=lcex, cex.main=1.5
		# )
	# errbar(1:6,p$Estimate, p$CI.upper, p$CI.lower, add=T)#, ,bty="n" ,xaxt="n", yaxt="n",ylab="",xlab="")
	# axis(1,labels=c("EMMA",expression(+s[1]),expression(+r),expression(+s[2]),expression(+rs[1]),expression(+rs[2])), at=1:6, cex.axis=acex2, cex.lab=lcex)
	# abline(0,0, lty=2)
	# text(tx,ty, "SFD", cex=tcex, xpd=T)
# #	mtext("SFD", side=2, line=0,  at=c(0,0), cex=1.3, outer=F, padj=-1, srt=90) #adj=-0.45, padj=-3,
# p <- subset(lms,(pred=="rv" & measure=="SFD")) 
	# par(mar=mar2)
	# plot(1:6,p$Estimate, type="p", pch=19 ,ylim=c(-0.1,0.45)
		# ,xlab="EMMA Simulation", ylab="Estimate" ,main="Retrieval"
		# ,bty="n" ,xaxt="n", cex=pcex, cex.axis=acex, cex.lab=lcex, cex.main=1.5
		# )
	# axis(1,labels=c("EMMA",expression(+s[1]),expression(+r),expression(+s[2]),expression(+rs[1]),expression(+rs[2])), at=1:6, cex.axis=acex2, cex.lab=lcex)
	# abline(0,0, lty=2)
	# errbar(1:6,p$Estimate, p$CI.upper, p$CI.lower, add=TRUE)

p <- subset(lms,(pred=="sp" & measure=="FFD")) 
	par(mar=mar1)
	plot(1:6,p$Estimate, type="p", pch=19 ,ylim=c(-0.03,0.145)
		,xlab="EMMA Simulation", ylab="Estimate" ,main="Surprisal"
		,bty="n" ,xaxt="n", cex=pcex, cex.axis=acex, cex.lab=lcex, cex.main=1.8
		)
	errbar(1:6,p$Estimate, p$CI.upper, p$CI.lower, add=T)#, ,bty="n" ,xaxt="n", yaxt="n",ylab="",xlab="")
	axis(1,labels=c("EMMA",expression(+s[1]),expression(+r),expression(+s[2]),expression(+rs[1]),expression(+rs[2])), at=1:6, cex.axis=acex2, cex.lab=lcex)
	abline(0,0, lty=2)
	text(tx,ty, "FFD", cex=tcex, xpd=T)
p <- subset(lms,(pred=="rv" & measure=="FFD")) 
	par(mar=mar2)
	plot(1:6,p$Estimate, type="p", pch=19 ,ylim=c(-0.1,0.5)
		,xlab="EMMA Simulation", ylab="Estimate" ,main="Retrieval"
		,bty="n" ,xaxt="n", cex=pcex, cex.axis=acex, cex.lab=lcex, cex.main=1.8
		)
	axis(1,labels=c("EMMA",expression(+s[1]),expression(+r),expression(+s[2]),expression(+rs[1]),expression(+rs[2])), at=1:6, cex.axis=acex2, cex.lab=lcex)
	abline(0,0, lty=2)
	errbar(1:6,p$Estimate, p$CI.upper, p$CI.lower, add=TRUE)

p <- subset(lms,(pred=="sp" & measure=="gaze")) 
	par(mar=mar1)
	plot(1:6,p$Estimate, type="p", pch=19 ,ylim=c(-0.03,0.16)
		,xlab="EMMA Simulation", ylab="Estimate" #,main="Surprisal"
		,bty="n" ,xaxt="n", cex=pcex, cex.axis=acex, cex.lab=lcex
		)
	errbar(1:6,p$Estimate, p$CI.upper, p$CI.lower, add=T)#, ,bty="n" ,xaxt="n", yaxt="n",ylab="",xlab="")
	axis(1,labels=c("EMMA",expression(+s[1]),expression(+r),expression(+s[2]),expression(+rs[1]),expression(+rs[2])), at=1:6, cex.axis=acex2, cex.lab=lcex)
	abline(0,0, lty=2)
	text(tx,ty, "Gaze", cex=tcex, xpd=T)
p <- subset(lms,(pred=="rv" & measure=="gaze")) 
	par(mar=mar2)
	plot(1:6,p$Estimate, type="p", pch=19 ,ylim=c(-0.1,0.7)
		,xlab="EMMA Simulation", ylab="Estimate" #,main="Retrieval"
		,bty="n" ,xaxt="n", cex=pcex, cex.axis=acex, cex.lab=lcex
		)
	axis(1,labels=c("EMMA",expression(+s[1]),expression(+r),expression(+s[2]),expression(+rs[1]),expression(+rs[2])), at=1:6, cex.axis=acex2, cex.lab=lcex)
	abline(0,0, lty=2)
	errbar(1:6,p$Estimate, p$CI.upper, p$CI.lower, add=TRUE)

p <- subset(lms,(pred=="sp" & measure=="RPD")) 
	par(mar=mar1)
	plot(1:6,p$Estimate, type="p", pch=19 ,ylim=c(-0.03,0.25)
		,xlab="EMMA Simulation", ylab="Estimate" #,main="Surprisal"
		,bty="n" ,xaxt="n", cex=pcex, cex.axis=acex, cex.lab=lcex
		)
	errbar(1:6,p$Estimate, p$CI.upper, p$CI.lower, add=T)#, ,bty="n" ,xaxt="n", yaxt="n",ylab="",xlab="")
	axis(1,labels=c("EMMA",expression(+s[1]),expression(+r),expression(+s[2]),expression(+rs[1]),expression(+rs[2])), at=1:6, cex.axis=acex2, cex.lab=lcex)
	abline(0,0, lty=2)
	text(tx,0.1, "RPD", cex=tcex, xpd=T)
p <- subset(lms,(pred=="rv" & measure=="RPD")) 
	par(mar=mar2)
	plot(1:6,p$Estimate, type="p", pch=19 ,ylim=c(-0.1,1)
		,xlab="EMMA Simulation", ylab="Estimate" #,main="Retrieval"
		,bty="n" ,xaxt="n", cex=pcex, cex.axis=acex, cex.lab=lcex
		)
	axis(1,labels=c("EMMA",expression(+s[1]),expression(+r),expression(+s[2]),expression(+rs[1]),expression(+rs[2])), at=1:6, cex.axis=acex2, cex.lab=lcex)
	abline(0,0, lty=2)
	errbar(1:6,p$Estimate, p$CI.upper, p$CI.lower, add=TRUE)


p <- subset(lms,(pred=="sp" & measure=="FPREG")) 
	par(mar=mar1)
	plot(1:6,p$Estimate, type="p", pch=19 ,ylim=c(-0.03,0.145)
		,xlab="EMMA Simulation", ylab="Estimate" #,main="Surprisal"
		,bty="n" ,xaxt="n", cex=pcex, cex.axis=acex, cex.lab=lcex
		)
	errbar(1:6,p$Estimate, p$CI.upper, p$CI.lower, add=T)#, ,bty="n" ,xaxt="n", yaxt="n",ylab="",xlab="")
	axis(1,labels=c("EMMA",expression(+s[1]),expression(+r),expression(+s[2]),expression(+rs[1]),expression(+rs[2])), at=1:6, cex.axis=acex2, cex.lab=lcex)
	abline(0,0, lty=2)
	text(tx,ty, "REG", cex=tcex, xpd=T)
p <- subset(lms,(pred=="rv" & measure=="FPREG")) 
	par(mar=mar2)
	plot(1:6,p$Estimate, type="p", pch=19 ,ylim=c(-0.05,0.5)
		,xlab="EMMA Simulation", ylab="Estimate" #,main="Retrieval"
		,bty="n" ,xaxt="n", cex=pcex, cex.axis=acex, cex.lab=lcex
		)
	axis(1,labels=c("EMMA",expression(+s[1]),expression(+r),expression(+s[2]),expression(+rs[1]),expression(+rs[2])), at=1:6, cex.axis=acex2, cex.lab=lcex)
	abline(0,0, lty=2)
	errbar(1:6,p$Estimate, p$CI.upper, p$CI.lower, add=TRUE)

# p <- subset(lms,(pred=="sp" & measure=="REG")) 
	# par(mar=mar1)
	# plot(1:6,p$Estimate, type="p", pch=19 ,ylim=c(-0.06,0.145)
		# ,xlab="EMMA Simulation", ylab="Estimate" #main="Surprisal"
		# ,bty="n" ,xaxt="n", cex=pcex, cex.axis=acex, cex.lab=lcex
		# )
	# errbar(1:6,p$Estimate, p$CI.upper, p$CI.lower, add=T)#, ,bty="n" ,xaxt="n", yaxt="n",ylab="",xlab="")
	# axis(1,labels=c("EMMA",expression(+s[1]),expression(+r),expression(+s[2]),expression(+rs[1]),expression(+rs[2])), at=1:6, cex.axis=acex2, cex.lab=lcex)
	# abline(0,0, lty=2)
	# text(tx,ty, "REREAD", cex=tcex, xpd=T)
# p <- subset(lms,(pred=="rv" & measure=="REG")) 
	# par(mar=mar2)
	# plot(1:6,p$Estimate, type="p", pch=19 ,ylim=c(-0.1,0.45)
		# ,xlab="EMMA Simulation", ylab="Estimate" #,main="Retrieval"
		# ,bty="n" ,xaxt="n", cex=pcex, cex.axis=acex, cex.lab=lcex
		# )
	# axis(1,labels=c("EMMA",expression(+s[1]),expression(+r),expression(+s[2]),expression(+rs[1]),expression(+rs[2])), at=1:6, cex.axis=acex2, cex.lab=lcex)
	# abline(0,0, lty=2)
	# errbar(1:6,p$Estimate, p$CI.upper, p$CI.lower, add=TRUE)

