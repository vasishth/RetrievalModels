# #remove(list=ls())
# #library(ggplot2)	

# load("data/fstat.RData")

 modelname = "model"

#################################################
### Plots
#################################################

## sumstat1 on frequency
# postscript(paste("plots/src",modelname,"fstat1.eps",sep="-")
	# ,width=5, height=5
	# ,encoding="TeXtext.enc"
	# #,family="ComputerModern"
	# ,horizontal=FALSE
	# )

pdf(file = paste("plots/src",modelname,"fstat1.pdf",sep="-"),
    width=5, height=5)

plot(c(1,2,3,4,5),fstat1.m$FPRT,
	,type="b" ,pch=19, ylim=c(200,320), lwd=2
	#,main="PSC"
	,xlab="Frequency Class"
	,ylab="Duration (ms)"
	#,pty=s
	)
lines(fstat1$FPRT, lty=3, type="b", pch=1, lwd=2)
lines(fstat1.m$FFD, type="b", pch=15, lwd=2)
lines(fstat1$FFD, lty=3, type="b", pch=22, lwd=2)
lines(fstat1.m$SFD, type="b", pch=17, lwd=2)
lines(fstat1$SFD, lty=3, type="b", pch=2, lwd=2)
legend("topright",
       legend = c("model","data","Gaze","FFD","SFD")
       ,lty=c(1,3,0,0,0)
       ,pch=c(NA,NA,19,15,17)
       ,lwd=2
       ,cex=0.8     #magnification of legend
       ,xjust=1
       ,yjust=1
       #,merge=TRUE
       ,horiz=FALSE)#, trace=TRUE)

dev.off()
######################



# postscript(paste("plots/src",modelname,"fstat2.eps",sep="-")
	# ,width=5, height=5
	# ,encoding="TeXtext.enc"
	# #,family="ComputerModern"
	# ,horizontal=FALSE
	# )
pdf(file = paste("plots/src",modelname,"fstat2.pdf",sep="-"),
    width=5, height=5)


plot(1:5,fstat2.m$skipped,
	,type="b" ,pch=19, ylim=c(0,1), lwd=2
	#,main="PSC"
	,xlab="Frequency Class"
	,ylab="Probability"
	#,pty=s
	)
lines(fstat2$skipped, lty=3, type="b", pch=1, lwd=2)
lines(fstat2.m$onefix, type="b", pch=15, lwd=2)
lines(fstat2$onefix, lty=3, type="b", pch=22, lwd=2)
lines(fstat2.m$twofix, type="b", pch=17, lwd=2)
lines(fstat2$twofix, lty=3, type="b", pch=2, lwd=2)

legend("topright",
       legend = c("model","data","skip","once","multiple")
       ,lty=c(1,3,0,0,0)
       ,pch=c(NA,NA,19,15,17)
       ,lwd=2
       ,cex=0.7     #magnification of legend
       ,xjust=1
       ,yjust=1
       #,merge=TRUE
       ,horiz=FALSE)#, trace=TRUE)

dev.off()
######################
