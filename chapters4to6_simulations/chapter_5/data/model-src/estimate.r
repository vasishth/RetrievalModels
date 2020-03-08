remove(list=ls())
library(plyr)


#assign and display starting values and call parameter-estimation function
# encoding-factor ; encoding-exponent ; preptime

## three params:
startParms <- c(0.004, 0.004, 0.0130)
transvect <- c(1, 100, 10)
lower <- c(0.002, 0.003, 0.0110)
upper <- c(0.005, 0.005, 0.0135)

## two params:
#startParms <- c(0.003, 0.0125)
#transvect <- c(1, 10)
#lower <- c(0.001, 0.0100)
#upper <- c(0.006, 0.0135)


write.table(rbind(c(0,NA,startParms*transvect)), "output/estimation-r.txt", col.names=c("cycle", "score", "enc.fact", "enc.exp", "preptime"))


bestvalues <- list()
bestscore <- 1000
run <- 1
l <- 0
#################################
getpred <-function(parms) {
	l <<- l+1
	print(paste("Cycle: ",l))
	parms <- parms * transvect
	print(parms)
	parms1 <- parms
#	parms <- c(parms[1], 0.4, parms[2]) # insert non-estimated parameter
    result <- run.paramlist(parms)
	rscore <- ifelse(result!="NA", analyze(), NA)
	write.table(rbind(c(l,rscore,parms1)), "output/estimation-r.txt", append=T, col.names=FALSE)
	if(rscore<bestscore) {bestscore<<-rscore; bestvalues<<-parms}
    print(rscore)
    return(rscore)
   }
   

#parms <- startParms
run.paramlist <- function(parms) {
  parmstr <- paste(parms,collapse=" ")
  # send params to lisp
  cm <- paste("echo \"",parmstr,"\" > listener",sep="")
  system(cm)
  # wait for lisp to send result
  result <- system("cat listener", intern=T)
  if(result == "STOP") {run <<- 0; stop("Aborted from LISP side")}
  as.numeric(result)
}

analyze <- function() {  
  source("1-analyze.R")
  source("2-f-statistics.R")
  load("data/fstat.RData")

  ## penalize range difference and variance
  scores <- as.vector(1*c(fstat1.rmsd,fstat2.rmsd) + 1*(1 - c(fstat1.cor,fstat2.cor)) + 0.01*fstat.rangediff)
  pscore <- mean(scores, na.rm=T) #+ var(scores, na.rm=T) + 0.1*fstat.zeros
#  print(pscore)
  return(pscore)
#  return(mean(c(fstat1.rmsd,fstat2.rmsd),na.rm=T))
}


#################################

#xout <- optim(startParms, getpred, gr=NULL, method="Nelder-Mead", control=list(maxit=100,trace=1)) #to be equivalent to fminsearch

#xout <- optim(startParms, getpred, gr=NULL, method="Nelder-Mead", control=list(maxit=100,trace=1,reltol=2e-3)) #to be equivalent to fminsearch

xout <- optim(startParms, getpred, gr=NULL, method="L-BFGS-B", lower=lower, upper=upper, control=list(maxit=300,trace=2,factr=1e3))

#xout <- optim(startParms, getpred, gr=NULL, method="SANN", lower=lower, upper=upper, control=list(maxit=100, trace=2, factr=1e3, reltol=0.01, abstol=0.1))


# send stop command to lisp
if(run==1) system("echo STOP > listener")

print(bestvalues)
print(bestscore)
write.table(rbind(c(0,bestscore,bestvalues)), "output/estimation-r.txt", append=T, col.names=FALSE)
