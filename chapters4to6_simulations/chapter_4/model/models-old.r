# TODO: model with subj as third cue.

# source("act-s.r")
# reset_params()
 

######################################################################
##
## Standard reflexive interference model with 4 conditions.
##
## Uses csim to compute fan
## Includes a variation of fan effect size determined by the activation of competing chunk
## Parameters:
##   abl= antecedent base level activation
##   dbl= distractor base level activation
## Returns a list of 
##   l= latencies by condition
##   m= misretrievals by condition
##   a= activations of antecedent and distractor AT RETRIEVAL for interference conditions
##   b= "BASE" activations without fan for interference conditions 
##
######################################################################
refl_model_4cond_orig <- function(iterations=1000, csim=cuesim, ND=1, cweights=c(strWeight(),semWeight()), print=0){
  ta <- NULL  ## collects target activations
  l <- NULL   ## collects latencies
  r <- NULL   ## collects retrievals
  baseact <- NULL   ## collects chunk base activations
  finalact <- NULL  ## collects chunk act including fan / interference
  for(i in 1:iterations){
    a <- matrix(rep(NA,8),nrow=4)   ## Chunk activations
    b <- NULL
    fs <- NULL
    ##
    ##
    ## a) MATCH/INT: full match with fan from partially matching chunk
      b[1] <- activation(fan=c(1,1), match=c(0,0), bl=blc, times=lp, weights=cweights)                  # +c +a
      b[2] <- activation(fan=c(NA,1), match=c(-1,0), bl=dbl, times=ldp, weights=cweights)      # -c +a
      fs[1] <- fan_strength(b[1],b[2])
      fs[2] <- fan_strength(b[2],b[1])
    a[1,1] <- activation(fan=c(1+(ND*(1+csim)*fs[1]), 1+(ND*fs[1])), match=c(0,0), bl=blc, times=lp, weights=cweights)  # +c +a
    a[1,2] <- activation(fan=c(NA, ND+((1)*fs[2])), match=c(-1,0), bl=dbl, times=ldp, weights=cweights)     # -c +a
    ##
    ## b) MATCH/NOINT: full match, no fan
    a[2,1] <- activation(fan=c(1,1), match=c(0,0), bl=blc, times=lp, weights=cweights)     # +c +a
    a[2,2] <- activation(fan=c(NA,NA), match=c(-1,-1), bl=dbl, times=ldp, weights=cweights) # -c -a
    ##
    ## c) MISMATCH/INT: partial match with fan from partially matching chunk
      b[3] <- activation(fan=c(1,NA), match=c(0,-1), bl=blc, times=lp, weights=cweights) # +c -a
      b[4] <- activation(fan=c(NA,1), match=c(-1,0), bl=dbl, times=ldp, weights=cweights) # -c +a
      fs[3] <- fan_strength(b[3],b[4])
      fs[4] <- fan_strength(b[4],b[3])
    a[3,1] <- activation(fan=c(1+(ND*(1+csim)*fs[3]), NA), match=c(0,-1), bl=blc, times=lp, weights=cweights) # +c -a
    a[3,2] <- activation(fan=c(NA, ND+((1+csim)*fs[4])), match=c(-1,0), bl=dbl, times=ldp, weights=cweights) # -c +a
    ##
    ## d) MISMATCH/NOINT: partial match, no fan
    a[4,1] <- activation(fan=c(1,NA), match=c(0,-1), bl=blc, times=lp, weights=cweights)   # +c -a
    a[4,2] <- activation(fan=c(NA,NA), match=c(-1,-1), bl=dbl, times=ldp, weights=cweights) # -c -a
    ##
    ##
    r <- rbind(r, apply(a,1,retrieve))  ## retrieved chunk index
    maxacts <- apply(a,1,max)           ## maximum activation per condition
    l <- rbind(l, latency(maxacts))     ## latencies
    ##
    ta <- rbind(ta, a[,1])              ## target activations
    #
    baseact <- rbind(baseact, b)
    finalact <- rbind(finalact, c(a[1,],a[3,]))
  }
  ## correct retrievals:
  c <- r           
  c[r!=1] <- 0
  ## retrieval failures:
  f <- r           
  f[r==0] <- 1; f[r!=0] <- 0
  ## misretrievals:
  m <- 1-c         
  m[f==1] <- 0
  ##
  if(print!=0) print(colMeans(l))
  if(print!=0) print(colMeans(m))
  if(print!=0) print(colMeans(f))
  colnames(baseact) <- c("match.int.ant","match.int.dis","mismatch.int.ant", "mismatch.int.dis")
  colnames(finalact) <- c("match.int.ant","match.int.dis","mismatch.int.ant", "mismatch.int.dis")
  return(list(l=l,c=c,f=f,m=m,baseact=baseact,act=finalact))
}



refl_model_4cond <- function(iterations=1000, csim=cuesim, ND=1, cweights=c(strWeight(),semWeight()), print=0){
  ta <- NULL  ## collects target activations
  l <- NULL   ## collects latencies
  r <- NULL   ## collects retrievals
  baseact <- NULL   ## collects chunk base activations
  finalact <- NULL  ## collects chunk act including fan / interference
  for(i in 1:iterations){
    a <- matrix(rep(NA,8),nrow=4)   ## Chunk activations
    b <- NULL
    fs <- NULL
    ##
    ##
    ## a) MATCH/INT: full match with fan from partially matching chunk
    res <- final_activation(c(1,1), c(NA,1), N2=ND, csim=cuesim, weights=cweights, bl1=blc, bl2=dbl, times1=lp, times2=ldp)
    a[1,1] <- res$a1
    a[1,2] <- res$a2
    b[1] <- res$b1
    b[2] <- res$b2
    ##
    ## b) MATCH/NOINT: full match, no fan
    res <- final_activation(c(1,1), c(NA,NA), N2=ND, csim=cuesim, weights=cweights, bl1=blc, bl2=dbl, times1=lp, times2=ldp)
    a[2,1] <- res$a1
    a[2,2] <- res$a2
    # b[1] <- res$b1
    # b[2] <- res$b2
    ##
    ## c) MISMATCH/INT: partial match with fan from partially matching chunk
    res <- final_activation(c(1,NA), c(NA,1), N2=ND, csim=cuesim, weights=cweights, bl1=blc, bl2=dbl, times1=lp, times2=ldp)
    a[3,1] <- res$a1
    a[3,2] <- res$a2
    b[3] <- res$b1
    b[4] <- res$b2
    ##
    ## d) MISMATCH/NOINT: partial match, no fan
    res <- final_activation(c(1,NA), c(NA,NA), N2=ND, csim=cuesim, weights=cweights, bl1=blc, bl2=dbl, times1=lp, times2=ldp)
    a[4,1] <- res$a1
    a[4,2] <- res$a2
    # b[1] <- res$b1
    # b[2] <- res$b2
    ##
    ##
    r <- rbind(r, apply(a,1,retrieve))  ## retrieved chunk index
    maxacts <- apply(a,1,max)           ## maximum activation per condition
    l <- rbind(l, latency(maxacts))     ## latencies
    ##
    ta <- rbind(ta, a[,1])              ## target activations
    #
    baseact <- rbind(baseact, b)
    finalact <- rbind(finalact, c(a[1,],a[3,]))
  }
  ## correct retrievals:
  c <- r           
  c[r!=1] <- 0
  ## retrieval failures:
  f <- r           
  f[r==0] <- 1; f[r!=0] <- 0
  ## misretrievals:
  m <- 1-c         
  m[f==1] <- 0
  ##
  if(print!=0) print(colMeans(l))
  if(print!=0) print(colMeans(m))
  if(print!=0) print(colMeans(f))
  colnames(baseact) <- c("match.int.ant","match.int.dis","mismatch.int.ant", "mismatch.int.dis")
  colnames(finalact) <- c("match.int.ant","match.int.dis","mismatch.int.ant", "mismatch.int.dis")
  return(list(l=l,c=c,f=f,m=m,baseact=baseact,act=finalact))
}


######################################################################
##
## Reflexive interference model including +subj as cue
##
######################################################################
refl_model_4cond_subj_cue <- function(iterations=1000, ND=1, cweights=c(strWeight(),semWeight(), semWeight()), subj=TRUE, print=0){
  SF <- ifelse(subj, 1, NA)   ## Distractor subject feature
  SFM <- ifelse(subj, 0, -1)   ## Subject feature match
  SFC <- ifelse(subj, 0, 1)   ## Cuesim multiplicator
  ta <- NULL  ## collects target activations
  l <- NULL   ## collects latencies
  r <- NULL   ## collects retrievals
  baseact <- NULL   ## collects chunk base activations
  finalact <- NULL  ## collects chunk act including fan / interference
  for(i in 1:iterations){
    a <- matrix(rep(NA,8),nrow=4)   ## Chunk activations
    b <- NULL
    fs <- NULL
    ##
    ## a) MATCH/INT: full match with fan from partially matching chunk
    res <- final_activation(c(1,1,1), c(NA,1,SF), N2=ND, csim=cuesim, weights=cweights, bl1=blc, bl2=dbl, times1=lp, times2=ldp)
    a[1,1] <- res$a1
    a[1,2] <- res$a2
    b[1] <- res$b1
    b[2] <- res$b2
    ##
    ## b) MATCH/NOINT:
    res <- final_activation(c(1,1,1), c(NA,NA,SF), N2=ND, csim=cuesim, weights=cweights, bl1=blc, bl2=dbl, times1=lp, times2=ldp)
    a[2,1] <- res$a1
    a[2,2] <- res$a2
    # b[] <- res$b1
    # b[] <- res$b2
    ##
    ## c) MISMATCH/INT: partial match with fan from partially matching chunk
    res <- final_activation(c(1,NA,1), c(NA,1,SF), N2=ND, csim=cuesim, weights=cweights, bl1=blc, bl2=dbl, times1=lp, times2=ldp)
    a[3,1] <- res$a1
    a[3,2] <- res$a2
    b[3] <- res$b1
    b[4] <- res$b2
    ##
    ## d) MISMATCH/NOINT: partial match, no fan
    res <- final_activation(c(1,NA,1), c(NA,NA,SF), N2=ND, csim=cuesim, weights=cweights, bl1=blc, bl2=dbl, times1=lp, times2=ldp)
    a[4,1] <- res$a1
    a[4,2] <- res$a2
    # b[] <- res$b1
    # b[] <- res$b2
    ##
    ##
    r <- rbind(r, apply(a,1,retrieve))  ## retrieved chunk index
    maxacts <- apply(a,1,max)           ## maximum activation per condition
    l <- rbind(l, latency(maxacts))     ## latencies
    ##
    ta <- rbind(ta, a[,1])              ## target activations
    #
    baseact <- rbind(baseact, b)
    finalact <- rbind(finalact, c(a[1,],a[3,]))
  }
  ## correct retrievals:
  c <- r           
  c[r!=1] <- 0
  ## retrieval failures:
  f <- r           
  f[r==0] <- 1; f[r!=0] <- 0
  ## misretrievals:
  m <- 1-c         
  m[f==1] <- 0
  ##
  if(print!=0) print(colMeans(l))
  if(print!=0) print(colMeans(m))
  if(print!=0) print(colMeans(f))
  colnames(baseact) <- c("match.int.ant","match.int.dis","mismatch.int.ant", "mismatch.int.dis")
  colnames(finalact) <- c("match.int.ant","match.int.dis","mismatch.int.ant", "mismatch.int.dis")
  return(list(l=l,c=c,f=f,m=m,baseact=baseact,act=finalact))
}






######################################################################
##
## Multiple feature mismatch model (Parker & Phillips, 2014)
##
######################################################################
refl_model_4cond_mult_feature_mismatch_orig <- function(iterations=1000, csim=cuesim, ND=1, cweights=c(strWeight(),semWeight(),semWeight()), print=0){
  ta <- NULL  ## collects target activations
  l <- NULL   ## collects latencies
  r <- NULL   ## collects retrievals
  baseact <- NULL   ## collects chunk base activations
  finalact <- NULL  ## collects chunk act including fan / interference
  for(i in 1:iterations){
    a <- matrix(rep(NA,12),ncol=2)   ## Chunk activations
    b <- NULL
    fs <- NULL
    ##
    ##
    ## a) MATCH/INT: full match with fan from partially matching chunk
      b[1] <- activation(fan=c(1,1,1), match=c(0,0,0), bl=blc, times=lp, weights=cweights)         # +c +g +n
      b[2] <- activation(fan=c(NA,1,1), match=c(-1,0,0), bl=dbl, times=ldp, weights=cweights)      # -c +g +n
      fs[1] <- fan_strength(b[1],b[2])
      fs[2] <- fan_strength(b[2],b[1])
    a[1,1] <- activation(fan=c(1+(ND*(1+csim)*fs[1]), 1+(ND*fs[1]), 1+(ND*fs[1])), match=c(0,0,0), bl=blc, times=lp, weights=cweights) 
    a[1,2] <- activation(fan=c(NA, ND+(1*fs[2]), ND+(1*fs[2])), match=c(-1,0,0), bl=dbl, times=ldp, weights=cweights)
    ##
    ## b) MATCH/NOINT: full match, no fan
      b[3] <- activation(fan=c(1,1,1), match=c(0,0,0), bl=blc, times=lp, weights=cweights)         # +c +g +n
      b[4] <- activation(fan=c(NA,NA,1), match=c(-1,-1,0), bl=dbl, times=ldp, weights=cweights)    # -c -g +n
      fs[3] <- fan_strength(b[3],b[4])
      fs[4] <- fan_strength(b[4],b[3])
    a[2,1] <- activation(fan=c(1+(ND*(1+csim)*fs[3]),1+(ND*(1+csim)*fs[3]),1+(ND*fs[3])), match=c(0,0,0), bl=blc, times=lp, weights=cweights)     # +c +g +n
    a[2,2] <- activation(fan=c(NA,NA,ND+(1*fs[4])), match=c(-1,-1,0), bl=dbl, times=ldp, weights=cweights) # -c -g +n
    ##
    ## c) 1MISMATCH/INT: partial match with fan from partially matching chunk
      b[5] <- activation(fan=c(1,NA,1), match=c(0,-1,0), bl=blc, times=lp, weights=cweights) # +c -g +n
      b[6] <- activation(fan=c(NA,1,1), match=c(-1,0,0), bl=dbl, times=ldp, weights=cweights) # -c +g +n
      fs[5] <- fan_strength(b[5],b[6])
      fs[6] <- fan_strength(b[6],b[5])
    a[3,1] <- activation(fan=c(1+(ND*(1+csim)*fs[5]), NA, 1+(ND*fs[5])), match=c(0,-1,0), bl=blc, times=lp, weights=cweights)
    a[3,2] <- activation(fan=c(NA, ND+((1+csim)*fs[6]), ND+(1*fs[6])), match=c(-1,0,0), bl=dbl, times=ldp, weights=cweights)
    ##
    ## d) 1MISMATCH/NOINT: partial match, no fan
      b[7] <- activation(fan=c(1,NA,1), match=c(0,-1,0), bl=blc, times=lp, weights=cweights)       # +c -g +n
      b[8] <- activation(fan=c(NA,NA,1), match=c(-1,-1,0), bl=dbl, times=ldp, weights=cweights)    # -c -g +n
      fs[7] <- fan_strength(b[7],b[8])
      fs[8] <- fan_strength(b[8],b[7])
    a[4,1] <- activation(fan=c(1+(ND*(1+csim)*fs[7]),NA,1+(ND*fs[7])), match=c(0,-1,0), bl=blc, times=lp, weights=cweights)   # +c -g +n
    a[4,2] <- activation(fan=c(NA,NA,ND+(1*fs[8])), match=c(-1,-1,0), bl=dbl, times=ldp, weights=cweights) # -c -g +n
    ##
    ## e) 2MISMATCH/INT: 
      b[9] <- activation(fan=c(1,NA,NA), match=c(0,-1,-1), bl=blc, times=lp, weights=cweights) # +c -g -n
      b[10] <- activation(fan=c(NA,1,1), match=c(-1,0,0), bl=dbl, times=ldp, weights=cweights) # -c +g +n
      fs[9] <- fan_strength(b[9],b[10])
      fs[10] <- fan_strength(b[10],b[9])
    a[5,1] <- activation(fan=c(1+(ND*(1+csim)*fs[9]), NA, NA), match=c(0,-1,-1), bl=blc, times=lp, weights=cweights)
    a[5,2] <- activation(fan=c(NA, ND+((1+csim)*fs[10]), ND+((1+csim)*fs[10])), match=c(-1,0,0), bl=dbl, times=ldp, weights=cweights)
    ##
    ## f) 2MISMATCH/NOINT: 
      b[11] <- activation(fan=c(1,NA,NA), match=c(0,-1,-1), bl=blc, times=lp, weights=cweights)   # +c -g -n
      b[12] <- activation(fan=c(NA,NA,1), match=c(-1,-1,0), bl=dbl, times=ldp, weights=cweights)  # -c -g +n
      fs[11] <- fan_strength(b[11],b[12])
      fs[12] <- fan_strength(b[12],b[11])
    a[6,1] <- activation(fan=c(1+(ND*(1+csim)*fs[11]), NA, NA), match=c(0,-1,-1), bl=blc, times=lp, weights=cweights)   # +c -g +n
    a[6,2] <- activation(fan=c(NA,NA,ND+((1+csim)*fs[12])), match=c(-1,-1,0), bl=dbl, times=ldp, weights=cweights) # -
    ##
    r <- rbind(r, apply(a,1,retrieve))  ## retrieved chunk index
    maxacts <- apply(a,1,max)           ## maximum activation per condition
    l <- rbind(l, latency(maxacts))     ## latencies
    ##
    ta <- rbind(ta, a[,1])              ## target activations
    #
    baseact <- rbind(baseact, b)
    # finalact <- rbind(finalact, c(a[1,],a[3,]))
  }
  ## correct retrievals:
  c <- r           
  c[r!=1] <- 0
  ## retrieval failures:
  f <- r           
  f[r==0] <- 1; f[r!=0] <- 0
  ## misretrievals:
  m <- 1-c         
  m[f==1] <- 0
  ##
  if(print!=0) print(colMeans(l))
  if(print!=0) print(colMeans(m))
  if(print!=0) print(colMeans(f))
  # colnames(baseact) <- c("match.int.ant","match.int.dis","mismatch.int.ant", "mismatch.int.dis")
  # colnames(finalact) <- c("match.int.ant","match.int.dis","mismatch.int.ant", "mismatch.int.dis")
  return(list(l=l,c=c,m=m,f=f,baseact=baseact))
}




######################################################################
##
## Multiple feature mismatch model (Parker & Phillips, 2014)
##
######################################################################
refl_model_4cond_mult_feature_mismatch <- function(iterations=1000, csim=cuesim, ND=1, cweights=c(strWeight(),semWeight(),semWeight()), print=0){
  SF <- ifelse(subj, 1, NA)   ## Distractor subject feature
  ta <- NULL  ## collects target activations
  l <- NULL   ## collects latencies
  r <- NULL   ## collects retrievals
  baseact <- NULL   ## collects chunk base activations
  finalact <- NULL  ## collects chunk act including fan / interference
  for(i in 1:iterations){
    a <- matrix(rep(NA,12),ncol=2)   ## Chunk activations
    b <- NULL
    fs <- NULL
    ##
    ##
    ## a) MATCH/INT: full match with fan from partially matching chunk
    ## +c +g +n +s
    ## -c +g +n +s
    res <- final_activation(c(1,1,1), c(NA,1,1), N2=ND, csim=cuesim, weights=cweights, bl1=blc, bl2=dbl, times1=lp, times2=ldp)
    a[1,1] <- res$a1
    a[1,2] <- res$a2
    b[1] <- res$b1
    b[2] <- res$b2
    ##
    ## b) MATCH/NOINT: full match, no fan
    res <- final_activation(c(1,1,1), c(NA,NA,1), N2=ND, csim=cuesim, weights=cweights, bl1=blc, bl2=dbl, times1=lp, times2=ldp)
    a[2,1] <- res$a1
    a[2,2] <- res$a2
    b[3] <- res$b1
    b[4] <- res$b2
    ##
    ## c) 1MISMATCH/INT: partial match with fan from partially matching chunk
    res <- final_activation(c(1,NA,1), c(NA,1,1), N2=ND, csim=cuesim, weights=cweights, bl1=blc, bl2=dbl, times1=lp, times2=ldp)
    a[3,1] <- res$a1
    a[3,2] <- res$a2
    b[5] <- res$b1
    b[6] <- res$b2
    ##
    ## d) 1MISMATCH/NOINT: partial match, no fan
    res <- final_activation(c(1,NA,1), c(NA,NA,1), N2=ND, csim=cuesim, weights=cweights, bl1=blc, bl2=dbl, times1=lp, times2=ldp)
    a[4,1] <- res$a1
    a[4,2] <- res$a2
    b[7] <- res$b1
    b[8] <- res$b2
    ##
    ## e) 2MISMATCH/INT: 
    res <- final_activation(c(1,NA,NA), c(NA,1,1), N2=ND, csim=cuesim, weights=cweights, bl1=blc, bl2=dbl, times1=lp, times2=ldp)
    a[5,1] <- res$a1
    a[5,2] <- res$a2
    b[9] <- res$b1
    b[10] <- res$b2
    ##
    ## f) 2MISMATCH/NOINT: 
    res <- final_activation(c(1,NA,NA), c(NA,NA,1), N2=ND, csim=cuesim, weights=cweights, bl1=blc, bl2=dbl, times1=lp, times2=ldp)
    a[6,1] <- res$a1
    a[6,2] <- res$a2
    b[11] <- res$b1
    b[12] <- res$b2
    ##
    r <- rbind(r, apply(a,1,retrieve))  ## retrieved chunk index
    maxacts <- apply(a,1,max)           ## maximum activation per condition
    l <- rbind(l, latency(maxacts))     ## latencies
    ##
    ta <- rbind(ta, a[,1])              ## target activations
    #
    baseact <- rbind(baseact, b)
    # finalact <- rbind(finalact, c(a[1,],a[3,]))
  }
  ## correct retrievals:
  c <- r           
  c[r!=1] <- 0
  ## retrieval failures:
  f <- r           
  f[r==0] <- 1; f[r!=0] <- 0
  ## misretrievals:
  m <- 1-c         
  m[f==1] <- 0
  ##
  if(print!=0) print(colMeans(l))
  if(print!=0) print(colMeans(m))
  if(print!=0) print(colMeans(f))
  # colnames(baseact) <- c("match.int.ant","match.int.dis","mismatch.int.ant", "mismatch.int.dis")
  # colnames(finalact) <- c("match.int.ant","match.int.dis","mismatch.int.ant", "mismatch.int.dis")
  return(list(l=l,c=c,m=m,f=f,baseact=baseact))
}




######################################################################
##
## Multiple feature mismatch model (Parker & Phillips, 2014)
##
######################################################################
refl_model_4cond_mult_feature_mismatch_subj_cue <- function(iterations=1000, csim=cuesim, ND=1, cweights=c(strWeight(),semWeight(),semWeight(),semWeight()), subj=TRUE, print=0){
  SF <- ifelse(subj, 1, NA)   ## Distractor subject feature
  ta <- NULL  ## collects target activations
  l <- NULL   ## collects latencies
  r <- NULL   ## collects retrievals
  baseact <- NULL   ## collects chunk base activations
  finalact <- NULL  ## collects chunk act including fan / interference
  for(i in 1:iterations){
    a <- matrix(rep(NA,12),ncol=2)   ## Chunk activations
    b <- NULL
    fs <- NULL
    ##
    ##
    ## a) MATCH/INT: full match with fan from partially matching chunk
    ## +c +g +n +s
    ## -c +g +n +s
    res <- final_activation(c(1,1,1,1), c(NA,1,1,SF), N2=ND, csim=cuesim, weights=cweights, bl1=blc, bl2=dbl, times1=lp, times2=ldp)
    a[1,1] <- res$a1
    a[1,2] <- res$a2
    b[1] <- res$b1
    b[2] <- res$b2
    ##
    ## b) MATCH/NOINT: full match, no fan
    res <- final_activation(c(1,1,1,1), c(NA,NA,1,SF), N2=ND, csim=cuesim, weights=cweights, bl1=blc, bl2=dbl, times1=lp, times2=ldp)
    a[2,1] <- res$a1
    a[2,2] <- res$a2
    b[3] <- res$b1
    b[4] <- res$b2
    ##
    ## c) 1MISMATCH/INT: partial match with fan from partially matching chunk
    res <- final_activation(c(1,NA,1,1), c(NA,1,1,SF), N2=ND, csim=cuesim, weights=cweights, bl1=blc, bl2=dbl, times1=lp, times2=ldp)
    a[3,1] <- res$a1
    a[3,2] <- res$a2
    b[5] <- res$b1
    b[6] <- res$b2
    ##
    ## d) 1MISMATCH/NOINT: partial match, no fan
    res <- final_activation(c(1,NA,1,1), c(NA,NA,1,SF), N2=ND, csim=cuesim, weights=cweights, bl1=blc, bl2=dbl, times1=lp, times2=ldp)
    a[4,1] <- res$a1
    a[4,2] <- res$a2
    b[7] <- res$b1
    b[8] <- res$b2
    ##
    ## e) 2MISMATCH/INT: 
    res <- final_activation(c(1,NA,NA,1), c(NA,1,1,SF), N2=ND, csim=cuesim, weights=cweights, bl1=blc, bl2=dbl, times1=lp, times2=ldp)
    a[5,1] <- res$a1
    a[5,2] <- res$a2
    b[9] <- res$b1
    b[10] <- res$b2
    ##
    ## f) 2MISMATCH/NOINT: 
    res <- final_activation(c(1,NA,NA,1), c(NA,NA,1,SF), N2=ND, csim=cuesim, weights=cweights, bl1=blc, bl2=dbl, times1=lp, times2=ldp)
    a[6,1] <- res$a1
    a[6,2] <- res$a2
    b[11] <- res$b1
    b[12] <- res$b2
    ##
    r <- rbind(r, apply(a,1,retrieve))  ## retrieved chunk index
    maxacts <- apply(a,1,max)           ## maximum activation per condition
    l <- rbind(l, latency(maxacts))     ## latencies
    ##
    ta <- rbind(ta, a[,1])              ## target activations
    #
    baseact <- rbind(baseact, b)
    # finalact <- rbind(finalact, c(a[1,],a[3,]))
  }
  ## correct retrievals:
  c <- r           
  c[r!=1] <- 0
  ## retrieval failures:
  f <- r           
  f[r==0] <- 1; f[r!=0] <- 0
  ## misretrievals:
  m <- 1-c         
  m[f==1] <- 0
  ##
  if(print!=0) print(colMeans(l))
  if(print!=0) print(colMeans(m))
  if(print!=0) print(colMeans(f))
  # colnames(baseact) <- c("match.int.ant","match.int.dis","mismatch.int.ant", "mismatch.int.dis")
  # colnames(finalact) <- c("match.int.ant","match.int.dis","mismatch.int.ant", "mismatch.int.dis")
  return(list(l=l,c=c,m=m,f=f,baseact=baseact))
}





########################################################
##
## Test-run models
##
########################################################
# reset_params()

# lf <<- 0.4
# mp <<- 0.2
# ans <<- 0.15
# cuesim <<- -1
# fsf <<- 0
# mas <<- 1
# rth <<- -1.5
# ldp <<- 1.5
# cueweighting <<- 1
# normalizeWeights <<- FALSE
# model <- refl_model_4cond_mult_feature_mismatch(iterations=3000,print=1)
# with(model, c(colMeans(l)[1]-colMeans(l)[2],colMeans(l)[3]-colMeans(l)[4],colMeans(l)[5]-colMeans(l)[6]))


# ## Original model: inhib/facil
# gaze
# rbrt
# lf <<- .15
# mp <<- 1.2
# ans <<- 0.15
# cuesim <<- 0
# fsf <<- 0
# mas <<- 3
# w <<- 1
# dbl <<- 0
# model <- refl_model_4cond(print=1)
# with(model, c(colMeans(l)[1]-colMeans(l)[2],colMeans(l)[3]-colMeans(l)[4]))

# ## Original model, prominent distr.:
# dbl <<- 1
# model <- refl_model_4cond(print=1)
# with(model, c(colMeans(l)[1]-colMeans(l)[2],colMeans(l)[3]-colMeans(l)[4]))



# ## Exp1: none/inhib
# lf <<- .3
# mp <<- 1
# ans <<- 0.2
# cuesim <<- -1
# fsf <<- 4
# dbl <<- 0
# model <- refl_model_4cond(iterations=3000,print=1)
# with(model, c(colMeans(l)[1]-colMeans(l)[2],colMeans(l)[3]-colMeans(l)[4]))


# ## Prominent distr. 
# lf <<- .15
# mp <<- 1.2
# ans <<- 0.15
# cuesim <<- -1
# fsf <<- 0
# dbl <<- 1
# model <- refl_model_4cond(print=1)
# with(model, c(colMeans(l)[1]-colMeans(l)[2],colMeans(l)[3]-colMeans(l)[4]))


# ## Exp2: mult distr
# lf <<- .15
# mp <<- 1.2
# ans <<- 0.15
# cuesim <<- 0
# fsf <<- 0
# mas <<- 3
# w <<- 1
# dbl <<- 0
# model <- refl_model_4cond_mult_distr(print=1)
# with(model, c(colMeans(l)[1]-colMeans(l)[2],colMeans(l)[3]-colMeans(l)[4]))





# ########################################################
# #########################################################

# ## Exp2: inhib/-
# gaze
# rbrt
# lf <- .13
# mp <- 1.2
# cuesim <- -.6
# fsf <- 4
# ans <- 0.1
# model <- similarity_model_3_exp2(1000, csim=cuesim, data=exp2, print=2)
# fit2(exp2, round(colMeans(model$l)), print=2)

# ## Multiple distr.: inhib/-
# gaze
# rbrt
# lf <- .15
# mp <- 1.2
# cuesim <- -.3
# fsf <- 4
# ans <- 0.15
# model <- similarity_model_3_mult_distr(1000, csim=cuesim, data=exp2, print=2)
# fit(rbrt, round(colMeans(model$l)), print=2)



# ## none/none
# gaze
# rbrt
# lf <- .13
# mp <- 1.2
# cuesim <- -.8
# fsf <- 4
# ans <- 0.1
# similarity_model_3_exp1(1000, csim=cuesim, data=gaze, print=2)

# ## none/facil
# gaze
# rbrt
# lf <- .13
# mp <- 1.2
# cuesim <- -.9
# fsf <- 4
# ans <- 0.1
# similarity_model_3_exp1(1000, csim=cuesim, data=gaze, print=2)

# ## inhib/facil
# gaze
# rbrt
# lf <- .13
# mp <- 1.2
# cuesim <- -.9
# fsf <- 1
# ans <- 0.1
# similarity_model_3_exp1(1000, csim=cuesim, data=gaze, print=2)

# ## inhib/none
# gaze
# rbrt
# lf <- .13
# mp <- 1.2
# cuesim <- -.8
# fsf <- 1
# ans <- 0.1
# similarity_model_3_exp1(1000, csim=cuesim, data=gaze, print=2)


# ## inhib/-
# gaze
# rbrt
# lf <- .13
# mp <- 1.2
# cuesim <- 0
# fsf <- 4
# ans <- 0.1
# similarity_model_3_exp1(1000, csim=cuesim, data=gaze, print=2)


