remove(list=ls())
flush.console()
library(em)

#load("pscem.RData")
d <- read.table("src98.fixations.txt", header=TRUE)
colnames(d)[1:3] <- c("id","sn1","sn")
# head(d) # summary(d)

(dim(subset(d,duration<51))[1]) / dim(d)[1]
(dim(subset(d,duration>1000))[1]) / dim(d)[1]

d$sn <- as.factor(d$sn)
d$id <- as.factor(d$id)

etm <- with(d,
            em(region, duration,
               trialId=list(id,sn),
               trialInfo=list(id=id, sn=sn)))                              
# head(etm) # summary(etm)

save(etm, file="src.etm.RData")
#load("src.etm.RData")


################################################
### WORD INFO
################################################
wi <- read.table("src98.measures.txt", header=TRUE)
colnames(wi)[1] <- c("sn")
# head(wi)

## Add word count to word info
wi$nw <- 0
lastrois <- NULL
for(i in 1:max(wi$sn)){
	last_i <- max(wi$roi[wi$sn==i])
	lastrois <- append(lastrois,last_i)
	wi$nw[wi$sn==i] <- last_i
	}
# head(wi)
write.table(wi, "src98.measures.txt")


################################################
## Merge with measures
################################################
dim(etm)
d <- merge(etm, wi, by=c("sn","roi"), all.x=TRUE)
dim(d)
# head(d) # summary(d)


################################################
## (1) Remove some words from data
################################################
## Remove first and last word from data
d1 <- d
d <- subset(d, (roi>1 & roi<nw))
dim(d)

##### Some data cleaning
d1.1 <- d
dim(d)
## Remove words with FFD < 30ms
(dim(subset(d,(FFD<30 & FFD>0)))[1]) / dim(d)[1]
d <- subset(d,(FFD>=30 | FFD==0))
dim(d)
## Remove words with FFD > 1s
(dim(subset(d,FFD>1000))[1]) / dim(d)[1]
d <- subset(d,FFD<=1000)
dim(d)
## Remove words with TFT or FPRT > 1.5s
(dim(subset(d,TFT>1500 | FPRT>1500))[1]) / dim(d)[1]
d <- subset(d,(TFT<=1500 & FPRT<=1500))
dim(d)
## number of words removed
dim(d1.1)[1]-dim(d)[1]


################################################
## (2) Assign skipping and one- and two-fixation values
################################################
d$skipped <- 0
d$onefix <- 0
d$twofix <- 0
d$skipped[d$FFP==0] <- 1
d$skipped[is.na(d$FFP)] <- 1
d$onefix[d$SFD>0 & d$FFP==1] <- 1
d$twofix[d$SFD==0 & d$TFT>0 & d$FFP==1] <- 1
# head(d)


################################################
## (3) Assign Regression Origin and Regression Goal
##     probability
################################################
d$RO <- 0
d$RG <- 0
d$RO[d$TRC>0] <- 1
#d$RG[d$RRT>0] <- 1  # wrong!
# head(d)

save(d, file="d.RData")


