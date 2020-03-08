remove(list=ls())

#############################################

load("../PSC-CORPUS-DATA/d.RData")
#head(d)

## correlations:
with(d, cor(spu,rv100))
with(d, cor(spu,log(rv100)))

with(d, cor(spu,rv1))
with(d, cor(rv1,rv100))
with(d, cor(spu,sp100))
with(d, cor(spu,roi))
with(d, cor(sp100,roi))
with(d, cor(rv100,roi))
with(d, cor(rv1,roi))

max(d$rv100)
unique(d$word[d$rv100>700])
unique(d$word[d$rv100>800])
unique(d$sn[d$rv100>700])
unique(d$sn[d$rv100>800])
unique(d$roi[d$rv100>700])

