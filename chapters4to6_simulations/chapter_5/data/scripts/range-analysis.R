remove(list=ls())
#flush.console()
library(psych)

load("../PSC-CORPUS-DATA/d.RData")
#head(d)

load("../model-psc-shr/m.RData")
head(m)

describe((m$rv100*0.1) + (m$spu*0.015*1000))
hist(m$rv100*0.1)
hist(m$spu*0.015*1000)


load("../model-psc-sh/m.RData")
head(m)

describe(m$rv100*0.2)
describe(m$spu*0.02*1000)