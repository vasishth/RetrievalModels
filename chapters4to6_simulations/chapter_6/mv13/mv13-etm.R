library(em2)

f <- read.table("mv13/fixations.txt", header=T)
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
