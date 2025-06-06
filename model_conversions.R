# Run analysis for alternative conversions, write model results

# Before: otoliths.csv (data)
# After: conversions.RData (model)

library(TAF)
library(RTMB)
library(fishgrowth)

# Equations circulated by Paul Hamer via email on 5 June 2025
# for converting eye orbital fork length (E) measurements
# to lower jaw fork length (L) used in SPC assessments
# (1) L =          1.1111 * E  2021 assessment, no source reference
# (2) L =  6.898 + 1.0753 * E  2017 assessment, from Rob Campbell 2008 SC paper
# (3) L = 7.7911 + 1.0647 * E  ISC SWO assessment based on a paper by Sun et al.
# (4) L = 10.323 + 1.0559 * E  Young and Drake 2004 FRDC project report 2001/014

mkdir("model")

otoliths <- read.taf("data/otoliths.csv")
E <- otoliths$length / 1.1111
L1 <- 1.1111 * E
L2 <- 6.898 + 1.0753 * E
L3 <- 7.7911 + 1.0647 * E
L4 <- 10.323 + 1.0559 * E

# Prepare parameters and data
init <- list(log_L1=log(80), log_L2=log(240), log_k=log(0.2),
             log_sigma_min=log(8), log_sigma_max=log(16))

# Fit models
m1 <- vonbert(init, list(Aoto=otoliths$age, Loto=L1, t1=0.25, t2=15))
m2 <- vonbert(init, list(Aoto=otoliths$age, Loto=L2, t1=0.25, t2=15))
m3 <- vonbert(init, list(Aoto=otoliths$age, Loto=L3, t1=0.25, t2=15))
m4 <- vonbert(init, list(Aoto=otoliths$age, Loto=L4, t1=0.25, t2=15))
f1 <- nlminb(m1$par, m1$fn, m1$gr, control=list(eval.max=1e4, iter.max=1e4))
f2 <- nlminb(m2$par, m2$fn, m2$gr, control=list(eval.max=1e4, iter.max=1e4))
f3 <- nlminb(m3$par, m3$fn, m3$gr, control=list(eval.max=1e4, iter.max=1e4))
f4 <- nlminb(m4$par, m4$fn, m4$gr, control=list(eval.max=1e4, iter.max=1e4))
r1 <- m1$report()
r2 <- m2$report()
r3 <- m3$report()
r4 <- m4$report()

# Save results
rm(E, init, otoliths)
save(list=ls(), file="model/conversions.RData")
