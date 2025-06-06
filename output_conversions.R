# Extract results for alternative conversions, write CSV output tables

# Before: conversions.RData (model)
# After:  conversions.RData (output)

library(TAF)
library(RTMB)
library(fishgrowth)

mkdir("output")

# Read results
load("model/conversions.RData")

# Parameters
pfun <- function(model)
{
  par <- summary(sdreport(model))
  par <- data.frame(name=rownames(par), est=exp(par[,"Estimate"]),
                    log=par[,"Estimate"], se=par[,"Std. Error"], row.names=NULL)
  par$name <- sub("log_", "", par$name)
  par
}
p1 <- pfun(m1)
p2 <- pfun(m2)
p3 <- pfun(m3)
p4 <- pfun(m4)

# Growth curves
x <- seq(0.25, 22, 0.25)
c1 <- pred_band(x, m1)
c2 <- pred_band(x, m2)
c3 <- pred_band(x, m3)
c4 <- pred_band(x, m4)

# Save results
rm(f1, f2, f3, f4, L1, L2, L3, L4, m1, m2, m3, m4, pfun, r1, r2, r3, r4, x)
save(list=ls(), file="output/conversions.RData")
