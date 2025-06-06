# Produce tables for report

# Before: otoliths.csv (data), conversions.RData (output)
#         par_female.csv, par_male.csv, par_pooled.csv (output)
# After:  conversions.csv, samples.csv (report)

library(TAF)
library(areaplot)

mkdir("report")

# Read results
load("output/conversions.RData")
otoliths <- read.taf("data/otoliths.csv")
par.female <- read.taf("output/par_female.csv")
par.male <- read.taf("output/par_male.csv")
par.pooled <- read.taf("output/par_pooled.csv")

# Otolith samples
samples <- data.frame(sex=c("female", "male"),
                      n=tapply(otoliths$length, otoliths$sex, length),
                      min=tapply(otoliths$length, otoliths$sex, min),
                      median=tapply(otoliths$length, otoliths$sex, median),
                      max=tapply(otoliths$length, otoliths$sex, max),
                      row.names=NULL)
samples <- rnd(samples, -1)

# Compare conversions
eqs <- c("L = 1.1111 * E",
         "L = 6.898 + 1.0753 * E",
         "L = 7.7911 + 1.0647 * E",
         "L = 10.323 + 1.0559 * E")
par <- c(p1$est[1:3], p2$est[1:3], p3$est[1:3], p4$est[1:3])
par <- matrix(par, ncol=3, byrow=TRUE, dimnames=list(1:4, c("L1", "L2", "k")))
conversions <- data.frame(eq=eqs, par)

# Write tables
write.taf(conversions, dir="report")
write.taf(samples, dir="report")
