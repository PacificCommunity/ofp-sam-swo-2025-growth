# Produce tables for report

# Before: otoliths.csv (data), conversions.RData (output)
#         par_female.csv, par_male.csv, par_pooled.csv (output)
# After:  samples.csv (report)

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

# Write table
write.taf(samples, dir="report")
