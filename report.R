# Produce plot for both sexes

# Before: otoliths.csv (data), curve_both.csv (output)
# After:  fit_both.png, samples.csv (report)

library(TAF)
library(areaplot)

mkdir("report")

# Read results
otoliths <- read.taf("data/otoliths.csv")
curve <- read.taf("output/curve_both.csv")

# Otolith samples
samples <- data.frame(sex=c("female", "male"),
                      n=tapply(otoliths$length, otoliths$sex, length),
                      min=tapply(otoliths$length, otoliths$sex, min),
                      median=tapply(otoliths$length, otoliths$sex, median),
                      max=tapply(otoliths$length, otoliths$sex, max),
                      row.names=NULL)
samples <- rnd(samples, -1)

# Plot both sexes
taf.png("fit_both", width=2400, height=2400, res=300)
confplot(cbind(lower,upper)~age, curve, xlim=c(0,22), ylim=c(0,320),
         xlab="Age (yrs)", ylab="Length (cm)", col="mistyrose")
points(length~age, otoliths, pch=16, col="#0080a090")
lines(Lhat~age, curve, lwd=2, col=2)
lines(lower~age, curve, lty=1, lwd=0.5, col=2)
lines(upper~age, curve, lty=1, lwd=0.5, col=2)
dev.off()

# Write table
write.taf(samples, dir="report")
