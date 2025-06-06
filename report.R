# Produce plots and tables for report

# Before: otoliths.csv (data),
#         curve_female.csv, curve_male.csv, curve_pooled.csv (output)
# After:  fit_female.csv, fit_male.csv, fit_pooled.png, samples.csv (report)

library(TAF)
library(areaplot)

mkdir("report")

# Read results
otoliths <- read.taf("data/otoliths.csv")
curve.female <- read.taf("output/curve_female.csv")
curve.male <- read.taf("output/curve_male.csv")
curve.pooled <- read.taf("output/curve_pooled.csv")

# Otolith samples
samples <- data.frame(sex=c("female", "male"),
                      n=tapply(otoliths$length, otoliths$sex, length),
                      min=tapply(otoliths$length, otoliths$sex, min),
                      median=tapply(otoliths$length, otoliths$sex, median),
                      max=tapply(otoliths$length, otoliths$sex, max),
                      row.names=NULL)
samples <- rnd(samples, -1)

# Prepare for plotting
f <- otoliths$sex == "female"
m <- otoliths$sex == "male"
red <- palette()[2]
blue <- palette()[4]

# Plot females
taf.png("fit_female", width=2400, height=2400, res=300)
plot(NA, xlim=c(0,22), ylim=c(0,320), xlab="Age (yrs)", ylab="Length (cm)")
grid()
confplot(cbind(lower,upper)~age, curve.female, col=adjustcolor(red, alpha=0.1),
         add=TRUE)
points(length~age, otoliths, subset=f, pch=16, col=adjustcolor(red, alpha=0.5))
lines(Lhat~age, curve.female, lwd=2, col=red)
lines(lower~age, curve.female, lty=1, lwd=0.5, col=red)
lines(upper~age, curve.female, lty=1, lwd=0.5, col=red)
dev.off()

# Plot males
taf.png("fit_male", width=2400, height=2400, res=300)
plot(NA, xlim=c(0,22), ylim=c(0,320), xlab="Age (yrs)", ylab="Length (cm)")
grid()
confplot(cbind(lower,upper)~age, curve.male, col=adjustcolor(blue, alpha=0.1),
         add=TRUE)
points(length~age, otoliths, subset=m, pch=16, col=adjustcolor(blue, alpha=0.5))
lines(Lhat~age, curve.male, lwd=2, col=blue)
lines(lower~age, curve.male, lty=1, lwd=0.5, col=blue)
lines(upper~age, curve.male, lty=1, lwd=0.5, col=blue)
dev.off()

# Plot pooled sexes
taf.png("fit_pooled", width=2400, height=2400, res=300)
plot(NA, xlim=c(0,22), ylim=c(0,320), xlab="Age (yrs)", ylab="Length (cm)")
grid()
confplot(cbind(lower,upper)~age, curve.pooled, col=gray(0.5, alpha=0.1),
         add=TRUE)
points(length~age, otoliths, pch=16, col=gray(0.4, alpha=0.5))
lines(Lhat~age, curve.pooled, lwd=2, col=gray(0.4))
lines(lower~age, curve.pooled, lty=1, lwd=0.5, col=gray(0.2))
lines(upper~age, curve.pooled, lty=1, lwd=0.5, col=gray(0.2))
dev.off()

# Write table
write.taf(samples, dir="report")
