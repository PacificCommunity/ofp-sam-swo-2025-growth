# Produce plots for report

# Before: otoliths.csv (data), conversions.RData, curve_female.csv,
#         curve_male.csv, curve_pooled.csv (output)
# After:  conversions.png, conversions_zoomed.png, fit_female.png, fit_male.png,
#         fit_pooled.png, fit_superimposed.png (report)

library(TAF)
library(areaplot)

mkdir("report")

# Read results
load("output/conversions.RData")
otoliths <- read.taf("data/otoliths.csv")
curve.female <- read.taf("output/curve_female.csv")
curve.male <- read.taf("output/curve_male.csv")
curve.pooled <- read.taf("output/curve_pooled.csv")

# Prepare for plotting
f <- otoliths$sex == "female"
m <- otoliths$sex == "male"
red <- palette()[2]
blue <- palette()[4]

# Plot conversions
taf.png("conversions", width=2400, height=2400, res=300)
eqs <- c("L = 1.1111 * E",
         "L = 6.898 + 1.0753 * E",
         "L = 7.7911 + 1.0647 * E",
         "L = 10.323 + 1.0559 * E")
cols <- c(8, 3, 5, 6)
plot(NA, xlim=c(0,22), ylim=c(0,320), xlab="Age (yrs)", ylab="Length (cm)")
grid()
lines(Lhat~age, c1, lwd=2, col=cols[1])
lines(Lhat~age, c2, lwd=2, col=cols[2])
lines(Lhat~age, c3, lwd=2, col=cols[3])
lines(Lhat~age, c4, lwd=2, col=cols[4])
legend("topleft", eqs, lwd=3, col=cols, bty="n", inset=0.02, y.intersp=1.25)
dev.off()

# Plot conversions (zoomed)
taf.png("conversions_zoomed", width=2400, height=2400, res=300)
plot(NA, xlim=c(0,6), ylim=c(80,200), xlab="Age (yrs)", ylab="Length (cm)")
abline(v=0:6, col="gray", lty=3)
abline(h=seq(80, 200, 10), col="gray", lty=3)
lines(Lhat~age, c1, lwd=2, col=cols[1])
lines(Lhat~age, c2, lwd=2, col=cols[2])
lines(Lhat~age, c3, lwd=2, col=cols[3])
lines(Lhat~age, c4, lwd=2, col=cols[4])
legend("topleft", eqs, lwd=3, col=cols, bty="n", inset=0.02, y.intersp=1.25)
dev.off()

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

# Plot superimposed sexes
taf.png("fit_superimposed", width=2400, height=2400, res=300)
plot(NA, xlim=c(0,22), ylim=c(0,320), xlab="Age (yrs)", ylab="Length (cm)")
grid()
points(length~age, otoliths, subset=f, pch=16, col=adjustcolor(red, alpha=0.5))
points(length~age, otoliths, subset=m, pch=16, col=adjustcolor(blue, alpha=0.5))
lines(Lhat~age, curve.female, lwd=2, col=red)
lines(Lhat~age, curve.male, lwd=2, col=blue)
dev.off()
