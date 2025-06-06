# Extract results for males, write CSV output tables

# Before: fit_male.rds, model_male.rds, report_male.rds (model)
# After:  curve_male.csv, par_male.csv (output)

library(TAF)
library(RTMB)
library(fishgrowth)

mkdir("output")

# Read results
fit <- readRDS("model/fit_male.rds")
model <- readRDS("model/model_male.rds")
report <- readRDS("model/report_male.rds")

# Parameters
par <- summary(sdreport(model))
par <- data.frame(name=rownames(par), est=exp(par[,"Estimate"]),
                  log=par[,"Estimate"], se=par[,"Std. Error"], row.names=NULL)
par$name <- sub("log_", "", par$name)

# Growth curve
x <- seq(0.25, 22, 0.25)
curve <- pred_band(x, model)

# Write tables
write.taf(par, "output/par_male.csv")
write.taf(curve, "output/curve_male.csv")
