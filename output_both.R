# Extract results for both sexes, write CSV output tables

# Before: fit_both.rds, model_both.rds, report_both.rds (model)
# After:  curve_both.csv, par_both.csv (output)

library(TAF)
library(RTMB)
library(fishgrowth)

mkdir("output")

# Read results
fit <- readRDS("model/fit_both.rds")
model <- readRDS("model/model_both.rds")
report <- readRDS("model/report_both.rds")

# Parameters
par <- summary(sdreport(model))
par <- data.frame(name=rownames(par), est=exp(par[,"Estimate"]),
                  log=par[,"Estimate"], se=par[,"Std. Error"], row.names=NULL)
par$name <- sub("log_", "", par$name)

# Growth curve
x <- seq(0.25, 22, 0.25)
curve <- pred_band(x, model)

# Write tables
write.taf(par, "output/par_both.csv")
write.taf(curve, "output/curve_both.csv")
