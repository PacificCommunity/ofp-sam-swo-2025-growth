# Extract results for pooled sexes, write CSV output tables

# Before: fit_pooled.rds, model_pooled.rds, report_pooled.rds (model)
# After:  curve_pooled.csv, par_pooled.csv (output)

library(TAF)
library(RTMB)
library(fishgrowth)

mkdir("output")

# Read results
fit <- readRDS("model/fit_pooled.rds")
model <- readRDS("model/model_pooled.rds")
report <- readRDS("model/report_pooled.rds")

# Parameters
par <- summary(sdreport(model))
par <- data.frame(name=rownames(par), est=exp(par[,"Estimate"]),
                  log=par[,"Estimate"], se=par[,"Std. Error"], row.names=NULL)
par$name <- sub("log_", "", par$name)

# Growth curve
x <- seq(0.25, 22, 0.25)
curve <- pred_band(x, model)

# Write tables
write.taf(par, "output/par_pooled.csv")
write.taf(curve, "output/curve_pooled.csv")
