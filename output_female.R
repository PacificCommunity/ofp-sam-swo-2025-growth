# Extract results for females, write CSV output tables

# Before: fit_female.rds, model_female.rds, report_female.rds (model)
# After:  curve_female.csv, par_female.csv (output)

library(TAF)
library(RTMB)
library(fishgrowth)

mkdir("output")

# Read results
fit <- readRDS("model/fit_female.rds")
model <- readRDS("model/model_female.rds")
report <- readRDS("model/report_female.rds")

# Parameters
par <- summary(sdreport(model))
par <- data.frame(name=rownames(par), est=exp(par[,"Estimate"]),
                  log=par[,"Estimate"], se=par[,"Std. Error"], row.names=NULL)
par$name <- sub("log_", "", par$name)

# Growth curve
x <- seq(0.25, 22, 0.25)
curve <- pred_band(x, model)

# Write tables
write.taf(par, "output/par_female.csv")
write.taf(curve, "output/curve_female.csv")
