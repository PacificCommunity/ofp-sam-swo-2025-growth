# Run analysis for both sexes, write model results

# Before: otoliths.csv (data)
# After: fit_both.rds, model_both.rds, report_both.rds (model)

library(TAF)
library(RTMB)
library(fishgrowth)

mkdir("model")

otoliths <- read.taf("data/otoliths.csv")

# Prepare parameters and data
init <- list(log_L1=log(80), log_L2=log(240), log_k=log(0.2),
             log_sigma_min=log(8), log_sigma_max=log(16))
dat <- list(Aoto=otoliths$age, Loto=otoliths$length, t1=0.25, t2=15)

# Fit model
model <- vonbert(init, dat)
fit <- nlminb(model$par, model$fn, model$gr,
              control=list(eval.max=1e4, iter.max=1e4))
report <- model$report()

# Save results
saveRDS(fit, "model/fit_both.rds")
saveRDS(model, "model/model_both.rds")
saveRDS(report, "model/report_both.rds")
