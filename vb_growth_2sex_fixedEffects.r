

# Nicholas Ducharme-Barth
# 10/07/2021
# Estimate fixed-effects 2-sex VB growth model using STAN
# Good summary of alternative parametrizations for VB growth: https://derekogle.com/NCNRS349/modules/Growth/BKG
# Code for models influenced by:
# https://github.com/colemonnahan/gradmcmc/blob/v1.0/models/growth_nc/growth_nc.stan
# https://discourse.mc-stan.org/t/programming-a-simple-binomial-glm-with-logit-link/3101

#________________________________________________________________________________________________________________________________________________________________________________________________________
# load packages
library(rstan)
library(data.table)
library(magrittr)

#________________________________________________________________________________________________________________________________________________________________________________________________________
# load data
data.dt = fread("boot/data/SWO_age_data_for_SPC_200421.csv")
tmp.dt = data.dt %>% .[,.(length_cm,decimal_age,sex)] %>% setnames(.,c("length_cm","decimal_age"),c("ofl_cm","otolith_age")) %>% .[,sex:=factor(sex,levels=c("Female","Male"),labels=c("F","M"))] %>% na.omit(.)

#________________________________________________________________________________________________________________________________________________________________________________________________________
# prep stan.data, as named list
Nobs = as.integer(nrow(tmp.dt))
Nsex = as.integer(length(unique(tmp.dt$sex)))
lengths = tmp.dt$ofl_cm * 1.1111 # Add conversion from EOFL to LJFL since this is what the stock assessment uses as the standard length!!!
sex = as.integer(tmp.dt$sex)
ages = tmp.dt$otolith_age

stan.data = list(Nsex = Nsex,
                 Nobs = Nobs,
                 lengths = lengths,
                 sex = sex,
                 ages = ages,
                 L1_age = as.integer(c(1,1)),
                 L2_age = as.integer(c(20,20)))
stan.data.priors = list(PriorMean_t0 = c(-2.13,-2.10),
                        PriorSD_t0 = c(5,5),
                        PriorMean_logk = c(log(0.16),log(0.24)),
                        PriorSD_logk = c(0.5,0.5),
                        PriorMean_logLinf = c(log(276),log(212)),
                        PriorSD_logLinf = c(0.5,0.5),
                        PriorMean_logSigma = c(log(26),log(26)),
                        PriorSD_logSigma = c(0.5,0.5))
stan.data = c(stan.data,stan.data.priors)

#________________________________________________________________________________________________________________________________________________________________________________________________________
# prep stan.inits, as named list
inits.func = function(Nsex=2)
{
  list(t0 = runif(Nsex,-3,1),
       logk = runif(Nsex,log(0.1),log(0.3)),
       logLinf = runif(Nsex,log(180), log(320)),
       logSigma = runif(Nsex,log(5),log(50)))
}

#________________________________________________________________________________________________________________________________________________________________________________________________________
# run stan
seed  = 123
set.seed(seed)
chains = 8
iter = 5000
burnin.prop = 0.2
options(mc.cores = 3)
file.name = "vb_growth_2sex_fixedEffects.stan" # path to stan file
stan.inits = replicate(chains,inits.func(),simplify=FALSE)
fit = stan(file = file.name,
           data = stan.data,
           init = stan.inits,
           chains = chains,
           warmup = iter*burnin.prop,
           iter = iter,
           thin = 1,
           seed = seed,
           control = list(adapt_delta = 0.80,max_treedepth=10))

#________________________________________________________________________________________________________________________________________________________________________________________________________
# STAN diagnostics
print(fit, digits = 3)
check_hmc_diagnostics(fit)
# extract chains
fit.df = as.data.frame(as.matrix(fit))
fit.array = as.array(fit)
# use bayesplot
fit.lp = bayesplot::log_posterior(fit)
fit.np = bayesplot::nuts_params(fit)
bayesplot::color_scheme_set("darkgray")
# bayesplot::mcmc_parcoord(fit.chains[,1:10], np = fit.np)
bayesplot::mcmc_pairs(fit.array, pars=c("L1[1]","L2[1]","k[1]","L1[2]","L2[2]","k[2]"), np = fit.np,off_diag_args = list(size = 0.75))
bayesplot::color_scheme_set("mix-brightblue-gray")
bayesplot::mcmc_trace(fit.array, pars = c("L1[1]","L2[1]","k[1]","L1[2]","L2[2]","k[2]"), np = fit.np)

#________________________________________________________________________________________________________________________________________________________________________________________________________
# make plots
# plot priors with posteriors
png("vb_growth_2sex_fixedEffects.png", width = 16, height = 9, units = "in",  res = 300)
par(mfrow=c(2,4))
for(target_variable in c("t0[1]","t0[2]","logk[1]","logk[2]","logLinf[1]","logLinf[2]","logSigma[1]","logSigma[2]"))
{
  x.pad = 0.25*diff(range(fit.df[,target_variable]))
  x.rng = c(min(c(min(fit.df[,target_variable])-x.pad)),max(fit.df[,target_variable])+x.pad)
  hist(fit.df[,target_variable],freq=FALSE,xlab=target_variable,main="",xlim=x.rng)
  if(grepl( "[", target_variable, fixed = TRUE) & grepl( "]", target_variable, fixed = TRUE))
  {
    target_variable.stem = strsplit(target_variable,"[[]")[[1]][1]
    target_variable.idx = as.numeric(gsub("]","",strsplit(target_variable,"[[]")[[1]][2]))
    lines(seq(from=x.rng[1],to=x.rng[2],length.out=1000),dnorm(seq(from=x.rng[1],to=x.rng[2],length.out=1000),stan.data.priors[[paste0("PriorMean_",target_variable.stem )]][target_variable.idx],stan.data.priors[[paste0("PriorSD_",target_variable.stem )]][target_variable.idx]),col="red",lwd=2)

  } else {
    lines(seq(from=x.rng[1],to=x.rng[2],length.out=1000),dnorm(seq(from=x.rng[1],to=x.rng[2],length.out=1000),stan.data.priors[[paste0("PriorMean_",target_variable)]],stan.data.priors[[paste0("PriorSD_",target_variable )]]),col="red",lwd=2)
  }
}
dev.off()

# plot model fits
n.subsample = 72
par.chains = as.data.table(fit.df)
sub.chains = par.chains[sample(.N,n.subsample)]
tmp.dt$ljfl_cm = tmp.dt$ofl_cm * 1.1111

png("vb_growth_2sex_fixedEffects-mu.png", width = 9, height = 9, units = "in",  res = 300)
plot(tmp.dt[,.(otolith_age,ljfl_cm)],type="n",ylim=c(0,300),xlab="Age (years)",ylab="LJFL (cm)",cex=1.5,cex.axis=1.5,cex.lab=1.5,las=1)
points(tmp.dt[,.(otolith_age,ljfl_cm)],pch=21,cex=1.25,bg="gray70")
a.vec = seq(from=-5,to=25,length.out=100)
for(i in 1:nrow(sub.chains))
{
  # lines(a.vec,rowMeans(as.data.frame(sub.chains[,c("Linf[1]","Linf[2]")]))[i]*(1-exp(-rowMeans(as.data.frame(sub.chains[,c("k[1]","k[2]")]))[i]*(a.vec-rowMeans(as.data.frame(sub.chains[,c("t0[1]","t0[2]")]))[i]))),col=scales::alpha("gray20",0.15))
  lines(0:25,rowMeans(as.data.frame(sub.chains[,c("L1[1]","L1[2]")]))[i]+(rowMeans(as.data.frame(sub.chains[,c("L2[1]","L2[2]")]))[i]-rowMeans(as.data.frame(sub.chains[,c("L1[1]","L1[2]")]))[i])*((1-exp(-rowMeans(as.data.frame(sub.chains[,c("k[1]","k[2]")]))[i]*(0:25-1)))/(1-exp(-rowMeans(as.data.frame(sub.chains[,c("k[1]","k[2]")]))[i]*(20-1)))),col=scales::alpha("gray20",0.15))

}
lines(1:20,109.319143026271+(241.178357853513-109.319143026271)*((1-exp(-0.190777127959981*(1:20-1)))/(1-exp(-0.190777127959981*(20-1)))),col="green",lwd=3)
dev.off()

png("vb_growth_2sex_fixedEffects-specific.png", width = 9, height = 9, units = "in",  res = 300)
plot(tmp.dt[,.(otolith_age,ljfl_cm)],type="n",ylim=c(0,300),xlab="Age (years)",ylab="LJFL (cm)",cex=1.5,cex.axis=1.5,cex.lab=1.5,las=1)
points(tmp.dt[,.(otolith_age,ljfl_cm)],pch=16,cex=1.25,col=c("blue","orange")[as.numeric(factor(tmp.dt$sex,levels=c("M","F")))])
a.vec = seq(from=-5,to=25,length.out=100)
for(i in 1:nrow(sub.chains))
{
  lines(a.vec,as.vector(as.matrix(sub.chains[,'Linf[1]']))[i]*(1-exp(-as.vector(as.matrix(sub.chains[,'k[1]']))[i]*(a.vec-as.vector(as.matrix(sub.chains[,'t0[1]']))[i]))),col=scales::alpha("orange",0.15))
  lines(a.vec,as.vector(as.matrix(sub.chains[,'Linf[2]']))[i]*(1-exp(-as.vector(as.matrix(sub.chains[,'k[2]']))[i]*(a.vec-as.vector(as.matrix(sub.chains[,'t0[1]']))[i]))),col=scales::alpha("blue",0.15))
}
lines(1:20,109.319143026271+(241.178357853513-109.319143026271)*((1-exp(-0.190777127959981*(1:20-1)))/(1-exp(-0.190777127959981*(20-1)))),col="green",lwd=3)
dev.off()

save(par.chains, file="vb_growth_2sex_fixedEffects.RData")

#________________________________________________________________________________________________________________________________________________________________________________________________________
# prep stan.data, as named list
Nobs = as.integer(nrow(tmp.dt))
Nsex = as.integer(length(unique(tmp.dt$sex)))
lengths = tmp.dt$ofl_cm * 1.1111 # Add conversion from EOFL to LJFL since this is what the stock assessment uses as the standard length!!!
sex = as.integer(tmp.dt$sex)
ages = tmp.dt$otolith_age

stan.data = list(Nsex = Nsex,
                 Nobs = Nobs,
                 lengths = lengths,
                 sex = sex,
                 ages = ages,
                 L1_age = as.integer(c(1,1)),
                 L2_age = as.integer(c(20,20)))
stan.data.priors = list(PriorMean_t0 = c(0,0),
                        PriorSD_t0 = c(0.25,0.25),
                        PriorMean_logk = c(log(0.16),log(0.24)),
                        PriorSD_logk = c(0.5,0.5),
                        PriorMean_logLinf = c(log(276),log(212)),
                        PriorSD_logLinf = c(0.5,0.5),
                        PriorMean_logSigma = c(log(26),log(26)),
                        PriorSD_logSigma = c(0.5,0.5))
stan.data = c(stan.data,stan.data.priors)

#________________________________________________________________________________________________________________________________________________________________________________________________________
# prep stan.inits, as named list
inits.func = function(Nsex=2)
{
  list(t0 = runif(Nsex,-3,1),
       logk = runif(Nsex,log(0.1),log(0.3)),
       logLinf = runif(Nsex,log(180), log(320)),
       logSigma = runif(Nsex,log(5),log(50)))
}

#________________________________________________________________________________________________________________________________________________________________________________________________________
# run stan
seed  = 123
set.seed(seed)
chains = 8
iter = 5000
burnin.prop = 0.2
options(mc.cores = 3)
file.name = "vb_growth_2sex_fixedEffects.stan" # path to stan file
stan.inits = replicate(chains,inits.func(),simplify=FALSE)
fit = stan(file = file.name,
           data = stan.data,
           init = stan.inits,
           chains = chains,
           warmup = iter*burnin.prop,
           iter = iter,
           thin = 1,
           seed = seed,
           control = list(adapt_delta = 0.80,max_treedepth=10))

#________________________________________________________________________________________________________________________________________________________________________________________________________
# STAN diagnostics
print(fit, digits = 3)
check_hmc_diagnostics(fit)
# extract chains
fit.df = as.data.frame(as.matrix(fit))
fit.array = as.array(fit)
# use bayesplot
fit.lp = bayesplot::log_posterior(fit)
fit.np = bayesplot::nuts_params(fit)
bayesplot::color_scheme_set("darkgray")
# bayesplot::mcmc_parcoord(fit.chains[,1:10], np = fit.np)
bayesplot::mcmc_pairs(fit.array, pars=c("L1[1]","L2[1]","k[1]","L1[2]","L2[2]","k[2]"), np = fit.np,off_diag_args = list(size = 0.75))
bayesplot::color_scheme_set("mix-brightblue-gray")
bayesplot::mcmc_trace(fit.array, pars = c("L1[1]","L2[1]","k[1]","L1[2]","L2[2]","k[2]"), np = fit.np)

#________________________________________________________________________________________________________________________________________________________________________________________________________
# make plots
# plot priors with posteriors
png("vb_growth_2sex_fixedEffects_t0prior.png", width = 16, height = 9, units = "in",  res = 300)
par(mfrow=c(2,4))
for(target_variable in c("t0[1]","t0[2]","logk[1]","logk[2]","logLinf[1]","logLinf[2]","logSigma[1]","logSigma[2]"))
{
  x.pad = 0.25*diff(range(fit.df[,target_variable]))
  x.rng = c(min(c(min(fit.df[,target_variable])-x.pad)),max(fit.df[,target_variable])+x.pad)
  hist(fit.df[,target_variable],freq=FALSE,xlab=target_variable,main="",xlim=x.rng)
  if(grepl( "[", target_variable, fixed = TRUE) & grepl( "]", target_variable, fixed = TRUE))
  {
    target_variable.stem = strsplit(target_variable,"[[]")[[1]][1]
    target_variable.idx = as.numeric(gsub("]","",strsplit(target_variable,"[[]")[[1]][2]))
    lines(seq(from=x.rng[1],to=x.rng[2],length.out=1000),dnorm(seq(from=x.rng[1],to=x.rng[2],length.out=1000),stan.data.priors[[paste0("PriorMean_",target_variable.stem )]][target_variable.idx],stan.data.priors[[paste0("PriorSD_",target_variable.stem )]][target_variable.idx]),col="red",lwd=2)

  } else {
    lines(seq(from=x.rng[1],to=x.rng[2],length.out=1000),dnorm(seq(from=x.rng[1],to=x.rng[2],length.out=1000),stan.data.priors[[paste0("PriorMean_",target_variable)]],stan.data.priors[[paste0("PriorSD_",target_variable )]]),col="red",lwd=2)
  }
}
dev.off()

# plot model fits
n.subsample = 72
par.chains = as.data.table(fit.df)
sub.chains = par.chains[sample(.N,n.subsample)]
tmp.dt$ljfl_cm = tmp.dt$ofl_cm * 1.1111

png("vb_growth_2sex_fixedEffects_t0prior-mu.png", width = 9, height = 9, units = "in",  res = 300)
plot(tmp.dt[,.(otolith_age,ljfl_cm)],type="n",ylim=c(0,300),xlab="Age (years)",ylab="LJFL (cm)",cex=1.5,cex.axis=1.5,cex.lab=1.5,las=1)
points(tmp.dt[,.(otolith_age,ljfl_cm)],pch=21,cex=1.25,bg="gray70")
a.vec = seq(from=-5,to=25,length.out=100)
for(i in 1:nrow(sub.chains))
{
  # lines(a.vec,rowMeans(as.data.frame(sub.chains[,c("Linf[1]","Linf[2]")]))[i]*(1-exp(-rowMeans(as.data.frame(sub.chains[,c("k[1]","k[2]")]))[i]*(a.vec-rowMeans(as.data.frame(sub.chains[,c("t0[1]","t0[2]")]))[i]))),col=scales::alpha("gray20",0.15))
  lines(0:25,rowMeans(as.data.frame(sub.chains[,c("L1[1]","L1[2]")]))[i]+(rowMeans(as.data.frame(sub.chains[,c("L2[1]","L2[2]")]))[i]-rowMeans(as.data.frame(sub.chains[,c("L1[1]","L1[2]")]))[i])*((1-exp(-rowMeans(as.data.frame(sub.chains[,c("k[1]","k[2]")]))[i]*(0:25-1)))/(1-exp(-rowMeans(as.data.frame(sub.chains[,c("k[1]","k[2]")]))[i]*(20-1)))),col=scales::alpha("gray20",0.15))

}
lines(1:20,109.319143026271+(241.178357853513-109.319143026271)*((1-exp(-0.190777127959981*(1:20-1)))/(1-exp(-0.190777127959981*(20-1)))),col="green",lwd=3)
dev.off()

png("vb_growth_2sex_fixedEffects_t0prior-specific.png", width = 9, height = 9, units = "in",  res = 300)
plot(tmp.dt[,.(otolith_age,ljfl_cm)],type="n",ylim=c(0,300),xlab="Age (years)",ylab="LJFL (cm)",cex=1.5,cex.axis=1.5,cex.lab=1.5,las=1)
points(tmp.dt[,.(otolith_age,ljfl_cm)],pch=16,cex=1.25,col=c("blue","orange")[as.numeric(factor(tmp.dt$sex,levels=c("M","F")))])
a.vec = seq(from=-5,to=25,length.out=100)
for(i in 1:nrow(sub.chains))
{
  lines(a.vec,as.vector(as.matrix(sub.chains[,'Linf[1]']))[i]*(1-exp(-as.vector(as.matrix(sub.chains[,'k[1]']))[i]*(a.vec-as.vector(as.matrix(sub.chains[,'t0[1]']))[i]))),col=scales::alpha("orange",0.15))
  lines(a.vec,as.vector(as.matrix(sub.chains[,'Linf[2]']))[i]*(1-exp(-as.vector(as.matrix(sub.chains[,'k[2]']))[i]*(a.vec-as.vector(as.matrix(sub.chains[,'t0[1]']))[i]))),col=scales::alpha("blue",0.15))
}
lines(1:20,109.319143026271+(241.178357853513-109.319143026271)*((1-exp(-0.190777127959981*(1:20-1)))/(1-exp(-0.190777127959981*(20-1)))),col="green",lwd=3)
dev.off()

save(par.chains, file="vb_growth_2sex_fixedEffects_t0prior.RData")
