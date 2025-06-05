// modified from https://github.com/colemonnahan/gradmcmc/blob/v1.0/models/growth_nc/growth_nc.stan
data {
  int<lower=0> Nsex; // number of groups
  int<lower=0> Nobs;   // number of observations
  vector[Nobs] lengths;  // observed length
  int sex[Nobs];      // vector to index sex
  vector[Nobs] ages;      // observed ages
  int L1_age[Nsex];    // age for MFCL L1 calc
  int L2_age[Nsex];    // age for MFCL L2 calc
  // define values for prior distributions
  vector[Nsex] PriorMean_t0;
  vector<lower=0>[Nsex] PriorSD_t0;
  vector[Nsex] PriorMean_logk;
  vector<lower=0>[Nsex] PriorSD_logk;
  vector[Nsex] PriorMean_logLinf;
  vector<lower=0>[Nsex] PriorSD_logLinf;
  vector[Nsex] PriorMean_logSigma;
  vector<lower=0>[Nsex] PriorSD_logSigma;
}
parameters {
  // fixed effects
  vector[Nsex] t0;
  vector[Nsex] logk;
  vector[Nsex] logLinf;
  vector[Nsex] logSigma;
}
transformed parameters {
  vector<lower=0>[Nsex] k;
  vector<lower=0>[Nsex] Linf;
  vector<lower=0>[Nsex] Sigma;
  
  k = exp(logk);
  Linf = exp(logLinf);
  Sigma = exp(logSigma);
}
model {
  vector[Nobs] ypred;

  // priors
  t0~normal(PriorMean_t0,PriorSD_t0);
  logk~normal(PriorMean_logk,PriorSD_logk);
  logLinf~normal(PriorMean_logLinf,PriorSD_logLinf);
  logSigma~normal(PriorMean_logSigma,PriorSD_logSigma);

  // calculate likelihood of data
  for(i in 1:Nobs){
    ypred[i] = Linf[sex[i]]*(1-exp(-k[sex[i]]*(ages[i]-t0[sex[i]])));
    target += normal_lpdf(lengths[i] | ypred[i],Sigma[sex[i]]);
  }
}
generated quantities {
  vector[Nsex] L1;
  vector[Nsex] L2;

  // calculate MFCL L1 & L2
  for(i in 1:Nsex){
    L1[i] = Linf[i]*(1-exp(-k[i]*(L1_age[i]-t0[i])));
    L2[i] = Linf[i]*(1-exp(-k[i]*(L2_age[i]-t0[i])));
  }
}