data {
  int N;                        //no trials
  int prob[N];                   //binary 0,1
  int<lower=1> P;               //no fixefs
  int<lower=0> J;               //no subjects
  int<lower=1> n_u;             //no subj ranefs (equals P)
  int<lower=0> K;               //no items
  int<lower=1> n_w;             //no item ranefs
  int<lower=1,upper=J> subj[N]; //subject indicator
  int<lower=1,upper=K> item[N]; //item indicator
  row_vector[P] X[N];           //fixef design matrix
  row_vector[n_u] Z_u[N];       //subj ranef design matrix
  row_vector[n_w] Z_w[N];       //item ranef design matrix
}

parameters {
  vector[P] beta;                 //fixef coefs
  vector<lower=0>[n_u] sigma_u;   //subj ranef sd 
  vector<lower=0>[n_w] sigma_w;   //item ranef sd
  cholesky_factor_corr[n_u] L_u;  //cholesky factor of subj ranef corr matrix
  cholesky_factor_corr[n_w] L_w;  //cholesky factor of item ranef corr matrix
  vector[n_u] z_u[J];             //spherical subj ranef
  vector[n_w] z_w[K];             //spherical item ranef
}

transformed parameters {
  vector[n_u] u[J];             //subj ranefs
  vector[n_w] w[K];             //item ranefs
  {
    matrix[n_u,n_u] Sigma_u;    //subj ranef cov matrix
    matrix[n_w,n_w] Sigma_w;    //item ranef cov matrix
    Sigma_u = diag_pre_multiply(sigma_u,L_u);
    Sigma_w = diag_pre_multiply(sigma_w,L_w);
    for(j in 1:J)
      u[j] = Sigma_u * z_u[j];
    for(k in 1:K)
      w[k] = Sigma_w * z_w[k];
  }
}

model {
  //priors
  beta ~ normal(0,2);    
  sigma_u ~ normal(0,1);  
  sigma_w ~ normal(0,1);  
  L_u ~ lkj_corr_cholesky(2.0);
  L_w ~ lkj_corr_cholesky(2.0);
  for (j in 1:J)
    z_u[j] ~ normal(0,1);
  for (k in 1:K)
    z_w[k] ~ normal(0,1);
  //likelihood
  for (i in 1:N)
    prob[i] ~ bernoulli_logit(X[i] * beta + Z_u[i] * u[subj[i]] + Z_w[i] * w[item[i]]);
}

generated quantities {
  real Dep;
  real AG;
  real AU;
  real RG;
  real RU;
  Dep = ((exp(beta[1] + beta[2]*.5))/(1+exp(beta[1] + beta[2]*.5)))-
  ((exp(beta[1] - beta[2]*.5))/(1+exp(beta[1] - beta[2]*.5)));
  AG = ((exp(beta[1] + beta[3]*.5))/(1+exp(beta[1] + beta[3]*.5)))-
  ((exp(beta[1] - beta[3]*.5))/(1+exp(beta[1] - beta[3]*.5)));
  AU = ((exp(beta[1] + beta[4]*.5))/(1+exp(beta[1] + beta[4]*.5)))-
  ((exp(beta[1] - beta[4]*.5))/(1+exp(beta[1] - beta[4]*.5)));
  RG = ((exp(beta[1] + beta[5]*.5))/(1+exp(beta[1] + beta[5]*.5)))-
  ((exp(beta[1] - beta[5]*.5))/(1+exp(beta[1] - beta[5]*.5)));
  RU = ((exp(beta[1] + beta[6]*.5))/(1+exp(beta[1] + beta[6]*.5)))-
  ((exp(beta[1] - beta[6]*.5))/(1+exp(beta[1] - beta[6]*.5)));
}
