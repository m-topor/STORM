data {
  int<lower=0> N; // number of observations
  int<lower=1> L; // number of universities
  int<lower=0,upper=8> y[N]; // response variable
  int<lower=1,upper=L> ll[N]; // university number
  int<lower=0,upper=1> D[N]; // dummy variable for "year 2"
  int<lower=0,upper=1> D1[N]; // dummy variable for "2021"
  int<lower=0,upper=1> D2[N]; // dummy variable for "2022"
    
}

parameters {
  real mu;
  real<lower=0> sigma;
  vector[L] alpha;
  real beta1;
  real beta2;
  real beta3;
}
model {
  mu ~ normal(0, 100);
  sigma ~ exponential(1);
  for (l in 1:L) {
      alpha[l] ~ normal(mu, sigma);
  }
  for (n in 1:N) {
    y[n] ~ poisson(exp(alpha[ll[n]] + beta1*D[n] + beta2*D1[n] + beta3*D2[n])) T[ , 8];
  }
}
