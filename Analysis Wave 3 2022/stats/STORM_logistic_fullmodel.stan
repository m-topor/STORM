data {
  int<lower=0> N; // number of observations
  int<lower=1> L; // number of universities
  int<lower=0,upper=1> y[N]; // response variable
  int<lower=1,upper=L> ll[N]; // university number
  int<lower=0,upper=1> D[N]; // dummy variable for cluster "yes"
  real x[N]; // number of years at uni
  real interact[N]; // interaction between D and x, i.e. D*x
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
    y[n] ~ bernoulli(inv_logit(alpha[ll[n]] + beta1*D[n] + beta2*x[n] + beta3*interact[n]));
  }
}
