data {
  int<lower=0> N; // number of observations
  int<lower=1> L; // number of universities
  real<lower=-2.001,upper=2.001> y[N]; // response variable
  int<lower=1,upper=L> ll[N]; // university number
  int<lower=0,upper=1> D[N]; // dummy variable for "year 2"
  int<lower=0,upper=1> D1[N]; // dummy variable for "2021"
  int<lower=0,upper=1> D2[N]; // dummy variable for "2022"
}
parameters {
  real mu;
  real<lower=0> sigma;
  real<lower=0> sigma_a;
  vector[L] alpha;
  real beta1;
  real beta2;
  real beta3;
}
model {
  mu ~ normal(0, 100);
  sigma ~ exponential(1);
  sigma_a ~ exponential(1);
  for (l in 1:L){
      alpha[l] ~ normal(mu, sigma_a);
  }
  for (n in 1:N){
    y[n] ~ normal(alpha[ll[n]] + beta1*D[n] + beta2*D1[n] + beta3*D2[n], sigma) T[-2.001 , 2.001];
  }
}
