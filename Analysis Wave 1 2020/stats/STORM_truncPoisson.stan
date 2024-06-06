data {
  int<lower=0> N; // number of observations
  int<lower=1> L; // number of universities
  int U; // upper truncation at value U
  int<lower=0,upper=U> y[N]; // response variable
  int<lower=1,upper=L> ll[N]; // university number
  int<lower=0,upper=1> D[N]; // dummy variable for cluster "yes"
  
}
parameters {
  real mu;
  real<lower=0> sigma;
  vector[L] alpha;
  real beta;
}
model {
  mu ~ normal(0, 100);
  sigma ~ exponential(1);
  for (l in 1:L) {
      alpha[l] ~ normal(mu, sigma);
  }
  for (n in 1:N) {
    y[n] ~ poisson(exp(alpha[ll[n]] + beta*D[n])) T[ , U];
  }
}
