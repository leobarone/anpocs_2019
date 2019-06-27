data {
  int n;
  int k;
  int<lower = 0, upper = n * k> l;
  int i[l];
  int j[l];
  int y[l];
  int n_fixo;
  int id_fixo[n_fixo];
  vector[n_fixo] posicao_fixo;
  int n_theta;
  int id_theta[n_theta];
  real alfa_mean;
  real<lower = 0.> alfa_sd;
  real beta_mean;
  real<lower = 0.> beta_sd;
  real theta0_mean;
  real<lower = 0.> theta0_sd;
  real tau_scale;
}
parameters {
  vector[K] alfa;          // 'dificuldade' da votacao
  vector[K] beta;          // 'discriminacao' da votacao
  vector[n_theta] theta;   // posicoes desconhecidas
  real<lower = 0.> tau;    // hyperpriori
  real<lower = 0.> zeta;   // hyperpriori
}
transformed parameters {
  vector[l] mu;
  vector[n] x;
  x[id_theta] = x_theta;
  x[id_fixo] = x_fixos;
  for (m in 1:l) {
    mu[m] = alfa[j[m]] + beta[j[m]] * x[i[m]];
  }
}
model {
  alfa ~ normal(alfa_mean, alfa_sd);
  beta ~ normal(beta_mean, beta_sd);
  xi_theta ~ normal(zeta, tau);
  xi_fixos ~ normal(zeta, tau);
  zeta ~ normal(zeta_mean, zeta_sd);
  tau ~ cauchy(0., tau_scale);
  y ~ bernoulli_logit(mu);
}
generated quantities {
  vector[l] log_lik;
  for (m in 1:l) {
    log_lik[m] = bernoulli_logit_lpmf(y[m] | mu[m]);
  }
}
