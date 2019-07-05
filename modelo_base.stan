data {
  // Dados do modelo
  int n;
  int k;
  int<lower = 0, upper = n * k> m;
  int id_parlamentar[m];
  int id_votacao[m];
  int voto[m];
  // Parlamentares fixos
  int n_fixos;
  int id_fixos[n_fixos];
  vector[n_fixos] fixos;
  // Parlamentares nao-fixos 
  int n_theta;
  int id_theta[n_theta];
  // Prioris
  real alfa_mean;
  real<lower = 0.> alfa_sd;
  real beta_mean;
  real<lower = 0.> beta_sd;
  real theta0_mean;
  real<lower = 0.> theta0_sd;
  real tau_scale;
}
parameters {
  vector[k] alfa;          // 'dificuldade' da votacao
  vector[k] beta;          // 'discriminacao' da votacao
  vector<lower = 0., upper = 1>[n_theta] theta;   // posicoes desconhecidas
  real<lower = 0.> tau;    // hyperpriori
  real<lower = 0.> theta0;   // hyperpriori
}
transformed parameters {
  vector[m] mu;
  vector[n] x;
  x[id_theta] = theta;
  x[id_fixos] = fixos;
  for (i in 1:m) {
    mu[i] = alfa[id_votacao[i]] + beta[id_votacao[i]] * x[id_parlamentar[i]];
  }
}
model {
  alfa ~ normal(alfa_mean, alfa_sd);
  beta ~ normal(beta_mean, beta_sd);
  theta ~ normal(theta0, tau);
  fixos ~ normal(theta0, tau);
  theta0 ~ normal(theta0_mean, theta0_sd);
  tau ~ cauchy(0., tau_scale);
  voto ~ bernoulli_logit(mu);
}
generated quantities {
  vector[m] log_lik;
  for (i in 1:m) {
    log_lik[i] = bernoulli_logit_lpmf(voto[i] | mu[i]);
  }
}
