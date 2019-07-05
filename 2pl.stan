data {
  int<lower=1> I;					      // numero de parlamentares
  int<lower=1> J;        				// numero de votacoes
  int<lower=1> N;				      	// numero de observacoes
  int<lower=1, upper=I> ii[N];	// id do parlamentar n
  int<lower=1, upper=J> jj[N];	// id da votacao n
  int<lower=0, upper=I * J> y[N];		// voto observado em n
}
parameters {
  vector<lower=0>[J] alpha; 		// parametro de discriminacao da votacao I
  vector[J] beta; 				      // parametro de dificuldade da votacao I
  vector[I] theta;				      // parametro de disciplina do parlamentar J
}
model {
  vector[N] eta;
  alpha ~ lognormal(0.5,1); 		// prior para alpha 
  beta ~ normal(0,10); 			    // prior para beta 
  theta ~ normal(0,1);			    // prior para theta
  for (n in 1:N)
    eta[n] = alpha[jj[n]] * (theta[ii[n]] - beta[jj[n]]); // 2PL Model
    y ~ bernoulli_logit(eta);
}