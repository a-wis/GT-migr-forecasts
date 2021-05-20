// 
//
// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> NG;
  int<lower=0> NI;
  vector[NI] y;
  vector[NG] x;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real phi0;
  real<lower=0, upper=1> phi1;
  real sig_a;
  // real sig_b;
  vector[NI] l_sigma; 
  
}

transformed parameters{
 vector<lower=0>[NI] sigma;
 
 for (i in 1:NI) sigma[i] = exp(l_sigma[i])/(1+exp(l_sigma[i]));
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  phi0 ~ normal(0,4);
  phi1 ~ normal(0,.4);
  sig_a ~ normal(-0.49,0.02641);
  // sig_a[2] ~ normal(0.61854,0.034);
  l_sigma[1:NI] ~ normal(sig_a*y[1:NI],0.2858);
  y[2:NI] ~ normal(phi0 + phi1*y[1:(NI-1)], sigma[2:NI]);
}

generated quantities{
  vector[NG-1] yf;//vector[NG+2] yf;
  real l_sigmaf[NG-NI];//l_sigmaf[NG-NI+3];
  real sigmaf[NG-NI];//sigmaf[NG-NI+3];
  // real yf1[1];
  
  // for (i in 1:(NG-NI)) {
    // yf1[1] = phi0 + phi1*y[NI];
    l_sigmaf[1] = normal_rng(sig_a*(phi0 + phi1*y[NI]),0.2858);
    sigmaf[1] = exp(l_sigmaf[1])/(1+exp(l_sigmaf[1]));
  // }
  for (i in 1:(NI-1)) yf[i]=normal_rng(phi0 + phi1*y[i],sigma[i+1]); // 
  
  yf[NI]=normal_rng(phi0 + phi1*y[NI],sigmaf[1]); //NI is the first year of forecast
  
  if (NG-NI>1) {
  for (i in (NI+1):(NG-1)){
    l_sigmaf[i-NI+1] = normal_rng(sig_a*(phi0 + phi1*yf[i-1]),0.2858);
    sigmaf[i-NI+1] = exp(l_sigmaf[i-NI+1])/(1+exp(l_sigmaf[i-NI+1]));
    yf[i]=normal_rng(phi0 + phi1*yf[i-1],sigmaf[i-NI+1]);
  }
  }
}
