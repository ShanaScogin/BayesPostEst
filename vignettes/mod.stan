	
data {
  int<lower=0> N;
  int<lower=0,upper=1> volunteer[N];
  vector[N] female;
  vector[N] neuroticism;
  vector[N] extraversion;
}
parameters {
  vector[4] b;
}
model {
  volunteer ~ bernoulli_logit(b[1] + b[2] * female + b[3] * neuroticism + b[4] * extraversion);
  for(i in 1:4){
    b[i] ~ normal(0, 3); 
  }
}

