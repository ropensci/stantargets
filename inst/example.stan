data {
  int <lower = 1> n;
  vector[n] x;
  vector[n] y;
}
parameters {
  real beta;
}
model {
  y ~ normal(x * beta, 1);
  beta ~ normal(0, 1);
}
generated quantities {
  array[n] real y_rep = normal_rng(x * beta, 1);
}
