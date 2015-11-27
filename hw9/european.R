d1 = function(S, K, r, sigma, T) {
  return((log(S/K) + (r+(sigma^2)/2)*T)/(sigma*sqrt(T)))
}

d2 = function(S, K, r, sigma, T) {
  return(d1(S, K, r, sigma, T) - sigma*sqrt(T))
}

bsm_call = function(sigma, K, S, r, T) {
  d1_val = d1(S, K, r, sigma, T)
  d2_val = d2(S, K, r, sigma, T)
  return(list(price=S*pnorm(d1_val) - K*exp(-r*T)*pnorm(d2_val), delta=pnorm(d1_val)))
}

bsm_call(sigma=0.1, T=1, r=0.08, K=51, S=50*exp(-0.1))