dividend_pv = function(dividends, times, r) {
  discount_factors = exp(-r*times)
  return(sum(dividends*discount_factors))
}

d1 = function(S, K, r, sigma, T) {
  return((log(S/K) + (r+(sigma^2)/2)*T)/(sigma*sqrt(T)))
}

d2 = function(S, K, r, sigma, T) {
  return(d1(S, K, r, sigma, T) - sigma*sqrt(T))
}

bsm_call = function(S, K, r, sigma, T, dividends, times) {
  if(hasArg(dividends) && hasArg(times)) {
    S = S - dividend_pv(dividends=dividends, times=times, r=r)
  }
  d1_val = d1(S, K, r, sigma, T)
  d2_val = d2(S, K, r, sigma, T)
  if(hasArg(dividends) && hasArg(times)) {
    return(list(price=S*pnorm(d1_val) - K*exp(-r*T)*pnorm(d2_val), d1=d1_val, d2=d2_val, dividend_pv=dividend_pv(dividends=dividends, times=times, r=r)))
  } else {
    return(list(price=S*pnorm(d1_val) - K*exp(-r*T)*pnorm(d2_val), d1=d1_val, d2=d2_val))
  }
}

bsm_put = function(S, K, r, sigma, T, dividends, times) {
  if(hasArg(dividends) && hasArg(times)) {
    S = S - dividend_pv(dividends=dividends, times=times, r=r)
  }
  d1_val = d1(S, K, r, sigma, T)
  d2_val = d2(S, K, r, sigma, T)
  if(hasArg(dividends) && hasArg(times)) {
    return(list(price=K*exp(-r*T)*pnorm(-d2_val) - S*pnorm(-d1_val), d1=d1_val, d2=d2_val, dividend_pv=dividend_pv(dividends=dividends, times=times, r=r)))
  } else {
    return(list(price=K*exp(-r*T)*pnorm(-d2_val) - S*pnorm(-d1_val), d1=d1_val, d2=d2_val))
  }
}

bsm_call(S=80, K=70, r=0.05, sigma=0.30, T=8/12, dividends=c(2,2), times=c(1/4, 1/2))