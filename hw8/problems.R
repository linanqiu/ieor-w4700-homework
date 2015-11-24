# problem 2

d1 = function(S, K, r, sigma, T) {
  return((log(S/K) + (r+(sigma^2)/2)*T)/(sigma*sqrt(T)))
}

d2 = function(S, K, r, sigma, T) {
  return(d1(S, K, r, sigma, T) - sigma*sqrt(T))
}

bsm_put = function(sigma, K, S, r, T) {
  d1_val = d1(S, K, r, sigma, T)
  d2_val = d2(S, K, r, sigma, T)
  return(price=K*exp(-r*T)*pnorm(-d2_val) - S*pnorm(-d1_val))
}

sigma_root_put = function(sigma, S, K, r, T, price) {
  return(bsm_put(sigma=sigma, K=K, S=S, r=r, T=T) - price)
}

implied_volatility_put = function(S, K, r, T, price) {
  return(uniroot(sigma_root_put, c(-10^99, 10^99), S=S, K=K, r=r, T=T, price=price)$root)
}

strike = c(91:110)
putprice = c(9.57,9.78,10.01,10.24,10.49,10.75,11.02,11.31,11.61,11.92,12.25,12.60,12.96,13.344,13.74,14.16,14.60,15.05,15.53,16.02)
volatility = mapply(implied_volatility_put, K=strike, price=putprice, S=100, r=0, T=1)

library(ggplot2)

data = as.data.frame(list(strike=strike, volatility=volatility))
plot = ggplot(data=data) + geom_line(aes(x=strike, y=volatility)) + geom_point(aes(x=strike, y=volatility)) + labs(title="Implied Volatility Smile", x="Strike Price", y="Implied Volatility")
plot

positions = c(-5000, -500, -2000, -500)
deltas = c(0.5, 0.7, -0.4, 0.7)
gammas = c(2.2, 0.6, 1.3, 1.9)
vegas = c(1.8, 0.2, 0.8, 1.4)
to_delta = 0.7
to_gamma = 1.4
to_vega = 0.9
sterling_delta = 1
sterling_gamma = 0
sterling_vega = 0

# delta gamma
# solve a %*% x = b
b = c(-sum(positions*deltas), -sum(positions*gammas))
a = matrix(c(to_delta, to_gamma, sterling_delta, sterling_gamma), nrow=2, ncol=2)
colnames(a) = c('traded_option', 'sterling')
rownames(a) = c('delta', 'gamma')
solve(a, b)

# delta vega
# solve a %*% x = b
b = c(-sum(positions*deltas), -sum(positions*gammas))
a = matrix(c(to_delta, to_vega, sterling_delta, sterling_vega), nrow=2, ncol=2)
colnames(a) = c('traded_option', 'sterling')
rownames(a) = c('delta', 'vega')
solve(a, b)
