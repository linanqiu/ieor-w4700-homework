q_prob = function(r, delta_t, sigma, div=0) {
  u = exp(sigma*sqrt(delta_t))
  d = exp(-sigma*sqrt(delta_t))
  
  return((exp((r-div)*delta_t) - d)/(u-d))
}

build_stock_tree = function(S, sigma, T, N) {
  delta_t = T/N
  tree = matrix(0, nrow=N+1, ncol=N+1)
  
  u = exp(sigma*sqrt(delta_t))
  d = exp(-sigma*sqrt(delta_t))
  
  for (i in 1:(N+1)) {
    for (j in 1:i) {
      tree[i,j] = S * u^(j-1) * d^((i-1)-(j-1))
    }
  }
  return(tree)
}

value_binomial_option = function(tree, sigma, N, T, r, alpha=3, div=0) {
  delta_t = T / N
  q = q_prob(r, delta_t, sigma, div=div)
  
  option_tree = matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  
  # option pays S^alpha at expiration
  option_tree[nrow(tree),] = tree[nrow(tree), ]^alpha
  
  for (i in (nrow(tree)-1):1) {
    for(j in 1:i) {
      option_tree[i, j] = ((1-q)*option_tree[i+1,j] + q*option_tree[i+1,j+1])/exp(r*delta_t)
    }
  }
  
  delta = (option_tree[2, 2] - option_tree[2, 1])/(tree[2, 2] - tree[2, 1])
  
  return(list(tree=tree, option_tree=option_tree, price=option_tree[1,1], q=q, delta=delta))
}

sigma=0.3
S=100
T=1/4
N=100
r=0.1

stock_tree = build_stock_tree(S=S, sigma=sigma, T=T, N=N)
option_tree = value_binomial_option(stock_tree, sigma=sigma, N=N, T=T, r=r, alpha=3)


black_scholes_power = function(alpha, r, T, sigma, S) {
  discount = exp(-r*T)
  p0 = S^alpha
  expectation = exp((r*alpha + (alpha * (alpha - 1)) / 2 * sigma^2)*T)
  return(discount*p0*expectation)
}

black_scholes_power(alpha=3, r=0.1, T=0.25, sigma=0.3, S=100)
