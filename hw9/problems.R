build_stock_tree = function(S, sigma, delta_t, N) {
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

stock = 100
adjusted_stock_p = 100 - 2*exp(-0.06/6)
sigma = 0.3
delta_t = 1/12
build_stock_tree(S=100 - 2*exp(-0.06/6), 0.3, 1/12, 4)
