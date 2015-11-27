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

value_binomial_option = function(tree, sigma, N, T, r, X, type, div=0) {
  delta_t = T / N
  q = q_prob(r, delta_t, sigma, div=div)
  
  value_ne_tree = matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  value_e_tree = matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  option_value_tree = matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  
  ## ONLY CALL OPTIONS NOW
  
  # set not exercise value to 0
  value_ne_tree[nrow(tree),] = 0
  # set exercise value to S - X
  value_e_tree[nrow(tree),] = tree[nrow(tree), ] - X
  # set option value to whichever is higher
  option_value_tree[nrow(tree),] = pmax(value_ne_tree[nrow(tree),], value_e_tree[nrow(tree),])
  
  
  for (i in (nrow(tree)-1):1) {
    for(j in 1:i) {
      # not exercise value is probability weighted discounted
      value_ne_tree[i, j] = ((1-q)*option_value_tree[i+1,j] + q*option_value_tree[i+1,j+1])/exp(r*delta_t)
      # exercise value = S - X
      value_e_tree[i, j] = tree[i, j] - X
      # set option value to whichever is higher
      option_value_tree[i, j] = max(value_ne_tree[i, j], value_e_tree[i, j])
    }
  }
  
  delta = (option_value_tree[2, 2] - option_value_tree[2, 1])/(tree[2, 2] - tree[2, 1])
    
  return(list(tree=tree, value_ne_tree=value_ne_tree, value_e_tree=value_e_tree, option_value_tree=option_value_tree, price=option_value_tree[1,1], q=q, delta=delta))
}

sigma=0.1
S=50
T=1
N=100
X=51
r=0.08
div=0.1

stock_tree = build_stock_tree(S=S, sigma=sigma, T=T, N=N)
option_tree = value_binomial_option(stock_tree, sigma=sigma, N=N, T=T, r=r, X=X, type="call", div=div)
option_tree