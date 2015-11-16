q_prob = function(r, delta_t, sigma) {
  u = exp(sigma*sqrt(delta_t))
  d = exp(-sigma*sqrt(delta_t))
  
  return((exp(r*delta_t) - d)/(u-d))
}

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

value_binomial_option = function(tree, sigma, delta_t, r, X, type) {
  q = q_prob(r, delta_t, sigma)
  
  N = nrow(tree) - 1
  
  option_tree = matrix(0, nrow=1, ncol=2^N)
  probabilities = matrix(0, nrow=1, ncol=2^N)
  
  u = exp(sigma*sqrt(delta_t))
  d = exp(-sigma*sqrt(delta_t))
  
  grid = expand.grid(rep(list(c(u, d)), N))
  qs = expand.grid(rep(list(c(q, 1-q)), N))
        
  for(i in 1 : 2^N) {
    S = tree[1,1]
    sum = S
    q_prob = 1
    for(j in 1 : N) {
      S = S * grid[i, j]
      q_prob = q_prob * qs[i, j]
      sum = sum + S
    }
    option_tree[1, i] = max(sum / (N+1) - tree[1,1], 0)
    probabilities[1, i] = q_prob
  }
    
  return(list(option_tree=option_tree, probabilities=probabilities, updown=grid, qs=qs))
}

binomial_option = function(type, sigma, T, r, X, S, N) {
  q = q_prob(r=r, delta_t=T/N, sigma=sigma)
  tree = build_stock_tree(S=S, sigma=sigma, delta_t=T/N, N=N)
  option = value_binomial_option(tree, sigma=sigma, delta_t=T/N, r=r, X=X, type=type)
  payoffs=option$option_tree[1,]
  probabilities=option$probabilities[1,]
  discount=exp(-r*T)
  return(list(q=q, stock=tree, option=option, payoffs=payoffs, probabilities=probabilities, updown=option$updown, qs=option$qs, discount=discount, price=sum(payoffs*probabilities)*discount))
}

delta = function(binomial_option, row, col) {
  stock_tree = binomial_option$stock
  option_tree = binomial_option$option
  return((option_tree[row+1, col+1] - option_tree[row+1, col])/(stock_tree[row+1, col+1] - stock_tree[row+1, col]))
}

T = 1/4
N = 3
delta_t = T / N
sigma = log(1.2)/sqrt(delta_t)

option_call = binomial_option(type='call', sigma=sigma, T=T, r=0.06, X=100, S=100, N=N)
