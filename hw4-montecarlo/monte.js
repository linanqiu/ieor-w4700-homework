monte = function(steps, delta_t, mu, sigma, s0) {
  s = s0;
  for(var i = 0; i < steps; i++) {
    if(Math.random() > 0.5) {
      s += (mu / delta_t) * s + (sigma / delta_t) * s;
    } else {
      s += (mu / delta_t) * s - (sigma / delta_t) * s;
    }
  }
  return s;
}

var results = [];

for(var i = 0; i < 10000; i++) {
  results.push(monte(100000, 10000, 0.15, 0.2, 10));
}

console.log(results);
