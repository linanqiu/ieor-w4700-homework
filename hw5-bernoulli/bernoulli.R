rho = -0.5
steps = 10000
x = vector(mode = 'numeric', length = steps)
z = vector(mode = 'numeric', length = steps)
dx = vector(mode = 'numeric', length = steps)
dz = vector(mode = 'numeric', length = steps)
random = runif(steps, 0, 1)

for(i in 2:steps) {
  if(random[i] < (1-rho) / 4) {
    x[i] = x[i-1] - 1;
    z[i] = z[i-1] + 1;
    dx[i] = -1;
    dz[i] = 1;
  } else if (random[i] < 0.5) {
    x[i] = x[i-1] + 1;
    z[i] = z[i-1] + 1;
    dx[i] = 1;
    dz[i] = 1;
  } else if (random[i] < 0.5 + (1-rho) / 4) {
    x[i] = x[i-1] + 1;
    z[i] = z[i-1] - 1;
    dx[i] = 1;
    dz[i] = -1;
  } else {
    x[i] = x[i-1] - 1;
    z[i] = z[i-1] - 1;
    dx[i] = -1;
    dz[i] = -1;
  }
}

library(ggplot2)
library(reshape)
data = cbind(x, z)
colnames(data) = c('x', 'z')
melted = melt(data, id=c('x', 'z'))
colnames(melted) = c('step', 'process', 'value')
plot = ggplot(data = melted) + geom_line(aes(x=step, y=value, alpha=factor(process)))
plot

cor(dx, dz)
