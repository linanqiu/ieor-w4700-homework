# problem 1

forward_rate = function(T1, T2, t, y1, y2) {
  return ((-y1*(T1 - t) + y2*(T2-t))/(T2-T1))
}

maturities = c(3, 6, 9, 12, 15, 18)
maturities = maturities / 12
spot_rates = c(7, 7.2, 7.3, 7.5, 7.7, 7.9)
spot_rates = spot_rates / 100

quarterly_forward_rate = function(period) {
  T2 = maturities[period]
  T1 = maturities[period - 1]
  
  y2 = spot_rates[period]
  y1 = spot_rates[period - 1]
  
  t = 0
  
  return(forward_rate(T1, T2, t, y1, y2))
}

forward_rates = sapply(c(2:6), quarterly_forward_rate)

forward_rates

# problem 2

maturities = c(3, 6, 9, 12, 15, 18)
maturities = maturities / 12
spot_rates = c(8, 8.2, 8.4, 8.5, 8.6, 8.7)
spot_rates = spot_rates / 100

quarterly_forward_rate = function(period) {
  T2 = maturities[period]
  T1 = maturities[period - 1]
  
  y2 = spot_rates[period]
  y1 = spot_rates[period - 1]
  
  t = 0
  
  return(forward_rate(T1, T2, t, y1, y2))
}

forward_rates = sapply(c(2:6), quarterly_forward_rate)

forward_rates

fra = 2000000 * (exp(-0.085) - (1+0.095*(3/12))*exp(-0.086*1.25))
fra

fixedleg = 0.095/4
floatleg = exp(0.09*0.25) - 1

difference = floatleg - fixedleg
difference_pv = difference * exp(-0.086*5/4)
fra = difference_pv*2000000
fra

# problem 3

time_month = c(0, 6, 12, 18, 24, 30)
time = time_month / 12
time_payment_month = time_month + 6
time_payment = time_payment_month / 12

six_m_libor = c(5, 5.8, 5.3, 5.5, 5.6, 6.1)
six_m_libor = six_m_libor / 100
fixed_rate = 0.1
fixed_rate = c(rep(fixed_rate, 6))

fixed_payments = (fixed_rate / 2) * 10000000
floating_payments = (six_m_libor/ 2) * 10000000
net_payments = fixed_payments - floating_payments

table = data.frame(time_payment_month, fixed_rate , six_m_libor, fixed_payments, floating_payments, net_payments)
colnames(table) = c('Time', 'Fixed Rate', '6 Month Libor Rate', 'Fixed Payments', 'Floating Payments', 'Net Payments')
library(xtable)
xtable(table)

discount_factors = c(0.9778, 0.9541, 0.9291, 0.9048, 0.8781, 0.8479)

value = sum(net_payments * discount_factors)
value

discount_factors = c(0.9778, 0.9541, 0.9291, 0.9048, 0.8781, 0.8479)

gradient_descent = function(k) {
  fixed_rate = c(rep(k, 6))
  fixed_payments = (fixed_rate / 2)
  floating_payments = (six_m_libor/2)
  pv_fixed = fixed_payments * discount_factors
  pv_float = floating_payments * discount_factors
  return(sum(pv_fixed) - sum(pv_float))
}

uniroot(gradient_descent, c(0, 1))
