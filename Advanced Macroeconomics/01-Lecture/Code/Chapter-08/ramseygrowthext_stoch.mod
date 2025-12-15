// Extended Ramsey Growth Model
// Oliver Holtemoeller 09.12.2025
// Temporary Productivity Shock
// Stochastic Solution
// Tested: Dynare 5.2

var k c h y w r a x i;
varexo eps_a;
parameters alpha, rho, delta, b, eta_a, gamma_a, gamma_n;
alpha = 0.3;
rho = 0.02;
b = 2.5;
delta = 0.03;
eta_a = 0.9;
gamma_n = 0;
gamma_a = 0;

model;
// Exogenous labor efficiency units:
a = eta_a*a(-1) + eps_a;
// Consumption Euler equation:
(exp(c(+1))/exp(c)) = (1+r(+1))/(1+rho)/(1+gamma_a);
exp(k) = (exp(y) + (1-delta)*exp(k(-1)) - exp(c))/(1+gamma_n)/(1+gamma_a);
exp(y) = exp(k(-1))^alpha*(exp(a)*exp(h))^(1-alpha);
exp(c) = exp(w)*(1-exp(h))/b;
exp(w) = (1-alpha)*exp(y)/exp(h);
r = alpha*exp(y)/exp(k(-1))-delta;
exp(x) = exp(y)/exp(h);
exp(i) = exp(y)-exp(c);
end;

initval;
a = 0;
k = log(4);
c = 0.5;
h = log(0.3);
y = 0.6;
w = 1.5;
r = 0.05;
x = y/h;
end;

steady;

// check;

// Specify temporary shock
shocks;
var eps_a; stderr 0.0075;
end;

stoch_simul(periods=200, drop=100, order=1, solve_algo=0, irf=20);

