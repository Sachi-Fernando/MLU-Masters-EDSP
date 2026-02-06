// Monopolistic Competition Model with Flexible Prices
// Oliver Holtemoeller 16.12.2025
// Temporary Productivity and Monetary Policy Shocks
// Stochastic Solution
// Tested with Dynare 5.2

var c n y w i a nu r pi;
varexo eps_a eps_i;
parameters alpha, rho, theta, varphi, eta_a, phi, eta_i, epsilon, mu, piast;
alpha = 0.3;                // production function
rho = 0.01;                 // time preference rate
varphi = 2;                 // inverse Frisch elasticity
theta = 2;                  // inverse elasticity of intertemporal substitution
eta_a = 0.9;                // productivity shock persistence
phi = 1.5;                  // interest rate rule parameter
eta_i = 0.5;                // monetary policy shock persistence
epsilon = 10;               // elasticity of substitution
mu = epsilon/(epsilon-1);   // mark-up
piast = 0.02;               // inflation target

model;
a = eta_a*a(-1) + eps_a;
nu = eta_i*nu(-1) + eps_i;
exp(y) = exp(c);
exp(y) = exp(a)*(exp(n))^(1-alpha);
(exp(c(+1))/exp(c))^theta = (1+r)/(1+rho);
exp(w) = exp(c)^theta*exp(n)^varphi;
exp(w) = (1-alpha)*exp(y)/exp(n)/mu;
r = (1+i)/(1+pi(+1))-1;
1+i = (1+rho)*(1+piast)*(1+pi-piast)^phi*exp(nu);
end;

initval;
a = 0;
c = 0.5;
n = 0.3;
y = 0.6;
w = 1.5;
r = 0.05;
i = 0.03;
end;

steady;

// check;

// Specify temporary shock
shocks;
var eps_a; stderr 0.0075;
var eps_i; stderr 0.003;
end;

stoch_simul(periods=200, drop=100, order=1, irf=20, nocorr, nofunctions, nodecomposition, nomoments);
