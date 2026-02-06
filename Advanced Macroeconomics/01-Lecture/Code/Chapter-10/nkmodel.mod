// Monopolistic Competition Model with Staggered Price Setting
// (Basic New Keynesian Model)
// Oliver Holtemoeller 27.12.2025
// Temporary Productivity and Monetary Policy Shocks
// Stochastic Solution
// Tested: Octave 6.4 + Dynare 5.2

var y i a nu r pi rast;
// y is output gap (not actual output)
varexo eps_a eps_i eps_pi;
// eps_pi is a cost-push shock (energy prices, e.g.)
parameters alpha, rho, theta, varphi, eta_a, phi_pi, phi_y, eta_i, epsilon, mu, piast, beta, kappa, psi;
alpha = 0.3;                // production function
rho = 0.01;                 // time preference rate
varphi = 1;                 // inverse Frisch elasticity
theta = 1;                  // inverse elasticity of intertemporal substitution
eta_a = 0.9;                // productivity shock persistence
phi_pi = 1.5;               // interest rate rule parameter
phi_y = 0.125;              // interest rate rule parameter
gamma = 0.667;              // Calvo parameter
eta_i = 0.5;                // monetary policy shock persistence
epsilon = 6;                // elasticity of substitution
mu = epsilon/(epsilon-1);   // mark-up
piast = 0;                  // inflation target
beta = 1/(1+rho+piast);
kappa = (1-gamma)*(1-gamma*beta)/gamma*(theta*(1-alpha)+varphi+alpha)/(1-alpha+epsilon);
psi = 1+(1-alpha)*(1-theta)/(theta*(1-alpha)+varphi+alpha);

model(linear);
// (1) Productivity
a = eta_a*a(-1) + eps_a;
// (2) Monetary policy shock
nu = eta_i*nu(-1) + eps_i;
// (3) New Keynesian IS Curve
y = y(+1)-(1/theta)*(i-piast-pi(+1)-rast);
// (4) New Keynesian Phillips Curve
pi = beta*pi(+1) + kappa*y + eps_pi;
// (5) Fischer equation
r = i - piast - pi(+1);
// (6) Monetary policy rule
i = rho + piast + phi_pi*pi + phi_y*y + nu;
// (7) Natural rate of interest
rast = rho + theta*psi*(a(+1)-a);
end;

steady;

// check;

// Specify temporary shock
shocks;
var eps_a; stderr 0.0075;
var eps_i; stderr 0.003;
var eps_pi; stderr 0.003;
end;

stoch_simul(periods=0, drop=0, order=1, irf=20, nofunctions, nocorr, nodecomposition);

