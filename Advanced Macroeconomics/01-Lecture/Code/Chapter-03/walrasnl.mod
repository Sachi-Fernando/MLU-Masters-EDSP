// Non-Linear Static Equilibrium Model
// Oliver Holtemoeller (v1) 25.10.2025
// Tested: Dynare 5.2, 6.4

// 1. Block: Definition of Variables and Parameters
// Endogenous variables:
var w n y c;
// Exogenous variables:
varexo a;

// Parameters
parameters alpha, sigma, varphi;
alpha = 0.3;
sigma = 1;
varphi = 1;

// 2. Model Block
model;
// Labor supply
w = c^sigma * n^varphi;
// Labor demand
w = a * (1-alpha) * n^(-alpha);
// Goods market equilibrium
c = y;
// Production function:
y = a * n^(1-alpha);
end;

// Initialization Block
initval;
a = 1;
w = 0.5;
n = 0.5;
y = 0.5;
c = 0.5;
end;

// Compute Static Solution (Steady State):
steady;

