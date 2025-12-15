// Ramsey Growth Model
// Oliver Holtemoeller 02.12.2025
// Temporary Productivity Shock
// Deterministic Extended Path Solution

// 1. Block: Definition of Variables and Parameters
// Endogenous variables:
var k c y i;
// Exogenous variables:
varexo Z;
// Parameters
parameters alpha, rho, delta, theta, a, n;
alpha = 0.3;
rho = 0.01;
theta = 1;
delta = 0.02;
a = 0.01;
n = 0;

// 2. Model Block
model;
// Consumption Euler Equation:
(c(+1)/c) = (((1+alpha*Z*k^(alpha-1)-delta)/(1+rho))^(1/theta))/(1+a);
// Modified Capital Accumulation Equation:
k = (Z*k(-1)^alpha + (1-delta)*k(-1) - c)/(1+n)/(1+a);
// Production Function:
y = Z*k(-1)^alpha;
// Definition of Investment:
i = y - c;
end;

// Initialization Block
initval;
k = 10;
c = 2;
y = 5;
i = 1;
Z = 1;
end;

// Compute Steady State:
steady;

// check;

// Specify temporary productivity shock
shocks;
var Z;
periods 1:1;
values 1.1;
end;

perfect_foresight_setup(periods=300);
perfect_foresight_solver;
send_endogenous_variables_to_workspace;