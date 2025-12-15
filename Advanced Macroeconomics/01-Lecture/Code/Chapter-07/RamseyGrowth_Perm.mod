// Ramsey Growth Model
// Oliver Holtemoeller 02.12.2025
// Permanent Productivity Shock
// Deterministic Extended Path Solution

var k c y i;
varexo Z;
parameters alpha, rho, delta, theta, a, n;
alpha = 0.3;
rho = 0.01;
theta = 1;
delta = 0.02;
a = 0.01;
n = 0;

model;
(c(+1)/c) = (((1+alpha*Z*k^(alpha-1)-delta)/(1+rho))^(1/theta))/(1+a);
k = (Z*k(-1)^alpha + (1-delta)*k(-1) - c)/(1+n)/(1+a);
y = Z*k(-1)^alpha;
i = y - c;
end;

initval;
k = 10;
c = 2;
y = 5;
i = 1;
Z = 1;
end;

steady;

// check;

endval;
Z = 1.1;
end;

steady;

perfect_foresight_setup(periods=300);
perfect_foresight_solver;
send_endogenous_variables_to_workspace;