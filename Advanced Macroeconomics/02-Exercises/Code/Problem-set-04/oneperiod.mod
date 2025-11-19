% Advanced Macroeconomics - Tutorial - Winter 25/26
% Alexandra Gutsch
% Date: 27/10/2025

% endogenous variables
var 
    y 
    c 
    n 
    w
    g
    tau
;

% exogenous variables
varexo 
    a
;

% parameters
parameters 
    alpha 
    sigma
    gamma
    varphi
    A_h
;
alpha = 0.3;
sigma = 1;
gamma = 0.2;
varphi = 1;
A_h = 1;

model;
[name = 'labor supply condition']
w = (1+tau) * c^sigma * A_h * n^varphi;

[name = 'production function']
y = exp(a) * n^(1-alpha);

[name = 'FOC firm labor demand']
w = (1-alpha) * y / n;

[name = 'public spending']
g = gamma * y;

[name = 'public budget']
g = tau * c;

[name = 'aggregate resources']
y = c + g;
end;

% provide analytical steady state
steady_state_model;
n = 1/3;
y = exp(a) * n^(1-alpha);
w = (1-alpha) * y / n;
g = gamma * y;
c = y - g;
tau = g / c;
A_h = w / (1+tau) * c^(-sigma) * n^(-varphi);
end;

% compute steady state
steady;