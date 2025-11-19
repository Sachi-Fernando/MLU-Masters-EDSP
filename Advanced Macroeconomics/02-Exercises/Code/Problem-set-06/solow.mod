% Advanced Macroeconomics - Tutorial - Winter 25/26
% Alexandra Gutsch
% Date: 18/11/2025

% Solow Growth Model with Permanent Shocks

% endogenous variables
var
    y
    k
    i
    c
    w
    r
;
predetermined_variables
    k
;

% exogenous variables
varexo
    z
    s
    n
;

% parameters
parameters
    alpha
    delta
    a
    zbar
    sbar
    nbar
;
alpha = 0.3;
delta = 0.03;
a = 0.01;
zbar = 1;
sbar = 0.2;
nbar = 0.005;

model;
[name = 'production function']
y = z * k^alpha;

[name = 'capital accumulation']
k(+1) = i + (1 - delta - n - a)*k;

[name = 'aggregate resources']
c = y - i;

[name = 'aggregate savings']
s * y = i;

[name = 'real interest rate']
r = z*alpha*k^(alpha-1) - delta;

[name = 'real wage']
w = z*k^alpha - k*z*alpha*k^(alpha-1);
end;

% provide analytical steady state
initval;
z = zbar;
s = sbar;
n = nbar;
k = ((s * z)/(delta + a + n)) ^ (1 /(1-alpha));
i = (delta + a + n)*k;
y = z * k^alpha;
c = y - i;
r = z*alpha*k^(alpha-1) - delta;
w = z*k^alpha - k*z*alpha*k^(alpha-1);
end;

% inital steady state
steady;

% scenarios with permanent shock
switch Scenario
    case 1
        endval;
        z = zbar*1.1;
        end;
    case 2
        endval;
        s = sbar*1.1;
        end;
    case 3
        endval;
        n = nbar*1.1;
        end;
end

% final steady state (with permanent shock)
steady;

% simulation with perfect foresight
perfect_foresight_setup(periods=300);
perfect_foresight_solver;

% save time paths of variables in workspace
send_endogenous_variables_to_workspace;