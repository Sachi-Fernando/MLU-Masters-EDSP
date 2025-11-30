% ===============================================
% Ramsey Static Equilibrium
% This version (v1): 26.11.2025
% Author: Oliver Holtemoeler
% Tested: MATLAB 2024a, 2023b
% ===============================================

clear all;
close all;

disp(' ');
disp('******************************************');
disp('*** Ramsey Static Equilibrium          ***');
disp('******************************************');
disp(' ');

% Parameters
params.alpha = 0.3;      % Capital elasticity of production
params.rho   = 0.01;     % Time preference rate
params.delta = 0.02;     % Depreciation rate

% Exogenous variables
exog.Z = 1; 

% Specify initial variables
initvals = [ 10; 2 ];

% Solve for static equilibrium
options = optimset('MaxIter', 10000, 'MaxFunEvals', 90000, 'TolX', 0.000001, 'TolFun', 0.00000001);
[ fsopt, fval, efl ] = fsolve(@(x)ramseysteadystate(x, exog, params), initvals, options);

% Analytical solution
kast = (params.alpha*exog.Z/(params.rho+params.delta))^(1/(1-params.alpha));

% Display results
disp('Analytical solution:');
disp(['k: ', num2str(kast)]);
disp(' ');

disp('Numeric solution:');
disp(['k: ', num2str(fsopt(1))]);
disp(['c: ', num2str(fsopt(2))]);
disp(' ');
disp(['Checksum: ', num2str(sum(fval.^2))]);
disp(['Exit flag: ', num2str(efl)]);
disp('******************************************');